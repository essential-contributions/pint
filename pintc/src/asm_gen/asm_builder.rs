use crate::{
    asm_gen::ComputeNode,
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{
        AsmOp, BinaryOp, Expr, ExternalIntrinsic, Immediate, InternalIntrinsic, IntrinsicKind,
        TupleAccess, UnaryOp,
    },
    predicate::{Contract, ExprKey, Predicate},
    span::{empty_span, Spanned},
    types::Type,
};
use essential_asm::short::*;
use essential_types::{
    predicate::{Predicate as CompiledPredicate, Reads},
    ContentAddress,
};

/// This object is a context that keeps track of various helper data structures. This context
/// evolves throughout the assembly generation process.
pub struct AsmBuilder<'a> {
    // A reference to a `HashMap` from predicate names to the compiled predicates and their
    // addresses.
    compiled_predicates: &'a fxhash::FxHashMap<String, (CompiledPredicate, ContentAddress)>,

    // A map from names of variables to their memory indices.
    var_to_mem_idx: fxhash::FxHashMap<(String, Reads), i64>,

    // A map from storage access expressions to their memory indices.
    storage_expr_to_mem_idx: fxhash::FxHashMap<ExprKey, i64>,

    // A map from pre-computed expressions to their memory indices.
    precomputed_expr_to_mem_idx: fxhash::FxHashMap<ExprKey, i64>,

    // Name of the parameter to the current morphism body, along with its location and size.
    morphism_param: Option<(String, Location, usize)>,
}

type Asm = Vec<essential_asm::Op>;

/// "Location" of an expression:
/// 1. `PredicateData` expressions are stored in prediate parameters with the param slot idx and
///    inner idx at the top of the stack.  Values may then be read by pushing a size to the stack
///    and using `PredicateData`.
/// 2. `Memory` expressions are stored in memory with the address at the top of the stack and
///    should be read using `Load` or `LoadRange`.
/// 3. `Storage` expressions are storage keys and need to be read using `KeyRange` or if the bool
///    param is true `KeyRangeExtern`.
/// 4. `Stack` expressions are at the top of the stack.
#[derive(Clone, Copy, Debug)]
enum Location {
    Memory,
    PredicateData,
    Stack,
    Storage(bool),
}

impl<'a> AsmBuilder<'a> {
    /// Creates a new `AsmBuilder` given a set of compiled predicates and their addresses.
    pub fn new(
        compiled_predicates: &'a fxhash::FxHashMap<String, (CompiledPredicate, ContentAddress)>,
    ) -> Self {
        Self {
            compiled_predicates,
            var_to_mem_idx: fxhash::FxHashMap::default(),
            precomputed_expr_to_mem_idx: fxhash::FxHashMap::default(),
            storage_expr_to_mem_idx: fxhash::FxHashMap::default(),
            morphism_param: Default::default(),
        }
    }

    /// Generates assembly for a given a non-leaf node. These nodes need to prepare data for their
    /// children
    pub(super) fn compile_compute_node(
        &mut self,
        handler: &Handler,
        node: &ComputeNode,
        parents: &[ComputeNode],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Asm, ErrorEmitted> {
        let expr = node.expr();
        let expr_ty = expr.get_ty(contract);
        let expr_size = expr_ty.size(handler, contract)? as i64;

        // Produce a map from input variables to this nodes to their indices in the concatinated
        // memory. Also compute the total amount of memory used so far.
        let mut total_memory_size = parents.iter().try_fold(0i64, |base, node| match node {
            ComputeNode::Var { var, reads, .. } => {
                self.var_to_mem_idx.insert((var.name.clone(), *reads), base);
                let size = var.expr.get_ty(contract).size(handler, contract)?;
                Ok(base + size as i64)
            }
            ComputeNode::Constraint { .. } => Ok(base),
            ComputeNode::Expr { expr, .. } => {
                self.precomputed_expr_to_mem_idx.insert(*expr, base);
                let size = expr.get_ty(contract).size(handler, contract)?;
                Ok(base + size as i64)
            }
        })?;

        let mut asm = Vec::new();

        // First, collect all storage accesses used in the node expression, and allocate enough
        // memory for all of them. We do this ahead of time so that we know exactly how the size of
        // memory allocated. Due to short-circuting, this also means that some allocations may not
        // be used, but this is okay for now.
        let alloc_size_for_storage_accesses: i64 = expr
            .collect_storage_accesses(contract)
            .iter()
            .try_fold(0, |total_alloc_size, access| {
            let num_keys = access
                .get_ty(contract)
                .get_optional_ty()
                .ok_or_else(|| {
                    handler.emit_internal_err(
                        "storage accesses must be of type optional",
                        empty_span(),
                    )
                })?
                .storage_keys(handler, contract)? as i64;

            let access_size = (access.get_ty(contract).size(handler, contract)?) as i64;

            let alloc_size = 2 * num_keys // for each key, add the data address then its length
                + access_size; // then lay out the data read from each key, sequentiallly

            // Keep track of the mem index for this access
            self.storage_expr_to_mem_idx
                .insert(*access, total_memory_size);

            total_memory_size += alloc_size;

            // Accumulate the allocation size
            Ok(total_alloc_size + alloc_size)
        })?;

        if expr.get(contract).is_asm_block() {
            self.compile_expr(handler, &mut asm, &expr, contract, pred)?;
        } else {
            if alloc_size_for_storage_accesses > 0 {
                asm.extend([PUSH(alloc_size_for_storage_accesses), ALOC, POP]);
            }

            if node.is_leaf() {
                self.compile_expr(handler, &mut asm, &expr, contract, pred)?;
            } else {
                // Allocate enough memory for the returned data, if needed
                if total_memory_size < expr_size {
                    asm.extend([PUSH(expr_size - total_memory_size), ALOC, POP]);
                }

                // Reserve scratch space at the bottom of the stack. Currently used by morphisms.
                // TODO: check if this expr contains morphisms and omit this if not.
                asm.extend([PUSH(2), RES, POP]);

                // Compile result to the stack.
                self.compile_expr(handler, &mut asm, &expr, contract, pred)?;

                // Stash it to the start of memory.
                asm.extend([PUSH(expr_size), PUSH(0), STOR]);

                // Free the scratch.
                asm.extend([PUSH(2), DROP]);

                // While compiling the expr we may have allocated more buffers in memory.  We need
                // to shrink memory down to just the result.
                // TODO: have compile_expr() return whether this is necessary.
                asm.extend([PUSH(expr_size), FREE]);
            }
        }

        Ok(asm)
    }

    /// Generates assembly for an `ExprKey` and insert it into `asm`. Returns the number of opcodes
    /// used to express `expr`.
    fn compile_expr(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<usize, ErrorEmitted> {
        let old_asm_len = asm.len();
        let expr_ty = expr.get_ty(contract);
        let expr_size = expr_ty.size(handler, contract)? as i64;

        match self.compile_expr_pointer(handler, asm, expr, contract, pred)? {
            Location::PredicateData => {
                asm.extend([PUSH(expr_size), DATA]);
            }

            Location::Memory => {
                if expr_size == 1 {
                    asm.push(LOD);
                } else {
                    asm.extend([PUSH(expr_size), LODR]);
                }
            }

            Location::Storage(is_extern) => {
                let num_keys = expr
                    .get_ty(contract)
                    .get_optional_ty()
                    .ok_or_else(|| {
                        handler.emit_internal_err(
                            "storage accesses must be of type optional",
                            empty_span(),
                        )
                    })?
                    .storage_keys(handler, contract)? as i64;

                let access_mem_idx = self.storage_expr_to_mem_idx[expr];

                // Read the storage keys into memory
                asm.extend([
                    PUSH(num_keys),
                    PUSH(access_mem_idx), // mem idx for the output of KREX or KRNG
                    if is_extern { KREX } else { KRNG },
                ]);

                // Now read the actual data
                asm.extend([
                    PUSH(access_mem_idx + num_keys * 2), // where the actual data lives
                    PUSH(expr_size - 1),                 // size of the data (excluding the tag)
                    LODR,
                ]);

                // Sum the sizes of all the values read. These are laid out as follows in
                // memory:
                // `[a_addr, a_len, b_addr, b_len, a_value, b_value, ...]`
                if num_keys > 1 {
                    asm.push(PUSH(0));
                    asm.extend(
                        (0..num_keys).flat_map(|i| [PUSH(access_mem_idx + 2 * i + 1), LOD, ADD]),
                    );
                } else {
                    asm.extend([PUSH(access_mem_idx + 1), LOD]);
                }

                // This computes the tag on the stack. If the total size read from storage is
                // equal to the size of the value in the optional, then the tag should be 1.
                // Otherwise, it should be 0.
                asm.extend([PUSH(expr_size - 1), EQ]);
            }

            Location::Stack => {}
        }
        Ok(asm.len() - old_asm_len)
    }

    /// Generates assembly for an `ExprKey` as a _pointer_. What this means is that, if the expr
    /// refers to something other than a "value" (like an immedate), we generate assembly for the
    /// pointer (i.e. `Location`) only. A "pointer" may be to predicate data or memory for
    /// example.
    fn compile_expr_pointer(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // For precomputed expressions, just return the address in memory
        if let Some(mem_idx) = self.precomputed_expr_to_mem_idx.get(expr) {
            asm.push(PUSH(*mem_idx));
            return Ok(Location::Memory);
        }

        fn compile_immediate(
            handler: &Handler,
            asm: &mut Asm,
            imm: &Immediate,
        ) -> Result<usize, ErrorEmitted> {
            match imm {
                Immediate::Int(val) => {
                    asm.push(PUSH(*val));
                    Ok(1)
                }
                Immediate::Bool(val) => {
                    asm.push(PUSH(*val as i64));
                    Ok(1)
                }
                Immediate::B256(val) => {
                    asm.extend([
                        PUSH(val[0] as i64),
                        PUSH(val[1] as i64),
                        PUSH(val[2] as i64),
                        PUSH(val[3] as i64),
                    ]);
                    Ok(4)
                }
                Immediate::Array(elements) => {
                    let mut value_size = 0;
                    for element in elements {
                        value_size += compile_immediate(handler, asm, element)?;
                    }
                    Ok(value_size)
                }
                Immediate::Tuple(fields) => {
                    let mut value_size = 0;
                    for (_, field) in fields {
                        value_size += compile_immediate(handler, asm, field)?;
                    }
                    Ok(value_size)
                }
                Immediate::UnionVariant {
                    tag_num,
                    value_size: max_size,
                    value,
                    ..
                } => {
                    asm.push(PUSH(*tag_num));

                    let mut value_size = 0;
                    if let Some(value) = value {
                        value_size = compile_immediate(handler, asm, value)?;
                    }
                    while value_size < *max_size {
                        asm.push(PUSH(0));
                        value_size += 1;
                    }
                    Ok(1 + value_size)
                }
                Immediate::Error | Immediate::Real(_) | Immediate::String(_) => {
                    Err(handler.emit_internal_err("unexpected literal", empty_span()))
                }
            }
        }

        match expr.get(contract) {
            Expr::Immediate { value, .. } => {
                compile_immediate(handler, asm, value)?;
                Ok(Location::Stack)
            }
            Expr::Array { elements, .. } => {
                for element in elements {
                    self.compile_expr(handler, asm, element, contract, pred)?;
                }
                Ok(Location::Stack)
            }
            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    self.compile_expr(handler, asm, field, contract, pred)?;
                }
                Ok(Location::Stack)
            }
            Expr::Path(path, _) => self.compile_path(handler, asm, path, Reads::Pre, pred),
            Expr::AsmBlock { ops, .. } => self.compile_asm_block(handler, asm, ops, pred),
            Expr::UnionVariant { path, value, .. } => {
                self.compile_union_expr(handler, asm, expr, path, value, contract, pred)
            }
            Expr::Nil(_) => self.compile_nil_expr(handler, asm, expr, contract),
            Expr::UnaryOp { op, expr, .. } => {
                self.compile_unary_op(handler, asm, op, expr, contract, pred)
            }
            Expr::BinaryOp { op, lhs, rhs, .. } => {
                self.compile_binary_op(handler, asm, op, lhs, rhs, contract, pred)
            }
            Expr::IntrinsicCall { kind, args, .. } => {
                self.compile_intrinsic_call(handler, asm, &kind.0, args, contract, pred)
            }
            Expr::LocalPredicateCall {
                predicate, args, ..
            } => self.compile_local_predicate_call(handler, asm, predicate, args, contract, pred),
            Expr::ExternalPredicateCall {
                c_addr,
                p_addr,
                args,
                ..
            } => self.compile_external_predicate_call(
                handler, asm, c_addr, p_addr, args, contract, pred,
            ),
            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => self.compile_select(
                handler, asm, condition, then_expr, else_expr, contract, pred,
            ),
            Expr::Index { expr, index, .. } => {
                self.compile_index(handler, asm, expr, index, contract, pred)
            }
            Expr::TupleFieldAccess { tuple, field, .. } => {
                self.compile_tuple_field_access(handler, asm, tuple, field, contract, pred)
            }
            Expr::UnionTag { union_expr, .. } => {
                self.compile_union_tag_is(handler, asm, union_expr, contract, pred)
            }
            Expr::UnionValue { union_expr, .. } => {
                self.compile_union_get_value(handler, asm, union_expr, contract, pred)
            }
            Expr::Map {
                param, range, body, ..
            } => self.compile_loop_map(handler, asm, &param.name, range, body, contract, pred),
            Expr::Error(_)
            | Expr::LocalStorageAccess { .. }
            | Expr::ExternalStorageAccess { .. }
            | Expr::MacroCall { .. }
            | Expr::Cast { .. }
            | Expr::In { .. }
            | Expr::Range { .. }
            | Expr::Generator { .. }
            | Expr::Match { .. } => Err(handler.emit_internal_err(
                "These expressions should have been lowered by now",
                empty_span(),
            )),
        }
    }

    /// Compile a path expression. Assumes that each path expressions corresponds to a predicate
    /// parameter or a variable. All other paths should have been lowered to something else
    /// by now.
    fn compile_path(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        path: &String,
        reads: Reads,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        if let Some((loc, sz)) = self
            .morphism_param
            .as_ref()
            .and_then(|(param_name, loc, sz)| {
                (param_name == path || path.starts_with("::") && param_name == &path[2..])
                    .then_some((loc, sz))
            })
        {
            // This path is to a morphism parameter.
            self.compile_morphism_param(asm, *loc, *sz)
        } else if let Some((param_index, _)) = pred
            .params
            .iter()
            .enumerate()
            .find(|(_, param)| &param.name.name == path)
        {
            asm.extend([
                PUSH(param_index as i64), // predicate parameter index
                PUSH(0),                  // placeholder for index computations
            ]);
            Ok(Location::PredicateData)
        } else if pred.variables().any(|(_, variable)| &variable.name == path) {
            asm.push(PUSH(self.var_to_mem_idx[&(path.clone(), reads)]));
            Ok(Location::Memory)
        } else {
            // Must not have anything else at this point. All other path expressions should have
            // been lowered to something else by now
            return Err(handler.emit_internal_err(
                "this path expression should have been lowered by now",
                empty_span(),
            ));
        }
    }

    /// Given an assembly block, convert each instruction in it into its corresponding op in
    /// `essential_asm::short` and append it to `asm`.
    ///
    /// TODO: we should really auto-generate this from the `essential-asm-spec` crate using a proc
    /// macro
    fn compile_asm_block(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        ops: &[AsmOp],
        _pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        asm.extend(
            ops.iter()
                .map(|op| match op {
                    AsmOp::Imm(imm, ..) => Ok(PUSH(*imm)),
                    AsmOp::Op(op) => match op.name.as_str() {
                        "ADD" => Ok(ADD),
                        "ALOC" => Ok(ALOC),
                        "AND" => Ok(AND),
                        "BAND" => Ok(BAND),
                        "BOR" => Ok(BOR),
                        "DATA" => Ok(DATA),
                        "DIV" => Ok(DIV),
                        "DLEN" => Ok(DLEN),
                        "DROP" => Ok(DROP),
                        "DSLT" => Ok(DSLT),
                        "DUP" => Ok(DUP),
                        "DUPF" => Ok(DUPF),
                        "EQ" => Ok(EQ),
                        "EQRA" => Ok(EQRA),
                        "EQST" => Ok(EQST),
                        "FREE" => Ok(FREE),
                        "GT" => Ok(GT),
                        "GTE" => Ok(GTE),
                        "HLT" => Ok(HLT),
                        "HLTIF" => Ok(HLTIF),
                        "JMPIF" => Ok(JMPIF),
                        "KREX" => Ok(KREX),
                        "KRNG" => Ok(KRNG),
                        "LOD" => Ok(LOD),
                        "LODR" => Ok(LODR),
                        "LODS" => Ok(LODS),
                        "LT" => Ok(LT),
                        "LTE" => Ok(LTE),
                        "MKEYS" => Ok(MKEYS),
                        "MOD" => Ok(MOD),
                        "MUL" => Ok(MUL),
                        "NOT" => Ok(NOT),
                        "OR" => Ok(OR),
                        "PEX" => Ok(PEX),
                        "PNCIF" => Ok(PNCIF),
                        "POP" => Ok(POP),
                        "PUSH" => Err(handler.emit_err(Error::Compile {
                            // While `PUSH` is an actual instruction, we still error out here
                            // because we expect the immediate to be pushed to be inserted directly
                            // without the `PUSH`
                            error: CompileError::BadPushInstruction {
                                span: op.span.clone(),
                            },
                        })),
                        "REP" => Ok(REP),
                        "REPC" => Ok(REPC),
                        "REPE" => Ok(REPE),
                        "RES" => Ok(RES),
                        "RSECP" => Ok(RSECP),
                        "SEL" => Ok(SEL),
                        "SHA2" => Ok(SHA2),
                        "SHL" => Ok(SHL),
                        "SHR" => Ok(SHR),
                        "SHRI" => Ok(SHRI),
                        "SLTR" => Ok(SLTR),
                        "STO" => Ok(STO),
                        "STOR" => Ok(STOR),
                        "STOS" => Ok(STOS),
                        "SUB" => Ok(SUB),
                        "SWAP" => Ok(SWAP),
                        "SWAPI" => Ok(SWAPI),
                        "THIS" => Ok(THIS),
                        "THISC" => Ok(THISC),
                        "VRFYED" => Ok(VRFYED),
                        _ => Err(handler.emit_err(Error::Compile {
                            error: CompileError::UnregonizedInstruction {
                                span: op.span.clone(),
                            },
                        })),
                    },
                })
                .collect::<Result<Vec<_>, _>>()?,
        );

        Ok(Location::Stack)
    }

    fn compile_morphism_param(
        &mut self,
        asm: &mut Asm,
        param_loc: Location,
        param_size: usize,
    ) -> Result<Location, ErrorEmitted> {
        match param_loc {
            Location::PredicateData => {
                // Push the slot index and calculate the inner index.
                asm.extend([
                    PUSH(0),
                    LODS, // Get the slot idx.
                    PUSH(param_size as i64),
                    REPC,
                    MUL, // Element size multiplied by the repeat counter.
                ]);

                Ok(Location::PredicateData)
            }

            Location::Memory => {
                // The index to the base of the array is stored at the very bottom of the stack
                // in scratch space.
                asm.extend([
                    PUSH(0),
                    LODS, // Get the index to the array base.
                ]);

                // If the size of the element is 1 then the ASM is simpler.
                if param_size == 1 {
                    asm.extend([REPC, ADD]);
                } else {
                    // Need to multiply the repeat counter by the size and add to the base.
                    asm.extend([PUSH(param_size as i64), REPC, MUL, ADD]);
                }

                Ok(Location::Memory)
            }

            // NOTE: These are unreachable for now.  Maps over storage or stack will always be over a
            // copy in a memory buffer.
            Location::Storage(_) => todo!(),
            Location::Stack => todo!(),
        }
    }

    fn compile_unary_op(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        op: &UnaryOp,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        match op {
            UnaryOp::Not => {
                self.compile_expr(handler, asm, expr, contract, pred)?;
                asm.push(NOT);
                Ok(Location::Stack)
            }
            UnaryOp::NextState => {
                if let Expr::Path(path, _) = expr.get(contract) {
                    self.compile_path(handler, asm, path, Reads::Post, pred)
                } else {
                    Err(handler.emit_internal_err(
                        "unexpected next state op for non-path".to_string(),
                        empty_span(),
                    ))
                }
            }
            UnaryOp::Neg => {
                // Push `0` (i.e. `lhs`) before the `expr` (i.e. `rhs`) opcodes. Then subtract
                // `lhs` - `rhs` to negate the value.
                asm.push(PUSH(0));
                self.compile_expr(handler, asm, expr, contract, pred)?;
                asm.push(SUB);
                Ok(Location::Stack)
            }

            UnaryOp::Unwrap => self.compile_unwrap_op(handler, asm, expr, contract, pred),

            UnaryOp::Error => {
                Err(handler.emit_internal_err("unexpected Unary::Error", empty_span()))
            }
        }
    }

    fn compile_unwrap_op(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let expr_size = expr.get_ty(contract).size(handler, contract)? as i64;
        let location = self.compile_expr_pointer(handler, asm, expr, contract, pred)?;
        match location {
            Location::PredicateData => {
                // Load the tag as the last word in the predicate data for `expr`. Panic if the tag
                // is 0. Otherwise, nothing to be done with the base address where the unwrapped
                // data lives.
                asm.extend([
                    PUSH(1),
                    DUPF, // duplicate `slot_ix
                    PUSH(1),
                    DUPF, // duplicate `value_ix`
                    PUSH(expr_size - 1),
                    ADD,     // Calculate the index of the tag (last word)
                    PUSH(1), // tag len
                    DATA,
                    NOT,
                    PNCIF,
                ]);
                Ok(location)
            }
            Location::Memory => {
                // Load the tag as the last word in the allocated memory for `expr`. Panic
                // if the tag is 0. Otherwise, nothing to be done with the base address
                // where the unwrapped data lives.
                asm.extend([DUP, PUSH(expr_size - 1), ADD, LOD, NOT, PNCIF]);
                Ok(location)
            }

            Location::Stack => {
                // Panic if the tag is 0. The tag is at the top of the stack. What remains
                // is the unwrapped data.
                asm.extend([NOT, PNCIF]);
                Ok(Location::Stack)
            }

            Location::Storage(is_extern) => {
                let num_keys = expr
                    .get_ty(contract)
                    .get_optional_ty()
                    .ok_or_else(|| {
                        handler.emit_internal_err(
                            "storage accesses must be of type optional",
                            empty_span(),
                        )
                    })?
                    .storage_keys(handler, contract)? as i64;

                let access_mem_idx = self.storage_expr_to_mem_idx[expr];

                // Read the storage keys into memory
                asm.extend([
                    PUSH(num_keys),
                    PUSH(access_mem_idx), // mem idx for the output of KREX or KRNG
                    if is_extern { KREX } else { KRNG },
                ]);

                // Sum the sizes of all the values read. These are laid out as follows in
                // memory:
                // `[a_addr, a_len, b_addr, b_len, a_value, b_value, ...]`
                if num_keys > 1 {
                    asm.push(PUSH(0));
                    asm.extend(
                        (0..num_keys).flat_map(|i| [PUSH(access_mem_idx + 2 * i + 1), LOD, ADD]),
                    );
                } else {
                    asm.extend([PUSH(access_mem_idx + 1), LOD]);
                }

                // This computes the tag on the stack and panics if the tag is 0.
                //
                // If the total size read from storage is equal to the size of the value in
                // the optional, then the tag should be 1.  Otherwise, it should be 0.
                asm.extend([PUSH(expr_size - 1), EQ, NOT, PNCIF]);

                // Now read the actual unwrapped data
                asm.extend([
                    PUSH(access_mem_idx + num_keys * 2), // where the actual data lives
                    PUSH(expr_size - 1),                 // size of the data (excluding the tag)
                    LODR,
                ]);

                Ok(Location::Stack)
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_binary_op(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        op: &BinaryOp,
        lhs: &ExprKey,
        rhs: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let lhs_len = self.compile_expr(handler, asm, lhs, contract, pred)?;
        let rhs_len = self.compile_expr(handler, asm, rhs, contract, pred)?;

        match op {
            BinaryOp::Add => asm.push(ADD),
            BinaryOp::Sub => asm.push(SUB),
            BinaryOp::Mul => asm.push(MUL),
            BinaryOp::Div => asm.push(DIV),
            BinaryOp::Mod => asm.push(MOD),
            BinaryOp::Equal => {
                let type_size = lhs.get_ty(contract).size(handler, contract)?;
                if type_size == 1 {
                    asm.push(EQ);
                } else {
                    asm.extend([PUSH(type_size as i64), EQRA]);
                }
            }
            BinaryOp::NotEqual => {
                let type_size = lhs.get_ty(contract).size(handler, contract)?;
                if type_size == 1 {
                    asm.extend([EQ, NOT]);
                } else {
                    asm.extend([PUSH(type_size as i64), EQRA, NOT]);
                }
            }
            BinaryOp::LessThanOrEqual => asm.push(LTE),
            BinaryOp::LessThan => asm.push(LT),
            BinaryOp::GreaterThanOrEqual => asm.push(GTE),
            BinaryOp::GreaterThan => asm.push(GT),
            BinaryOp::LogicalAnd => {
                // Short-circuit AND. Using `JMPIF`, converts `x && y` to:
                // if !x { false } else { y }

                // Location right before the `lhs` opcodes
                let lhs_position = asm.len() - rhs_len - lhs_len;

                // Location right before the `rhs` opcodes
                let rhs_position = asm.len() - rhs_len;

                // Push `false` before `lhs` opcodes. This is the result of the `AND` operation if
                // `lhs` is false.
                asm.insert(lhs_position, PUSH(0));

                // Then push the number of instructions to skip over if the `lhs` is true.  That's
                // `rhs_len + 2` because we're going to add to add `POP` later and we want to skip
                // over that AND all the `rhs` opcodes
                asm.insert(lhs_position + 1, PUSH(rhs_len as i64 + 2));

                // Now, invert `lhs` to get the jump condition which is `!lhs`
                asm.insert(rhs_position + 2, NOT);

                // Then, add the `JMPIF` instruction after the `rhs` opcodes and the two
                // newly added opcodes. The `lhs` is the condition.
                asm.insert(rhs_position + 3, JMPIF);

                // Finally, insert a ` POP`. The point here is that if the jump condition (i.e.
                // `!lhs`) is false, then we want to remove the `true` we push on the stack above.
                asm.insert(rhs_position + 4, POP);
            }
            BinaryOp::LogicalOr => {
                // Short-circuit OR. Using `JMPIF`, converts `x || y` to:
                // if x { true } else { y }

                // Location right before the `lhs` opcodes
                let lhs_position = asm.len() - rhs_len - lhs_len;

                // Location right before the `rhs` opcodes
                let rhs_position = asm.len() - rhs_len;

                // Push `true` before `lhs` opcodes. This is the result of the `OR` operation if
                // `lhs` is true.
                asm.insert(lhs_position, PUSH(1));

                // Then push the number of instructions to skip over if the `lhs` is true.  That's
                // `rhs_len + 2` because we're going to add to add `POP` later and we want to skip
                // over that AND all the `rhs` opcodes
                asm.insert(lhs_position + 1, PUSH(rhs_len as i64 + 2));

                // Now add the `JMPIF` instruction after the `rhs` opcodes and the two
                // newly added opcodes. The `lhs` is the condition.
                asm.insert(rhs_position + 2, JMPIF);

                // Then, insert a ` POP`. The point here is that if the jump condition (i.e. `lhs`)
                // is false, then we want to remove the `true` we push on the stack above.
                asm.insert(rhs_position + 3, POP);
            }
        }
        Ok(Location::Stack)
    }

    fn compile_intrinsic_call(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        kind: &IntrinsicKind,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let expected_args = kind.args();
        for (expected, arg) in expected_args.iter().zip(args.iter()) {
            let found = arg.get_ty(contract);
            if !expected.eq(contract, found) {
                handler.emit_internal_err("unexpected intrinsic arg type", empty_span());
            }
        }

        // Also, ensure that the number of arguments is correct
        if args.len() != expected_args.len() {
            handler.emit_internal_err("unexpected number of args for intrinsic", empty_span());
        }

        match kind {
            IntrinsicKind::External(kind) => {
                self.compile_external_intrinsic_call(handler, asm, kind, args, contract, pred)
            }

            IntrinsicKind::Internal(kind) => {
                self.compile_internal_intrinsic_call(handler, asm, kind, args, contract, pred)
            }

            IntrinsicKind::Error => {
                Err(handler.emit_internal_err("intrinsic of kind `Error` encounter", empty_span()))
            }
        }
    }

    fn compile_external_intrinsic_call(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        kind: &ExternalIntrinsic,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        match kind {
            ExternalIntrinsic::AddressOf => {
                if let Some(Expr::Immediate {
                    value: Immediate::String(s),
                    ..
                }) = args[0].try_get(contract)
                {
                    // Push the predicate address on the stack, one word at a time.
                    let Some(predicate_address) = &self.compiled_predicates.get(s) else {
                        return Err(handler
                            .emit_internal_err("predicate address should exist!", empty_span()));
                    };

                    for word in essential_types::convert::word_4_from_u8_32(predicate_address.1 .0)
                    {
                        asm.push(PUSH(word));
                    }
                }
            }

            ExternalIntrinsic::ArrayLen => {
                match self.compile_expr_pointer(handler, asm, &args[0], contract, pred)? {
                    Location::PredicateData => {
                        // The length of the array is at the start.
                        asm.extend([PUSH(1), DATA]);
                    }

                    Location::Memory => todo!("__len() of array in memory"),
                    Location::Storage(_) => todo!("__len() of array in stack"),
                    Location::Stack => todo!("__len() of array on stack"),
                }
            }

            ExternalIntrinsic::SizeOf => {
                asm.push(PUSH(
                    args[0].get_ty(contract).size(handler, contract)? as i64
                ));
            }

            ExternalIntrinsic::VerifyEd25519 => {
                self.compile_expr(handler, asm, &args[0], contract, pred)?;
                asm.push(PUSH(
                    8 * args[0].get_ty(contract).size(handler, contract)? as i64,
                ));
                self.compile_expr(handler, asm, &args[1], contract, pred)?;
                self.compile_expr(handler, asm, &args[2], contract, pred)?;
                asm.push(VRFYED)
            }

            // All other external intrinsics can be handled generically
            _ => {
                for (i, arg) in args.iter().enumerate() {
                    self.compile_expr(handler, asm, arg, contract, pred)?;

                    // if the type of the arg is `Any`, then follow with its size
                    if kind.args()[i].is_any() {
                        asm.push(PUSH(arg.get_ty(contract).size(handler, contract)? as i64));
                    }
                }

                match kind {
                    ExternalIntrinsic::RecoverSECP256k1 => asm.push(RSECP),

                    ExternalIntrinsic::Sha256 => asm.extend([PUSH(3), SHL, SHA2]),

                    ExternalIntrinsic::ThisAddress => asm.push(THIS),

                    ExternalIntrinsic::ThisContractAddress => asm.push(THISC),

                    ExternalIntrinsic::AddressOf
                    | ExternalIntrinsic::ArrayLen
                    | ExternalIntrinsic::SizeOf
                    | ExternalIntrinsic::VerifyEd25519 => {
                        return Err(handler.emit_internal_err(
                            "SizeOf and AddressOf have already been handled!",
                            empty_span(),
                        ))
                    }

                    ExternalIntrinsic::VecLen => {
                        return Err(handler.emit_internal_err(
                            "__vec_len should have been lowered to something else by now",
                            empty_span(),
                        ))
                    }
                }
            }
        }

        Ok(Location::Stack)
    }

    fn compile_internal_intrinsic_call(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        kind: &InternalIntrinsic,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        for (i, arg) in args.iter().enumerate() {
            self.compile_expr(handler, asm, arg, contract, pred)?;

            // if the type of the arg is `Any`, then follow with its size
            //
            // The only exception is set types since they are not very well specified yet. The
            // length of a set currently lives in the set itself.
            if !matches!(kind, InternalIntrinsic::EqSet) && kind.args()[i].is_any() {
                asm.push(PUSH(arg.get_ty(contract).size(handler, contract)? as i64));
            }
        }

        match kind {
            InternalIntrinsic::EqSet => {
                asm.push(EQST);
                Ok(Location::Stack)
            }
            InternalIntrinsic::MutKeys => {
                asm.push(MKEYS);
                Ok(Location::Stack)
            }
            InternalIntrinsic::PreState | InternalIntrinsic::PostState => {
                Ok(Location::Storage(false))
            }
            InternalIntrinsic::PreStateExtern | InternalIntrinsic::PostStateExtern => {
                Ok(Location::Storage(true))
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_local_predicate_call(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        predicate: &String,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // Use `PredicateExists` here which takes the following as input:
        //
        // [sha256(arg0len, arg0, argNlen, argN, contract_addr, predicate_addr)]
        //
        // First compute the total size, in bytes, of the data to hash. This includes 2 `b256` for
        // the contract and predicate addresses (hence why we start with 64 bytes), the size of
        // each arg, and an integer per arg desribing the arg length
        let data_to_hash_size = args.iter().try_fold(64, |acc, arg| {
            let arg_size = arg.get_ty(contract).size(handler, contract)?;
            asm.push(PUSH(arg_size as i64));
            self.compile_expr(handler, asm, arg, contract, pred)?;
            Ok(acc + 8 * (1 + arg_size))
        })?;

        // This is a local predicate call: use the current contract address here.
        asm.push(THISC);

        // This is a local predicate call: use the pre-computed predicate address of the called
        // predicate
        let Some(predicate_address) = &self.compiled_predicates.get(predicate) else {
            return Err(handler.emit_internal_err("predicate address should exist!", empty_span()));
        };

        for word in essential_types::convert::word_4_from_u8_32(predicate_address.1 .0) {
            asm.push(PUSH(word));
        }

        asm.extend([PUSH(data_to_hash_size as i64), SHA2, PEX]);

        Ok(Location::Stack)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_external_predicate_call(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        c_addr: &ExprKey,
        p_addr: &ExprKey,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // Use `PredicateExists` here which takes the following as input:
        //
        // [sha256(arg0len, arg0, argNlen, argN, contract_addr, predicate_addr)]
        //
        // First compute the total size, in bytes, of the data to hash. This includes 2 `b256` for
        // the contract and predicate addresses (hence why we start with 64 bytes), the size of
        // each arg, and an integer per arg desribing the arg length
        let data_to_hash_size = args.iter().try_fold(64, |acc, arg| {
            let arg_size = arg.get_ty(contract).size(handler, contract)?;
            asm.push(PUSH(arg_size as i64));
            self.compile_expr(handler, asm, arg, contract, pred)?;
            Ok(acc + 8 * (1 + arg_size))
        })?;

        self.compile_expr(handler, asm, c_addr, contract, pred)?;
        self.compile_expr(handler, asm, p_addr, contract, pred)?;

        asm.extend([PUSH(data_to_hash_size as i64), SHA2, PEX]);

        Ok(Location::Stack)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_select(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        condition: &ExprKey,
        then_expr: &ExprKey,
        else_expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        if then_expr.can_panic(contract, pred) || else_expr.can_panic(contract, pred) {
            // We need to short circuit these with control flow to avoid potential panics.  The
            // 'else' is put before the 'then' since it's easier to jump-if-true.
            //
            // This jump to 'then' will get updated with the proper distance below.
            let to_then_jump_idx = asm.len();
            asm.push(PUSH(-1));
            self.compile_expr(handler, asm, condition, contract, pred)?;
            asm.push(JMPIF);

            // Compile the 'else' selection, update the prior jump.  We need to jump over the size
            // of 'else` plus 3 instructions it uses to jump the 'then'.
            let else_size = self.compile_expr(handler, asm, else_expr, contract, pred)?;

            asm[to_then_jump_idx] = PUSH(else_size as i64 + 4);

            // This (unconditional) jump over 'then' will also get updated.
            let to_end_jump_idx = asm.len();
            asm.extend([PUSH(-1), PUSH(1), JMPIF]);

            // Compile the 'then' selection, update the prior jump.
            let then_size = self.compile_expr(handler, asm, then_expr, contract, pred)?;

            asm[to_end_jump_idx] = PUSH(then_size as i64 + 1);
        } else {
            // Alternatively, evaluate both options and use ASM `select` to choose one.
            let type_size = then_expr.get_ty(contract).size(handler, contract)?;
            self.compile_expr(handler, asm, else_expr, contract, pred)?;
            self.compile_expr(handler, asm, then_expr, contract, pred)?;
            if type_size == 1 {
                self.compile_expr(handler, asm, condition, contract, pred)?;
                asm.push(SEL);
            } else {
                asm.push(PUSH(type_size as i64));
                self.compile_expr(handler, asm, condition, contract, pred)?;
                asm.push(SLTR);
            }
        }
        Ok(Location::Stack)
    }

    fn compile_index(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        index: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, asm, expr, contract, pred)?;

        if let Location::Stack | Location::Storage(_) = location {
            return Err(handler.emit_internal_err(
                "unexpected index operator for `Location::Stack` or `Location::Storage`",
                empty_span(),
            ));
        }

        // Compile the index
        self.compile_expr(handler, asm, index, contract, pred)?;

        // Grab the element ty of the array
        match expr.get_ty(contract) {
            Type::FixedArray { ty, .. } => {
                let el_size = ty.size(handler, contract)? as i64;
                if el_size > 1 {
                    // Multiply the index by the size of `ty`.
                    asm.extend([PUSH(el_size), MUL]);
                }

                // Add it to the base index.
                asm.push(ADD);
            }

            Type::UnsizedArray { ty, .. } => {
                // Panic if the index is out of bounds.  Get the length from the front (since we
                // only support PredicateData or Memory) and check it.
                match location {
                    Location::PredicateData => {
                        asm.extend([
                            PUSH(2), //
                            DUPF,    // Dupe the slot idx.
                            PUSH(2), //
                            DUPF,    // Dupe the base idx.
                            PUSH(1), // Length of 1.
                            DATA,
                        ]);
                    }

                    Location::Memory => {
                        asm.extend([
                            PUSH(1), //
                            DUPF,    // Dupe the mem idx.
                            LOD,     //
                        ]);
                    }

                    Location::Storage(_) | Location::Stack => unreachable!("already checked above"),
                }

                // Dupe the array index and compare to the length.
                asm.extend([
                    PUSH(1), //
                    DUPF,    // Dupe the array idx.
                    LTE,     // Length is <= idx?
                    PNCIF,   // Panic if so.
                ]);

                let el_size = ty.size(handler, contract)? as i64;
                if el_size > 1 {
                    // Multiply the index by the size of `ty`.
                    asm.extend([PUSH(el_size), MUL]);
                }

                // Add it to the base index and skip the length.
                asm.extend([ADD, PUSH(1), ADD]);
            }

            _ => {
                return Err(
                    handler.emit_internal_err("type must exist and be an array type", empty_span())
                );
            }
        }

        Ok(location)
    }

    fn compile_tuple_field_access(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        tuple: &ExprKey,
        field: &TupleAccess,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, asm, tuple, contract, pred)?;

        if let Location::Stack | Location::Storage(_) = location {
            return Err(handler.emit_internal_err(
                "unexpected tuple access for `Location::Stack` and `Location::Storage`",
                empty_span(),
            ));
        }

        // Grab the fields of the tuple
        let Type::Tuple { ref fields, .. } = tuple.get_ty(contract) else {
            return Err(
                handler.emit_internal_err("type must exist and be a tuple type", empty_span())
            );
        };

        // The field index is based on the type definition
        let field_idx = match field {
            TupleAccess::Index(idx) => *idx,

            TupleAccess::Name(ident) => fields
                .iter()
                .position(|(field_name, _)| {
                    field_name
                        .as_ref()
                        .is_some_and(|name| name.name == ident.name)
                })
                .ok_or_else(|| {
                    handler.emit_internal_err(
                        "field name must exist, this was checked in type checking",
                        empty_span(),
                    )
                })?,

            TupleAccess::Error => {
                return Err(
                    handler.emit_internal_err("unexpected TupleAccess::Error", empty_span())
                );
            }
        };

        // Use `Add` to compute the offset from the base key where the full tuple is stored, if
        // necessary.
        let offset = fields.iter().take(field_idx).try_fold(0, |acc, (_, ty)| {
            ty.size(handler, contract).map(|slots| acc + slots)
        })? as i64;
        if offset > 0 {
            asm.push(PUSH(offset));
            asm.push(ADD);
        }

        Ok(location)
    }

    fn compile_nil_expr(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        contract: &Contract,
    ) -> Result<Location, ErrorEmitted> {
        // tag and value are both zeros
        asm.push(PUSH(expr.get_ty(contract).size(handler, contract)? as i64));
        asm.extend([RES, POP]);
        Ok(Location::Stack)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_union_expr(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        union_expr_key: &ExprKey,
        tag: &str,
        value: &Option<ExprKey>,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // Find the tag string in the union decl and convert to an index.
        let union_ty = union_expr_key.get_ty(contract);
        let tag_num = union_ty
            .get_union_variant_names(contract)
            .into_iter()
            .enumerate()
            .find_map(|(idx, variant_name)| (variant_name == tag[2..]).then_some(idx))
            .ok_or_else(|| {
                handler.emit_internal_err(
                    "Union tag not found in union decl",
                    contract.expr_key_to_span(*union_expr_key),
                )
            })?;

        // Track the actual value size in words.
        let mut actual_value_size = 0;

        // Push the tag and then compile the value if necessary.
        asm.push(PUSH(tag_num as i64));
        if let Some(value_key) = value {
            self.compile_expr(handler, asm, value_key, contract, pred)?;
            actual_value_size = value_key.get_ty(contract).size(handler, contract)?;
        }

        // Get the total union (max) size MINUS one since .size() includes the tag word.
        let union_value_size = union_ty.size(handler, contract)? - 1;
        while union_value_size > actual_value_size {
            // Pad out the value with zeros.
            asm.push(PUSH(0));
            actual_value_size += 1;
        }

        Ok(Location::Stack)
    }

    fn compile_union_tag_is(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        union_expr_key: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // Get the location of the union but read just a single word, as we only want the tag at
        // the front.
        match self.compile_expr_pointer(handler, asm, union_expr_key, contract, pred)? {
            Location::PredicateData => asm.extend([PUSH(1), DATA]),

            // Are these supported?
            Location::Memory | Location::Storage { .. } | Location::Stack => {
                unimplemented!("support union matches in non- predicate data?")
            }
        }

        Ok(Location::Stack)
    }

    fn compile_union_get_value(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        union_expr_key: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, asm, union_expr_key, contract, pred)?;
        match location {
            Location::Memory | Location::PredicateData => {
                // Skip the tag.
                asm.extend([PUSH(1), ADD]);
                Ok(location)
            }

            Location::Stack | Location::Storage { .. } => {
                unimplemented!("union value or unions in storage")
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_loop_map(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        param_name: &str,
        range_key: &ExprKey,
        body_key: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let range_ty = range_key.get_ty(contract);

        let Some(in_el_ty) = range_ty.get_array_el_type() else {
            todo!("Implement map expressions for non-array ranges.")
        };
        let in_el_size = in_el_ty.size(handler, contract)?;

        let in_ary_location = self.compile_expr_pointer(handler, asm, range_key, contract, pred)?;

        let out_ary_location = match in_ary_location {
            Location::PredicateData => {
                // When compiling these there's a slot index and a zero (index) pushed to the
                // stack. We need to store the slot index in the global area so it can be resolved
                // by the iterator.
                asm.extend([
                    POP,     // We don't need the index.
                    PUSH(0), // Scratch address at bottom of stack.
                    STOS,
                ]);

                Location::PredicateData
            }

            Location::Memory => {
                // Just store the array address at the scratch space at bottom of stack.
                asm.extend([PUSH(0), STOS]);

                Location::Memory
            }

            Location::Storage(is_extern) => {
                let num_keys = range_ty
                    .get_optional_ty()
                    .ok_or_else(|| {
                        handler.emit_internal_err(
                            "storage accesses must be of type optional",
                            empty_span(),
                        )
                    })?
                    .storage_keys(handler, contract)? as i64;

                let access_mem_idx = self.storage_expr_to_mem_idx[range_key];

                asm.extend([
                    PUSH(num_keys),
                    PUSH(access_mem_idx),
                    if is_extern { KREX } else { KRNG }, // Read the keys and values into memory.
                    PUSH(access_mem_idx + num_keys * 2), // Offset to values.
                    PUSH(0),
                    STOS, // Store offset in scratch space at bottom of stack.
                ]);

                // We've moved the location of the array.
                Location::Memory
            }

            Location::Stack => {
                // We need to move input array off the stack and into a new heap buffer.  Get the
                // array length.
                self.compile_array_size(handler, asm, range_ty, in_el_size, contract)?;

                // Allocate a buffer, move array to it.
                asm.extend([
                    DUP,     // Dupe the size.
                    ALOC,    // Allocate; now buf idx and size are on the stack.
                    DUP,     // Dupe the buf idx.
                    PUSH(0), //
                    STOS,    // Store buf idx to scratch space.
                    STOR,    // Now move entire array to memory buffer.
                ]);

                // We've moved the location of the array.
                Location::Memory
            }
        };

        // Depending on which type of location the array lives we need to bind the
        // iterator.  The slot index is stored in the global area.
        self.morphism_param = Some((param_name.to_string(), out_ary_location, in_el_size));

        // Put the array length onto the stack.
        self.compile_array_num_entries(handler, asm, range_ty, contract)?;

        // Start the loop, counting up.
        asm.extend([
            PUSH(1), // 1 == 0..len
            REP,
        ]);

        // Compile the loop body.
        let body_result_location =
            self.compile_expr_pointer(handler, asm, body_key, contract, pred)?;

        match body_result_location {
            Location::PredicateData => todo!("Copy morphism body result from pred data to stack"),
            Location::Memory => todo!("Copy morphism body result from memory to stack"),
            Location::Storage(_) => todo!("Copy morphism body result from storage to stack"),

            Location::Stack => {} // Leave it.
        }

        // Finish the loop.
        asm.push(REPE);

        Ok(Location::Stack)
    }

    fn compile_array_num_entries(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        array_ty: &Type,
        contract: &Contract,
    ) -> Result<(), ErrorEmitted> {
        self.compile_array_size(handler, asm, array_ty, 1, contract)
    }

    fn compile_array_size(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        array_ty: &Type,
        el_size: usize,
        contract: &Contract,
    ) -> Result<(), ErrorEmitted> {
        // Currently only supporting fixed size arrays, where the size may be extracted from the
        // type.  Will need to support variable sized arrays using a `len()` operation of some
        // sort, depending on the array location.

        let Some(array_size) = array_ty
            .get_array_size()
            .map(Ok)
            .or_else(|| {
                array_ty.get_array_range_expr().map(|range_expr_key| {
                    Type::get_array_size_from_range_expr(handler, range_expr_key, contract)
                })
            })
            .transpose()?
        else {
            return Err(handler.emit_internal_err(
                "Truly unable to get array size for ASM gen.",
                array_ty.span().clone(),
            ));
        };

        asm.push(PUSH(array_size * el_size as i64));

        Ok(())
    }
}
