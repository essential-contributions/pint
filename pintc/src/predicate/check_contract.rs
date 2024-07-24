use super::Contract;

use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    predicate::{ConstraintDecl, IfDecl, InterfaceInstance, PredicateInstance},
    span::Span,
};

impl Contract {
    /// Makes sure that the contract structure is correct. The root Pred cannot have variables,
    /// states, or constraints.
    pub fn check_contract(self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        fn emit_invalid_decl_err(handler: &Handler, kind: &str, span: &Span) {
            handler.emit_err(Error::Compile {
                error: CompileError::InvalidDeclOutsidePredicateDecl {
                    kind: kind.to_string(),
                    span: span.clone(),
                },
            });
        }

        let root_pred = self.root_pred();
        for (_, var) in root_pred.vars() {
            emit_invalid_decl_err(handler, "variable", &var.span);
        }
        for (_, state) in root_pred.states() {
            emit_invalid_decl_err(handler, "state", &state.span);
        }
        for ConstraintDecl { span, .. } in &root_pred.constraints {
            emit_invalid_decl_err(handler, "constraint", span);
        }
        for IfDecl { span, .. } in &root_pred.if_decls {
            emit_invalid_decl_err(handler, "`if` statement", span);
        }
        for InterfaceInstance { span, .. } in &root_pred.interface_instances {
            emit_invalid_decl_err(handler, "interface instance", span);
        }
        for PredicateInstance { span, .. } in &root_pred.predicate_instances {
            handler.emit_err(Error::Compile {
                error: CompileError::InvalidDeclOutsidePredicateDecl {
                    kind: "predicate instance".to_string(),
                    span: span.clone(),
                },
            });
        }

        if handler.has_errors() {
            return Err(handler.cancel());
        }

        Ok(self)
    }
}
