//! Items related to the generation of address constants for contracts and their predicates.

use essential_types::{contract::Contract, ContentAddress};

/// The contract and predicate addresses.
pub(crate) struct Addresses {
    pub(crate) contract: ContentAddress,
    /// Predicate content addresses in the order declared within the ABI.
    pub(crate) predicates: Vec<ContentAddress>,
}

impl<'a> From<&'a Contract> for Addresses {
    fn from(contract: &'a Contract) -> Self {
        let predicates: Vec<_> = contract
            .predicates
            .iter()
            .map(essential_hash::content_addr)
            .collect();
        Self {
            contract: essential_hash::contract_addr::from_predicate_addrs(
                predicates.iter().cloned(),
                &contract.salt,
            ),
            predicates,
        }
    }
}

/// The array of bytes for a content address.
fn content_addr_expr_array(addr: &ContentAddress) -> syn::ExprArray {
    let elems = addr
        .0
        .iter()
        .map(|b| {
            let byte: syn::Expr = syn::parse_quote!(#b);
            byte
        })
        .collect();
    syn::ExprArray {
        attrs: vec![],
        bracket_token: Default::default(),
        elems,
    }
}

/// An expression of type `ContentAddress`.
fn content_addr_expr(addr: &ContentAddress) -> syn::Expr {
    let expr_array = content_addr_expr_array(addr);
    syn::parse_quote!(pint_abi::types::essential::ContentAddress(#expr_array))
}

/// A `PredicateAddress` expression for predicate address consts.
fn predicate_addr_expr(contract: &ContentAddress, predicate: &ContentAddress) -> syn::Expr {
    let contract = content_addr_expr(contract);
    let predicate = content_addr_expr(predicate);
    syn::parse_quote! {
        pint_abi::types::essential::PredicateAddress {
            contract: #contract,
            predicate: #predicate,
        }
    }
}

/// An `ADDRESS` constant with the contract's `ContentAddress`.
pub(crate) fn contract_const(addr: &ContentAddress) -> syn::ItemConst {
    let expr: syn::Expr = content_addr_expr(addr);
    let doc_str = format!("- Address: `{addr}`");
    syn::parse_quote! {
        /// The content address of the contract.
        ///
        #[doc = #doc_str]
        pub const ADDRESS: pint_abi::types::essential::ContentAddress = #expr;
    }
}

/// An `ADDRESS` constant with the predicate's `PredicateAddress`.
pub(crate) fn predicate_const(
    contract: &ContentAddress,
    predicate: &ContentAddress,
) -> syn::ItemConst {
    let expr = predicate_addr_expr(contract, predicate);
    let doc_str = format!("- Contract: `{contract}`\n- Predicate: `{predicate}`");
    syn::parse_quote! {
        /// The predicate address, composed of the contract and predicate content addresses.
        ///
        #[doc = #doc_str]
        pub const ADDRESS: pint_abi::types::essential::PredicateAddress = #expr;
    }
}
