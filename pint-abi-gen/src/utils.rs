//! Utility functions for working with `VarABI`

use crate::{field_name_from_var_name, ty_from_pint_ty};
use pint_abi_types::VarABI;
use proc_macro2::Span;

/// A named field for each of the decision variables.
pub(super) fn fields(vars: &[VarABI]) -> Vec<syn::Field> {
    vars.iter()
        .map(|var| {
            let name = field_name_from_var_name(&var.name);
            let ident = syn::Ident::new(&name, Span::call_site());
            let ty = ty_from_pint_ty(&var.ty);
            syn::parse_quote! {
                pub #ident: #ty
            }
        })
        .collect()
}

/// Just the ident for each field.
pub(super) fn field_idents(vars: &[VarABI]) -> impl '_ + Iterator<Item = syn::Ident> {
    vars.iter()
        .map(|var| field_name_from_var_name(&var.name))
        .map(|name| syn::Ident::new(&name, Span::call_site()))
}