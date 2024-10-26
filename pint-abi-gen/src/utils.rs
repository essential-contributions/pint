//! Utility functions for working with `ParamABI`

use crate::{field_name_from_var_name, ty_from_pint_ty};
use pint_abi_types::ParamABI;
use proc_macro2::Span;

/// A named field for each of the decision variables.
pub(super) fn fields(vars: &[ParamABI], mod_level: usize) -> Vec<syn::Field> {
    vars.iter()
        .map(|var| {
            let name = field_name_from_var_name(&var.name);
            let ident = syn::Ident::new(&name, Span::call_site());
            let ty = ty_from_pint_ty(&var.ty, mod_level);
            syn::parse_quote! {
                pub #ident: #ty
            }
        })
        .collect()
}

/// Just the ident for each field.
pub(super) fn field_idents(vars: &[ParamABI]) -> impl '_ + Iterator<Item = syn::Ident> {
    vars.iter()
        .map(|var| field_name_from_var_name(&var.name))
        .map(|name| syn::Ident::new(&name, Span::call_site()))
}
