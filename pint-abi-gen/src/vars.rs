//! Struct and impls for the decision variables `Vars` type.

use crate::{field_name_from_var_name, ty_from_pint_ty};
use pint_abi_types::VarABI;
use proc_macro2::Span;

/// A named field for each of the decision variables.
fn fields(vars: &[VarABI]) -> Vec<syn::Field> {
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

/// Generate a struct for an predicate's decision variables.
fn struct_decl(vars: &[VarABI]) -> syn::ItemStruct {
    let fields = fields(vars);
    syn::parse_quote! {
        /// The predicate's decision variables.
        #[derive(Clone, Debug)]
        pub struct Vars {
            #(
                #fields
            ),*
        }
    }
}

/// Generate a `Encode` implementation for a `Vars` type.
fn impl_encode(vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents = vars
        .iter()
        .map(|var| field_name_from_var_name(&var.name))
        .map(|name| syn::Ident::new(&name, Span::call_site()));
    syn::parse_quote! {
        impl pint_abi::Encode for Vars {
            fn encode<W: pint_abi::Write>(&self, w: &mut W) -> Result<(), W::Error> {
                #(
                    pint_abi::Encode::encode(&self.#field_idents, w).expect("cannot fail");
                )*
                Ok(())
            }
        }
    }
}

/// Generate a `From<Vars>` implementation for converting `Vars` to `Vec<Value>`.
fn impl_(vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents = vars
        .iter()
        .map(|var| field_name_from_var_name(&var.name))
        .map(|name| syn::Ident::new(&name, Span::call_site()));
    syn::parse_quote! {
        impl From<Vars> for Vec<pint_abi::types::essential::Value> {
            fn from(vars: Vars) -> Self {
                let mut values: Vec<pint_abi::types::essential::Value> = vec![];
                #(
                    values.push(pint_abi::encode(&vars.#field_idents));
                )*
                values
            }
        }
    }
}

/// The struct decl and impls for the decision variables `Vars` type.
pub(crate) fn items(vars: &[VarABI]) -> Vec<syn::Item> {
    vec![
        struct_decl(vars).into(),
        impl_encode(vars).into(),
        impl_(vars).into(),
    ]
}
