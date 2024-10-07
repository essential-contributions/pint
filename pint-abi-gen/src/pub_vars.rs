//! Struct and impls for the decision variables `PubVars` type.

use crate::utils::{field_idents, fields};
use pint_abi_types::VarABI;

/// Generate a struct for an predicate's decision variables.
fn struct_decl(pub_vars: &[VarABI]) -> syn::ItemStruct {
    let fields = fields(pub_vars, 1);
    syn::parse_quote! {
        /// The predicate's decision variables.
        #[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
        pub struct PubVars {
            #(
                #fields
            ),*
        }
    }
}

/// Generate an `Encode` implementation for a `PubVars` type.
fn impl_encode(pub_vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents = field_idents(pub_vars);
    syn::parse_quote! {
        impl pint_abi::Encode for PubVars {
            fn encode<W: pint_abi::Write>(&self, w: &mut W) -> Result<(), W::Error> {
                #(
                    pint_abi::Encode::encode(&self.#field_idents, w).expect("cannot fail");
                )*
                Ok(())
            }
        }
    }
}

/// Generate a `Decode` implementation for a `PubVars` type.
fn impl_decode(pub_vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents: Vec<_> = field_idents(pub_vars).collect();
    syn::parse_quote! {
        impl pint_abi::Decode for PubVars {
            type Error = PubVarDecodeError;
            fn decode<R: pint_abi::Read>(r: &mut R) -> Result<Self, Self::Error> {
                #(
                    let #field_idents = <_>::decode(r).map_err(|e| {
                        PubVarDecodeError {
                            field: PubVarDecodeFieldError::#field_idents,
                            err: format!("{e}"),
                        }
                    })?;
                )*
                Ok(Self {
                    #(
                        #field_idents,
                    )*
                })
            }
        }
    }
}

/// Generate a `From<PubVars>` implementation for converting `PubVars` to `Vec<Mutation>`.
fn impl_from_vars_for_vec_value(pub_vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents = field_idents(pub_vars);

    let mutations =
        field_idents
            .enumerate()
            .map(|(index, field_ident)| -> proc_macro2::TokenStream {
                syn::parse_quote! {
                    mutations.push(pint_abi::types::essential::solution::Mutation {
                        key: [#index as i64].to_vec(),
                        value: pint_abi::encode(&pub_vars.#field_ident),
                    });
                }
            });

    syn::parse_quote! {
        impl From<PubVars> for Vec<pint_abi::types::essential::solution::Mutation> {
            fn from(pub_vars: PubVars) -> Self {
                let mut mutations: Vec<pint_abi::types::essential::solution::Mutation> = vec![];
                #(#mutations)*
                mutations
            }
        }
    }
}

fn impl_try_from_values_for_vars(pub_vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents: Vec<_> = field_idents(pub_vars).collect();
    syn::parse_quote! {
        impl<'a> core::convert::TryFrom<&'a [pint_abi::types::essential::solution::Mutation]> for PubVars {
            type Error = PubVarDecodeError;
            fn try_from(mut mutations: &[pint_abi::types::essential::solution::Mutation]) -> Result<Self, Self::Error> {
                const NOT_ENOUGH_MUTATIONS: &str = "not enough mutations";
                let mut mutations = mutations.iter();
                #(
                    let mutation = mutations.next().ok_or_else(|| {
                        PubVarDecodeError {
                            field: PubVarDecodeFieldError::#field_idents,
                            err: NOT_ENOUGH_MUTATIONS.to_string(),
                        }
                    })?;
                    let #field_idents = pint_abi::decode(&mutation.value).map_err(|e| {
                        PubVarDecodeError {
                            field: PubVarDecodeFieldError::#field_idents,
                            err: format!("{e}"),
                        }
                    })?;
                )*
                Ok(Self {
                    #(
                        #field_idents,
                    )*
                })
            }
        }
    }
}

/// A declaration for the `PubVarDecodeError` enum for the `Decode` impl.
fn decode_error_struct() -> syn::ItemStruct {
    syn::parse_quote! {
        /// An error type for the [`PubVars`] [`pint_abi::Decode`] implementation.
        #[derive(Debug)]
        pub struct PubVarDecodeError {
            /// The field that failed to be decoded.
            pub field: PubVarDecodeFieldError,
            /// The `Display` formatted error message produced by decoding.
            pub err: String,
        }
    }
}

/// Generate a type describing which field failed to decode.
fn decode_field_error_enum(pub_vars: &[VarABI]) -> syn::ItemEnum {
    let field_idents = field_idents(pub_vars);
    syn::parse_quote! {
        /// A type describing which field failed to decode.
        #[derive(Debug)]
        #[allow(non_camel_case_types)]
        pub enum PubVarDecodeFieldError {
            #(
                #field_idents,
            )*
        }
    }
}

/// An implementation of `Display` for `PubVarDecodeError`.
fn decode_error_impl_display() -> syn::ItemImpl {
    syn::parse_quote! {
        impl core::fmt::Display for PubVarDecodeError {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "failed to decode field `{:?}`: {}", self.field, self.err)
            }
        }
    }
}

/// An implementation of `Error` for `PubVarDecodeError`.
fn decode_error_impl_error() -> syn::ItemImpl {
    syn::parse_quote! {
        impl std::error::Error for PubVarDecodeError {}
    }
}

/// Generate an enum for the `PubVarDecodeError` type.
fn decode_error_items(pub_vars: &[VarABI]) -> Vec<syn::Item> {
    vec![
        decode_error_struct().into(),
        decode_field_error_enum(pub_vars).into(),
        decode_error_impl_display().into(),
        decode_error_impl_error().into(),
    ]
}

/// The struct decl and impls for the decision variables `PubVars` type.
pub(crate) fn items(pub_vars: &[VarABI]) -> Vec<syn::Item> {
    let mut items = vec![
        struct_decl(pub_vars).into(),
        impl_encode(pub_vars).into(),
        impl_decode(pub_vars).into(),
        impl_from_vars_for_vec_value(pub_vars).into(),
        impl_try_from_values_for_vars(pub_vars).into(),
    ];
    items.extend(decode_error_items(pub_vars));
    items
}
