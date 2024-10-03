//! Struct and impls for the decision variables `Vars` type.

use crate::utils::{field_idents, fields};
use pint_abi_types::VarABI;

/// Generate a struct for an predicate's decision variables.
fn struct_decl(vars: &[VarABI]) -> syn::ItemStruct {
    let fields = fields(vars);
    syn::parse_quote! {
        /// The predicate's decision variables.
        #[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
        pub struct Vars {
            #(
                #fields
            ),*
        }
    }
}

/// Generate an `Encode` implementation for a `Vars` type.
fn impl_encode(vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents = field_idents(vars);
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

/// Generate a `Decode` implementation for a `Vars` type.
fn impl_decode(vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents: Vec<_> = field_idents(vars).collect();
    syn::parse_quote! {
        impl pint_abi::Decode for Vars {
            type Error = DecodeError;
            fn decode<R: pint_abi::Read>(r: &mut R) -> Result<Self, Self::Error> {
                #(
                    let #field_idents = <_>::decode(r).map_err(|e| {
                        DecodeError {
                            field: DecodeFieldError::#field_idents,
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

/// Generate a `From<Vars>` implementation for converting `Vars` to `Vec<Value>`.
fn impl_from_vars_for_vec_value(vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents = field_idents(vars);
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

fn impl_try_from_values_for_vars(vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents: Vec<_> = field_idents(vars).collect();
    syn::parse_quote! {
        impl<'a> core::convert::TryFrom<&'a [pint_abi::types::essential::Value]> for Vars {
            type Error = DecodeError;
            fn try_from(mut values: &[pint_abi::types::essential::Value]) -> Result<Self, Self::Error> {
                const NOT_ENOUGH_VALUES: &str = "not enough values";
                let mut values = values.iter();
                #(
                    let value = values.next().ok_or_else(|| {
                        DecodeError {
                            field: DecodeFieldError::#field_idents,
                            err: NOT_ENOUGH_VALUES.to_string(),
                        }
                    })?;
                    let #field_idents = pint_abi::decode(value).map_err(|e| {
                        DecodeError {
                            field: DecodeFieldError::#field_idents,
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

/// A declaration for the `DecodeError` enum for the `Decode` impl.
fn decode_error_struct() -> syn::ItemStruct {
    syn::parse_quote! {
        /// An error type for the [`Vars`] [`pint_abi::Decode`] implementation.
        #[derive(Debug)]
        pub struct DecodeError {
            /// The field that failed to be decoded.
            pub field: DecodeFieldError,
            /// The `Display` formatted error message produced by decoding.
            pub err: String,
        }
    }
}

/// Generate a type describing which field failed to decode.
fn decode_field_error_enum(vars: &[VarABI]) -> syn::ItemEnum {
    let field_idents = field_idents(vars);
    syn::parse_quote! {
        /// A type describing which field failed to decode.
        #[derive(Debug)]
        #[allow(non_camel_case_types)]
        pub enum DecodeFieldError {
            #(
                #field_idents,
            )*
        }
    }
}

/// An implementation of `Display` for `DecodeError`.
fn decode_error_impl_display() -> syn::ItemImpl {
    syn::parse_quote! {
        impl core::fmt::Display for DecodeError {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "failed to decode field `{:?}`: {}", self.field, self.err)
            }
        }
    }
}

/// An implementation of `Error` for `DecodeError`.
fn decode_error_impl_error() -> syn::ItemImpl {
    syn::parse_quote! {
        impl std::error::Error for DecodeError {}
    }
}

/// Generate an enum for the `DecodeError` type.
fn decode_error_items(vars: &[VarABI]) -> Vec<syn::Item> {
    vec![
        decode_error_struct().into(),
        decode_field_error_enum(vars).into(),
        decode_error_impl_display().into(),
        decode_error_impl_error().into(),
    ]
}

/// The struct decl and impls for the decision variables `Vars` type.
pub(crate) fn items(vars: &[VarABI]) -> Vec<syn::Item> {
    let mut items = vec![
        struct_decl(vars).into(),
        impl_encode(vars).into(),
        impl_decode(vars).into(),
        impl_from_vars_for_vec_value(vars).into(),
        impl_try_from_values_for_vars(vars).into(),
    ];
    items.extend(decode_error_items(vars));
    items
}
