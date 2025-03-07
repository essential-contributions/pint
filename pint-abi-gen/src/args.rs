//! Struct and impls for the predicate arguments `Args` type.

use crate::utils::{field_idents, fields};
use pint_abi_types::ParamABI;

/// Generate a struct for the arguments of a predicate.
fn struct_decl(args: &[ParamABI]) -> syn::ItemStruct {
    let fields = fields(args, 1);
    syn::parse_quote! {
        /// The predicate's arguments.
        #[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
        pub struct Args {
            #(
                #fields
            ),*
        }
    }
}

/// Generate an `Encode` implementation for the `Args` type.
fn impl_encode(args: &[ParamABI]) -> syn::ItemImpl {
    let field_idents = field_idents(args);
    syn::parse_quote! {
        impl pint_abi::Encode for Args {
            fn encode<W: pint_abi::Write>(&self, w: &mut W) -> Result<(), W::Error> {
                #(
                    pint_abi::Encode::encode(&self.#field_idents, w).expect("cannot fail");
                )*
                Ok(())
            }
        }
    }
}

/// Generate a `Decode` implementation for the `Args` type.
fn impl_decode(args: &[ParamABI]) -> syn::ItemImpl {
    let field_idents: Vec<_> = field_idents(args).collect();
    syn::parse_quote! {
        impl pint_abi::Decode for Args {
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

/// Generate a `From<Args>` implementation for converting `Args` to `Vec<Value>`.
fn impl_from_args_for_vec_value(args: &[ParamABI]) -> syn::ItemImpl {
    let field_idents = field_idents(args);
    syn::parse_quote! {
        impl From<Args> for Vec<pint_abi::types::essential::Value> {
            fn from(args: Args) -> Self {
                let mut values: Vec<pint_abi::types::essential::Value> = vec![];
                #(
                    values.push(pint_abi::encode(&args.#field_idents));
                )*
                values
            }
        }
    }
}

fn impl_try_from_values_for_args(args: &[ParamABI]) -> syn::ItemImpl {
    let field_idents: Vec<_> = field_idents(args).collect();
    syn::parse_quote! {
        impl<'a> core::convert::TryFrom<&'a [pint_abi::types::essential::Value]> for Args {
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
        /// An error type for the [`Args`] [`pint_abi::Decode`] implementation.
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
fn decode_field_error_enum(args: &[ParamABI]) -> syn::ItemEnum {
    let field_idents = field_idents(args);
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
fn decode_error_items(args: &[ParamABI]) -> Vec<syn::Item> {
    vec![
        decode_error_struct().into(),
        decode_field_error_enum(args).into(),
        decode_error_impl_display().into(),
        decode_error_impl_error().into(),
    ]
}

/// The struct decl and impls for the predicate arguments `Args` type.
pub(crate) fn items(args: &[ParamABI]) -> Vec<syn::Item> {
    let mut items = vec![
        struct_decl(args).into(),
        impl_encode(args).into(),
        impl_decode(args).into(),
        impl_from_args_for_vec_value(args).into(),
        impl_try_from_values_for_args(args).into(),
    ];
    items.extend(decode_error_items(args));
    items
}
