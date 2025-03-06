use crate::{strip_colons_prefix, ty_from_pint_ty};
use pint_abi_types::{TypeABI, UnionVariant};
use proc_macro2::Span;
use std::collections::BTreeSet;

/// Given the name of a union as a path and the level of the module we're currently building,
/// convert it to a `syn::Type`. The `mod_level` tells us how many `super`s we need to reach the
/// root module so that we can form a relative path to the Rust enum corresponding to the Pint
/// union.
pub(crate) fn ty_from_union(name: &str, mod_level: usize) -> syn::Type {
    let path = strip_colons_prefix(name)
        .split("::")
        .map(|ident| syn::Ident::new(ident, Span::call_site()))
        .collect::<Vec<_>>();

    let supers = (0..mod_level)
        .map(|_| syn::parse_quote! { super })
        .collect::<Vec<proc_macro2::TokenStream>>();

    if supers.is_empty() {
        syn::parse_quote! { #( #path )::* }
    } else {
        syn::parse_quote! { #( #supers )::*::#( #path )::* }
    }
}

/// This struct represents a Pint module that contains other Pint modules and some pint unions.
#[derive(Debug, PartialEq, Eq)]
struct PintModule {
    name: String,
    mods: Vec<PintModule>,
    level: usize,
    unions: Vec<(String, Vec<UnionVariant>)>,
}

impl PintModule {
    /// Produce a new `PintModule` given its name and its level
    fn new(name: String, level: usize) -> Self {
        Self {
            name,
            mods: vec![],
            level,
            unions: vec![],
        }
    }
}

/// Given a set of Pint unions, generates all the items required to describe all the corressponding
/// Rust enums as well implementations of traits `Encode` and `Decode`.
pub(crate) fn items_from_unions(
    unions: &BTreeSet<(Vec<String>, Vec<UnionVariant>)>,
) -> Vec<syn::Item> {
    items_from_module(&module_from_unions(unions))
}

/// Given a `PintModule`, generates all the items required to describe all the Pint unions in the
/// module as well as implementations of traits `Encode` and `Decode`.
fn items_from_module(module: &PintModule) -> Vec<syn::Item> {
    let mut items: Vec<syn::Item> = vec![];

    // Recursively process sub-modules
    items.extend(module.mods.iter().flat_map(items_from_module));

    // Process unions
    for (union_name, union_variants) in &module.unions {
        // Create the `enum` declaration
        let enum_name = syn::Ident::new(union_name, Span::call_site());
        let enum_variants = variants_from_union_variants(union_variants, module.level);
        items.push(syn::parse_quote! {
            #[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
            pub enum #enum_name {
                #(
                    #enum_variants
                ),*
            }
        });

        // Manually implement `Default` for the `enum` declaration
        items.push(impl_default_from_union(
            &enum_name,
            union_variants,
            module.level,
        ));

        // Implement `Encode` for the `enum` declaration
        items.push(impl_encode_from_union(&enum_name, union_variants));

        // Decode error struct
        items.extend(decode_error_struct(&enum_name));

        // Decode variant error enum
        items.push(decode_variant_error_enum(&enum_name, union_variants).into());

        // Decode error `impl Display`
        items.push(decode_error_impl_display(&enum_name).into());

        // Decode error `impl Error`
        items.push(decode_error_impl_error(&enum_name).into());

        // Implement `Decode` for the `enum` declaration
        items.push(impl_decode_from_union(
            &enum_name,
            union_variants,
            module.level,
        ));
    }

    // If there's a name for the mod, wrap the items in a module. Otherwise, they just live in the
    // root module
    if module.name.is_empty() {
        items
    } else {
        let mod_name = syn::Ident::new(&module.name, Span::call_site());
        vec![syn::parse_quote! {
            pub mod #mod_name {
                #(
                    #items
                )*
            }
        }]
    }
}

/// Generate a `PintModule` from a set of pint unions. The root module is named `""` and its
/// submodules are derived from the paths of the unions.
fn module_from_unions(unions: &BTreeSet<(Vec<String>, Vec<UnionVariant>)>) -> PintModule {
    let mut root = PintModule::new("".to_string(), 0);

    for (path, variants) in unions {
        let mut path = path.clone();
        let union_name = path
            .pop()
            .expect("path to union must have at least one ident");

        let mut current_mod = &mut root;
        for p in path {
            // First, check if the module already exists in a separate scope
            current_mod = if let Some(position) = current_mod.mods.iter().position(|m| m.name == p)
            {
                &mut current_mod.mods[position]
            } else {
                // Push a new module and get the last element in the mods vector
                current_mod
                    .mods
                    .push(PintModule::new(p.clone(), current_mod.level + 1));
                current_mod
                    .mods
                    .last_mut()
                    .expect("we just pushed to `current_mods.mods`!")
            };
        }

        current_mod
            .unions
            .push((union_name.clone(), variants.clone()));
    }

    root
}

/// Converts the given pint union variants to Rust enum variants.
fn variants_from_union_variants(variants: &[UnionVariant], mod_level: usize) -> Vec<syn::Variant> {
    variants
        .iter()
        .map(|UnionVariant { name, ty }| {
            let variant_name = variant_name_from_full_path(name);

            if let Some(ty) = ty {
                let ty = ty_from_pint_ty(ty, mod_level);
                syn::parse_quote!(
                    #variant_name ( #ty )
                )
            } else {
                syn::parse_quote!(
                    #variant_name
                )
            }
        })
        .collect()
}

/// Generates an implementation of the `Default` trait for an `enum`.
fn impl_default_from_union(
    enum_name: &syn::Ident,
    variants: &[UnionVariant],
    mod_level: usize,
) -> syn::Item {
    let default_variant = variants
        .first()
        .expect("every union in Pint has at least one variant!");
    let default_variant_name = variant_name_from_full_path(&default_variant.name);

    let default_expr: syn::Expr = match &default_variant.ty {
        Some(TypeABI::Optional(ref wrapped_ty)) => {
            // Need the turbofish operator here
            let wrapped_ty = ty_from_pint_ty(wrapped_ty, mod_level);
            syn::parse_quote! {
                #enum_name::#default_variant_name(Option::<#wrapped_ty>::default())
            }
        }
        Some(ty) => {
            // None of the other types are generic (so far)
            let ty = ty_from_pint_ty(ty, mod_level);
            syn::parse_quote! {
                #enum_name::#default_variant_name(#ty::default())
            }
        }
        None => syn::parse_quote! {
            #enum_name::#default_variant_name
        },
    };

    syn::parse_quote! {
        impl std::default::Default for #enum_name {
            fn default() -> Self {
                #default_expr
            }
        }
    }
}

/// Given an `enum` and its variants, produce an implementation of the `Encode` trait for it
fn impl_encode_from_union(enum_name: &syn::Ident, variants: &[UnionVariant]) -> syn::Item {
    let largest_variant_size = largest_variant_size(variants);

    let variant_matches: Vec<syn::Arm> = variants
        .iter()
        .enumerate()
        .map(|(tag, variant)| {
            // This is how many 0s we need based on the size of the variant. We pad with enough 0s
            // to reach the size of the largest variant.
            let padding =
                largest_variant_size - variants[tag].ty.as_ref().map_or(0, pint_abi_visit::ty_size);

            let encode_padding_code: Option<syn::Expr> = (padding > 0).then(|| {
                syn::parse_quote! {
                    for _ in 0..#padding {
                        pint_abi::Encode::encode(&0, w)?;
                    }
                }
            });

            let variant_name = variant_name_from_full_path(&variant.name);

            if variant.ty.is_some() {
                // encode the tag, then encode the value, then pad
                syn::parse_quote! {
                    #enum_name::#variant_name (__value) => {
                        pint_abi::Encode::encode(&(#tag as i64), w)?;
                        pint_abi::Encode::encode(__value, w)?;
                        #encode_padding_code
                    }
                }
            } else {
                // encode the tag, then pad
                syn::parse_quote! {
                    #enum_name::#variant_name => {
                        pint_abi::Encode::encode(&(#tag as i64), w)?;
                        #encode_padding_code
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    syn::parse_quote! {
        impl pint_abi::Encode for #enum_name {
            fn encode<W: pint_abi::Write>(&self, w: &mut W) -> Result<(), W::Error> {
                match self {
                    #(#variant_matches),*
                }
                Ok(())
            }
        }
    }
}

/// Given an `enum` and its variants, produce an implementation of the `Decode` trait for it
fn impl_decode_from_union(
    enum_name: &syn::Ident,
    variants: &[UnionVariant],
    mod_level: usize,
) -> syn::Item {
    let largest_variant_size = largest_variant_size(variants);

    let decode_error_name =
        syn::Ident::new(&format!("{}DecodeError", enum_name), Span::call_site());
    let decode_variant_error_name = syn::Ident::new(
        &format!("{}DecodeVariantError", enum_name),
        Span::call_site(),
    );

    let mut variant_matches: Vec<syn::Arm> = variants
        .iter()
        .enumerate()
        .map(|(tag, variant)| {
            // This is how many 0s we need based on the size of the variant. We pad with enough 0s
            // to reach the size of the largest variant.
            let padding =
                largest_variant_size - variants[tag].ty.as_ref().map_or(0, pint_abi_visit::ty_size);

            let variant_name = variant_name_from_full_path(&variant.name);

            let decode_padding_code: Option<syn::Expr> = (padding > 0).then(|| {
                syn::parse_quote! {
                    for _ in 0..#padding {
                        <pint_abi::types::essential::Word>::decode(r).map_err(|e|
                            #decode_error_name::VariantError(
                                #decode_variant_error_name::#variant_name,
                                format!("{e}")
                            )
                        )?;
                    }
                }
            });

            if let Some(ty) = &variants[tag].ty {
                // Decode the value then decode the padding if needed then return the enum
                // expression
                let ty = ty_from_pint_ty(ty, mod_level);
                syn::parse_quote! {
                    #tag => {
                        let value = <#ty>::decode(r).map_err(|e|
                            #decode_error_name::VariantError(
                                #decode_variant_error_name::#variant_name,
                                format!("{e}")
                            )
                        )?;
                        #decode_padding_code
                        #enum_name::#variant_name(value)
                    }
                }
            } else {
                // Just decode the padding then return the enum expression
                syn::parse_quote! {
                    #tag => {
                        #decode_padding_code
                        #enum_name::#variant_name
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    // Catch-all match arm. Should be unreachable!
    variant_matches.push(syn::parse_quote! {
        _ => unreachable!()
    });

    syn::parse_quote! {
        impl pint_abi::Decode for #enum_name {
            type Error = #decode_error_name;
            fn decode<R: pint_abi::Read>(r: &mut R) -> Result<Self, Self::Error> {
                // Decode the tag then decode the value, if present, then decode the padding if
                // needed
                let tag = <pint_abi::types::essential::Word>::decode(r).map_err(|e|
                    #decode_error_name::TagError(format!("{e}"))
                )?;
                Ok(match tag as usize {
                    #(#variant_matches),*
                })
            }
        }
    }
}

/// A declaration for the `{enum_name}DecodeError` enum for the `Decode` impl.
fn decode_error_struct(enum_name: &syn::Ident) -> Vec<syn::Item> {
    let mut items = vec![];

    let decode_error_name =
        syn::Ident::new(&format!("{}DecodeError", enum_name), Span::call_site());
    let decode_variant_error_name = syn::Ident::new(
        &format!("{}DecodeVariantError", enum_name),
        Span::call_site(),
    );

    items.push(syn::parse_quote! {
        /// An error type for the enum [`pint_abi::Decode`] implementation.
        #[derive(Debug)]
        pub enum #decode_error_name {
            TagError(String),
            VariantError(#decode_variant_error_name, String),
        }
    });

    items
}

/// Generate a type describing which variant failed to decode.
fn decode_variant_error_enum(enum_name: &syn::Ident, variants: &[UnionVariant]) -> syn::ItemEnum {
    let variant_idents = variants
        .iter()
        .map(|variant| variant_name_from_full_path(&variant.name))
        .collect::<Vec<syn::Ident>>();

    let decode_variant_error_name = syn::Ident::new(
        &format!("{}DecodeVariantError", enum_name),
        Span::call_site(),
    );

    syn::parse_quote! {
        /// A type describing which variant failed to decode.
        #[derive(Debug)]
        #[allow(non_camel_case_types)]
        pub enum #decode_variant_error_name {
            #(
                #variant_idents,
            )*
        }
    }
}

/// An implementation of `Display` for `{enum_name}DecodeError`.
fn decode_error_impl_display(enum_name: &syn::Ident) -> syn::ItemImpl {
    let decode_error_name =
        syn::Ident::new(&format!("{}DecodeError", enum_name), Span::call_site());

    syn::parse_quote! {
        impl core::fmt::Display for #decode_error_name {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                match self {
                    Self::TagError(err) => write!(f, "failed to decode variant: {}", err),
                    Self::VariantError(variant, err) =>
                        write!(f, "failed to decode variant `{:?}`: {}", variant, err),
                }
            }
        }
    }
}

/// An implementation of `Error` for `{enum_name}DecodeError`.
fn decode_error_impl_error(enum_name: &syn::Ident) -> syn::ItemImpl {
    let decode_error_name =
        syn::Ident::new(&format!("{}DecodeError", enum_name), Span::call_site());

    syn::parse_quote! {
        impl std::error::Error for #decode_error_name {}
    }
}

/// Extract a variant name as `syn::Ident` from a path to a union variant
fn variant_name_from_full_path(path: &str) -> syn::Ident {
    syn::Ident::new(
        path.rsplit("::")
            .next()
            .expect("last ident representing the variant name must exist"),
        Span::call_site(),
    )
}

/// Compute the size of the largest variant given a list of `UnionVariant`s
fn largest_variant_size(variants: &[UnionVariant]) -> usize {
    variants
        .iter()
        .filter_map(|variant| variant.ty.as_ref())
        .map(pint_abi_visit::ty_size)
        .max()
        .unwrap_or_default()
}

/// Given a `TypeABI`, collect a set of all the unions that the type depends on, including type
/// itself. The unions are collected in a `BTreeSet`. Each union is represented using:
/// 1. a `Vec<String>` representing the path to the union.
/// 2. a `Vec<UnionVariant>` representing the variants of the union.
pub(crate) fn collect_unions(
    ty: &TypeABI,
    unions: &mut BTreeSet<(Vec<String>, Vec<UnionVariant>)>,
) {
    match ty {
        TypeABI::Bool | TypeABI::Int | TypeABI::Real | TypeABI::String | TypeABI::B256 => {}

        TypeABI::Optional(ty) => collect_unions(ty, unions),

        TypeABI::Tuple(fields) => {
            fields
                .iter()
                .for_each(|field| collect_unions(&field.ty, unions));
        }

        TypeABI::Array { ty, .. } => collect_unions(ty, unions),

        TypeABI::Union { name, variants } => {
            unions.insert((
                strip_colons_prefix(name)
                    .split("::")
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>(),
                variants.clone(),
            ));
            variants
                .iter()
                .filter_map(|variant| variant.ty.as_ref())
                .for_each(|ty| collect_unions(ty, unions));
        }

        TypeABI::Map { ty_from, ty_to } => {
            collect_unions(ty_from, unions);
            collect_unions(ty_to, unions);
        }
    }
}
