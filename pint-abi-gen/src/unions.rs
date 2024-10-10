use crate::{strip_colons_prefix, ty_from_pint_ty, ModLevel};
use pint_abi_types::{TypeABI, UnionVariant};
use proc_macro2::Span;
use std::collections::BTreeSet;

/// Given the name of a union as a path and a `ModLevel`, convert it to a `syn::Type`.
pub(crate) fn ty_from_union(name: &str, mod_level: &ModLevel) -> syn::Type {
    let path = strip_colons_prefix(name)
        .split("::")
        .map(|ident| syn::Ident::new(ident, Span::call_site()))
        .collect::<Vec<_>>();

    match mod_level {
        ModLevel::Root => {
            // If we're in the Root module, go back as many time as needed to actually be in the
            // root. We do this using `super`
            let supers = (0..path.len() - 1)
                .map(|_| syn::parse_quote! { super })
                .collect::<Vec<proc_macro2::TokenStream>>();

            if supers.is_empty() {
                syn::parse_quote! { #( #path )::* }
            } else {
                syn::parse_quote! { #( #supers )::*::#( #path )::* }
            }
        }
        ModLevel::Predicate | ModLevel::Storage => {
            // Only one `super` needed to go back to the root module
            syn::parse_quote! { super::#( #path )::* }
        }
        ModLevel::Mutations | ModLevel::Keys => syn::parse_quote! {
            // Two `super` needed to go back to the root module
            super::super::#( #path )::*
        },
    }
}

/// This struct represents that contains other modules and some pint unions.
#[derive(Debug, PartialEq, Eq)]
struct Module {
    name: String,
    mods: Vec<Module>,
    unions: Vec<(String, Vec<UnionVariant>)>,
}

impl Module {
    fn new(name: String) -> Self {
        Self {
            name,
            mods: vec![],
            unions: vec![],
        }
    }
}

/// Given a set of unions, generates all the items required to describe all the corressponding
/// enums as well implementations of the `Encode` trait for them.
pub(crate) fn items_from_unions(
    unions: &BTreeSet<(Vec<String>, Vec<UnionVariant>)>,
) -> Vec<syn::Item> {
    items_from_module(&module_from_unions(unions))
}

/// Generate a `Module` from a set of pint unions. The root module is named `""` and its submodule
/// match the paths of the unions.
fn module_from_unions(unions: &BTreeSet<(Vec<String>, Vec<UnionVariant>)>) -> Module {
    let mut root = Module::new("".to_string());

    for (path, variants) in unions {
        let mut path = path.clone();
        let union_name = path.pop().expect("");

        let mut current_mod = &mut root;
        for p in path {
            // First, check if the module already exists in a separate scope
            current_mod = if let Some(position) = current_mod.mods.iter().position(|m| m.name == p)
            {
                &mut current_mod.mods[position]
            } else {
                // Push a new module and get the last element in the mods vector
                current_mod.mods.push(Module::new(p.clone()));
                current_mod.mods.last_mut().expect("")
            };
        }

        current_mod
            .unions
            .push((union_name.clone(), variants.clone()));
    }

    root
}

/// Given a `Module`, generates all the items required to describe all the unions in the module as
/// well as implementations of the `Encode` for them.
fn items_from_module(module: &Module) -> Vec<syn::Item> {
    let mut items: Vec<syn::Item> = vec![];

    // Recursively process sub-modules
    items.extend(module.mods.iter().flat_map(items_from_module));

    // Process unions
    for (union_name, union_variants) in &module.unions {
        // Create the `enum` declaration
        let enum_name = syn::Ident::new(union_name, Span::call_site());
        let enum_variants = variants_from_union_variants(union_variants);
        items.push(syn::parse_quote! {
            #[derive(Debug)]
            pub enum #enum_name {
                #(
                    #enum_variants
                ),*
            }
        });

        // implement `Encode` for the `enum` declaration
        items.push(impl_encode_from_union(&enum_name, union_variants));
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

/// Converts the given pint union variants to Rust enum variants.
fn variants_from_union_variants(variants: &[UnionVariant]) -> Vec<syn::Variant> {
    variants
        .iter()
        .map(|UnionVariant { name, ty }| {
            let variant_name = syn::Ident::new(
                name.rsplit("::")
                    .next()
                    .expect("last ident representing the variant name must exist"),
                Span::call_site(),
            );

            if let Some(ty) = ty {
                let ty = ty_from_pint_ty(ty, &ModLevel::Root);
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

/// Given an `enum` and its variants, produce an implementation of the `Encode` trait for it
fn impl_encode_from_union(enum_name: &syn::Ident, variants: &[UnionVariant]) -> syn::Item {
    let largest_variant_size = variants
        .iter()
        .filter_map(|variant| variant.ty.as_ref())
        .map(pint_abi_visit::ty_size)
        .max()
        .unwrap_or_default();

    let variant_matches: Vec<syn::Arm> = variants
        .iter()
        .enumerate()
        .map(|(tag, variant)| {
            // This is how many 0s we need based on the size of the variant. We pad with enough 0s
            // to reach the size of the largest variant.
            let padding =
                largest_variant_size - variants[tag].ty.as_ref().map_or(0, pint_abi_visit::ty_size);

            let padding_code: Option<syn::Expr> = (padding > 0).then(|| {
                syn::parse_quote! {
                    for _ in 0..#padding {
                        pint_abi::Encode::encode(&0, w)?;
                    }
                }
            });

            let variant_name = syn::Ident::new(
                variant
                    .name
                    .rsplit("::")
                    .next()
                    .expect("last ident representing the variant name must exist"),
                Span::call_site(),
            );

            if variant.ty.is_none() {
                // encode the tag, then pad
                syn::parse_quote! {
                    #enum_name::#variant_name => {
                        pint_abi::Encode::encode(&(#tag as i64), w)?;
                        #padding_code
                    }
                }
            } else {
                // encode the tag, the encode the value, then pad
                syn::parse_quote! {
                    #enum_name::#variant_name (__value) => {
                        pint_abi::Encode::encode(&(#tag as i64), w)?;
                        pint_abi::Encode::encode(__value, w)?;
                        #padding_code
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

/// Given a `TypeABI`, collect a set of all the unions that the type depends on, including type
/// itself. The unions are collected in a `BTreeSet`. Each union is represented using:
/// 1. a `Vec<String>` representing the path to the union.
/// 2. a `Vec<UnionVariant>` representing the variants of the union.
pub(crate) fn collect_unions(
    ty: &TypeABI,
    unions: &mut BTreeSet<(Vec<String>, Vec<UnionVariant>)>,
) {
    match ty {
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
        _ => {}
    }
}
