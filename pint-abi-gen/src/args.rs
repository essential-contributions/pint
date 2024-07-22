//! A type and `Parse` implementation for the args provided to the `from_file!` macro.

use syn::parse::{Parse, ParseStream};

/// Arguments provided to the `from_file!` macro.
pub(crate) struct FromFile {
    pub(crate) abi: syn::LitStr,
    pub(crate) contract: Option<syn::LitStr>,
}

impl Parse for FromFile {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut abi = None;
        let mut contract = None;
        while !input.is_empty() {
            let ident: syn::Ident = input.parse()?;
            input.parse::<syn::Token![:]>()?;
            match ident.to_string().as_str() {
                "abi" => {
                    if abi.is_some() {
                        return Err(input.error("duplicate `abi` argument"));
                    }
                    abi = Some(input.parse()?);
                }
                "contract" => {
                    if contract.is_some() {
                        return Err(input.error("duplicate `contract` argument"));
                    }
                    contract = Some(input.parse()?);
                }
                _ => return Err(input.error("unexpected argument")),
            }
            if !input.is_empty() {
                input.parse::<syn::Token![,]>()?;
            }
        }
        let abi =
            abi.ok_or_else(|| syn::Error::new(input.span(), "missing required `abi` argument"))?;
        Ok(FromFile { abi, contract })
    }
}
