use proc_macro::TokenStream;
use syn::*;
use syn::spanned::Spanned;
use syn::export::ToTokens;
use proc_macro2::{Ident, Span};
use std::ops::Deref;
use syn::parse::{Parse, ParseStream};
use quote::quote;


#[proc_macro_derive(ReadWrite)]
pub fn derive_readwrite(item: TokenStream) -> TokenStream {
    println!("input: \"{}\"", item);
    let input = syn::parse_macro_input!(item as DeriveInput);
    derive_readwrite_inner(input).unwrap_or_else(|e| e.into_compile_error()).into()
}

fn derive_readwrite_inner(input: DeriveInput) -> Result<proc_macro2::TokenStream> {
    match &input.data {
        Data::Struct(_) => {
            unimplemented!()
        }
        Data::Enum(e) => {
            let tag_type: Ident = input.attrs
                .iter()
                .find(|&a| a.path.is_ident("tag_type"))
                .ok_or_else(|| Error::new(input.span(), "Couldn't find tag_type attribute"))
                .and_then(|a| parse2(a.tokens.clone()) )?;

            let generics = &input.generics;
            let name = &input.ident;
            let variants = &e.variants;
            fn add_one(litint: LitInt) -> LitInt {
                if litint.base10_digits().starts_with('-') {
                    let mut str: String = litint.base10_digits()[1..].chars().rev().scan(true, |b, c| {
                        if *b {
                            if c == '0' {
                                Some('9')
                            } else {
                                *b = false;
                                Some((c as u8 - 1) as char)
                            }
                        } else {
                            Some(c)
                        }
                    }).collect();
                    if str.as_bytes()[0] == b'0' {
                        str.remove(0);
                    }
                    str.insert(0, '-');
                    LitInt::new(&str, litint.span())
                } else {
                    let mut str: String = litint.base10_digits().chars().rev().scan(true, |b, c| {
                        if *b {
                            if c == '9' {
                                Some('0')
                            } else {
                                *b = false;
                                Some((c as u8 + 1) as char)
                            }
                        } else {
                            Some(c)
                        }
                    }).collect();
                    if str.as_bytes()[0] == b'0' {
                        str.insert(0, '1');
                    }
                    LitInt::new(&str, litint.span())
                }
            }
            let tags: Vec<_> = variants
                .iter()
                .scan(LitInt::new("0", Span::call_site()), |prev_discr, v| {
                    let lit = v.attrs.iter().find(|&a| a.path.is_ident("tag"))
                        .and_then(|a| parse2::<LitInt>(a.tokens.clone()).ok() )
                        .or_else(|| v.discriminant.clone().and_then(|(_, e)| if let Expr::Lit(ExprLit { attrs:_, lit: Lit::Int(l) }) = e { Some(l) } else { None }))
                        .unwrap_or_else(|| add_one(prev_discr.clone()));
                    *prev_discr = lit.clone();
                    Some(lit)
                }).collect();
            Ok(quote! {
                    #input
                    impl #generics coffer::ReadWrite for #name #generics where #name: Sized {
                            fn read_from<T: Read>(reader: &mut T) -> Result<AccessFlags> {

                            }

                            fn write_to<T: Write>(&self, writer: &mut T) -> Result<()> {
                                match self {

                                }
                                Ok(())
                            }
                    }
                })
        }
        Data::Union(_) => {
            Err(Error::new(input.span(), "This macro should not be used on a union type"))
        }
    }
}