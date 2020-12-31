use proc_macro::TokenStream;
use syn::*;
use syn::spanned::Spanned;
use syn::export::TokenStream2;
use proc_macro2::{Ident, Span, Group};
use quote::{quote, quote_spanned, ToTokens};

#[proc_macro_derive(ReadWrite, attributes(tag_type, tag))]
pub fn derive_readwrite(item: TokenStream) -> TokenStream {
    //println!("input: \"{}\"", item);
    let input = syn::parse_macro_input!(item as DeriveInput);
    let res = derive_readwrite_inner(input).unwrap_or_else(|e| e.into_compile_error());
    //println!("res: \"{}\"", res.to_string());
    res.into()
}


fn derive_readwrite_inner(mut input: DeriveInput) -> Result<proc_macro2::TokenStream> {
    fn assert_type<T: ToTokens + Spanned>(ty: &T) -> TokenStream2 {
        let span = ty.span();
        quote_spanned! { span =>
            <#ty as crate::ReadWrite>
        }
    }
    let generics = &input.generics;
    let name = &input.ident;
    match &input.data {
        Data::Struct(s) => {
            // just read all fields
            // TODO header
            // TODO vec length type specification
            let field_names = s.fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
            let field_types = s.fields.iter().map(|f| &f.ty).map(assert_type).collect::<Vec<_>>();
            let construct = match s.fields {
                Fields::Named(_) => {
                    quote! {
                        Self {
                            #(
                                #field_names: #field_types::read_from(reader)?
                            ),*
                        }
                    }
                }
                Fields::Unnamed(_) => {
                    quote! { Self( #(#field_types::read_from(reader)?),* ) }
                }
                Fields::Unit => {
                    quote! { Self }
                }
            };

            Ok(quote! {
                impl #generics crate::ReadWrite for #name #generics {
                        fn read_from<Reader: std::io::Read>(reader: &mut Reader) -> crate::error::Result<Self> {
                            Ok(#construct)
                        }
                        fn write_to<Writer: std::io::Write>(&self, writer: &mut Writer) -> crate::error::Result<()> {
                            #(
                                #field_types::write_to(&self.#field_names, writer)?;
                            )*
                            Ok(())
                        }
                    }
            })
        }
        Data::Enum(e) => {
            // TODO vec length type specification
            let (idx, tag_type): (_, Ident) = input.attrs
                .iter()
                .enumerate()
                .find(|(_idx, a)| a.path.is_ident("tag_type"))
                .ok_or_else(|| Error::new(input.span(), "Couldn't find tag_type attribute"))
                .and_then(|(idx, a)| Ok((idx, parse2(parse2::<Group>(a.tokens.clone())?.stream())?)) )?;
            input.attrs.remove(idx);
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
                    if &str != "0" {
                        str = str.chars().rev().collect();
                        if str.as_bytes()[0] == b'0' {
                            str.remove(0);
                        }
                        str.insert(0, '-');
                    }
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
                    str = str.chars().rev().collect();
                    if str.as_bytes()[0] == b'0' {
                        str.insert(0, '1');
                    }
                    LitInt::new(&str, litint.span())
                }
            }
            let tags = variants
                .iter()
                .scan(LitInt::new("-1", Span::call_site()), |prev_discr, v| {
                    let lit = v.attrs.iter().find(|&a| a.path.is_ident("tag"))
                        .and_then(|a| parse2::<Group>(a.tokens.clone()).ok().as_ref().map(Group::stream).map(parse2).map(Result::ok).flatten() )
                        .or_else(|| v.discriminant.clone().and_then(|(_, e)| if let Expr::Lit(ExprLit { attrs:_, lit: Lit::Int(l) }) = e { Some(l) } else { None }))
                        .unwrap_or_else(|| add_one(prev_discr.clone()));
                    *prev_discr = lit.clone();
                    Some(lit)
                }).collect::<Vec<_>>();
            let variant_fields_idents = variants.iter().map(|v| match &v.fields {
                Fields::Named(FieldsNamed { brace_token: _, named }) => {
                    named.iter().map(|f| f.ident.clone().unwrap() ).collect::<Vec<_>>()
                }
                Fields::Unnamed(v) => {
                    fn num_to_str(mut n: usize) -> Ident {
                        let map = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".as_bytes(); // using all caps to avoid clash with keywords
                        let size = map.len();
                        let mut str = String::new();
                        loop {
                            let m = n % size;
                            n /= size;
                            str.push(map[m] as char);
                            if n == 0 {
                                break
                            }
                        }
                        Ident::new(&str, Span::call_site())
                    }
                    (0..v.unnamed.len()).map(num_to_str).collect()
                }
                Fields::Unit => { Vec::new() }
            }).collect::<Vec<_>>();
            let variant_match_arms = variants.iter().zip(variant_fields_idents.iter()).map(|(v, idents)| {
                let variant_ident = &v.ident;
                match &v.fields {
                    Fields::Named(_) => { quote! { Self::#variant_ident{ #(ref #idents),* } => } }
                    Fields::Unnamed(_) => { quote! { Self::#variant_ident( #(ref #idents),* ) => } }
                    Fields::Unit => { quote! { Self::#variant_ident => } }
                }
            });
            let variant_constructs = variants.iter().zip(variant_fields_idents.iter()).map(|(v,idents)| {
                let variant_ident = &v.ident;
                match &v.fields {
                    Fields::Named(_) => { quote! { Self::#variant_ident { #(#idents),* } } }
                    Fields::Unnamed(_) => { quote! { Self::#variant_ident( #(#idents),* ) } }
                    Fields::Unit => { quote! { Self::#variant_ident } }
                }
            });
            let variant_write_bodies = variant_fields_idents.iter().map(|idents| {
                quote! {
                    #(
                        #idents.write_to(writer)?;
                    )*
                }
            });
            let variant_read_bodies = variants.iter().zip(variant_fields_idents.iter()).map(|(v, idents)| {
                let types = v.fields.iter().map(|f| &f.ty).map(assert_type);
                quote! {
                    #(
                        let #idents = #types::read_from(reader)?;
                    )*
                }
            });
            let tag_ty = assert_type(&tag_type);
            Ok(quote! {
                    impl #generics crate::ReadWrite for #name #generics {
                        fn read_from<Reader: std::io::Read>(reader: &mut Reader) -> crate::error::Result<Self> {
                            let tag = #tag_ty::read_from(reader)?;
                            match tag {
                                #(
                                    #tags => {
                                        #variant_read_bodies
                                        Ok(#variant_constructs)
                                    }
                                )*
                                _ => {
                                    Err(crate::error::Error::Unrecognized("tag", tag.to_string()))
                                }
                            }
                        }
                        fn write_to<Writer: std::io::Write>(&self, writer: &mut Writer) -> crate::error::Result<()> {
                            match self {
                                #(
                                    #variant_match_arms {
                                        #tag_ty::write_to(&#tags, writer)?;
                                        #variant_write_bodies
                                        Ok(())
                                    }
                                )*
                            }
                        }
                    }
                })
        }
        Data::Union(_) => {
            Err(Error::new(input.span(), "This macro should not be used on a union type"))
        }
    }
}