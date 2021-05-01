/*
 *     This file is part of Coffer.
 *
 *     Coffer is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     Coffer is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with Coffer. (LICENSE.md)  If not, see <https://www.gnu.org/licenses/>.
 */

use proc_macro2::{Group, Ident, Span, TokenStream as TokenStream2};
use quote::{quote, quote_spanned, ToTokens};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;

pub(crate) fn generics(input: &DeriveInput) -> (TokenStream2, TokenStream2) {
    let generics = &input.generics;
    let params = &generics.params;
    let where_clause = &generics.where_clause;
    (
        if params.is_empty() {
            Default::default()
        } else {
            quote! { <#params> }
        },
        quote! { #where_clause },
    )
}
pub(crate) fn attr_enum(input: DeriveInput) -> Result<TokenStream2> {
    let (generics, where_c) = generics(&input);
    let span = input.span();
    let ident = input.ident;

    if let Data::Enum(e) = input.data {
        let raw_variant = &e
            .variants
            .iter()
            .find(|v| {
                v.attrs
                    .iter()
                    .any(|a| a.path.to_token_stream().to_string() == "raw_variant")
            })
            .ok_or_else(|| {
                Error::new(
                    e.variants.span(),
                    "Expected raw variant annotated with #[raw_variant]",
                )
            })?
            .ident;
        let new_variants = e
            .variants
            .iter()
            .filter(|v| {
                !v.attrs
                    .iter()
                    .any(|a| a.path.to_token_stream().to_string() == "raw_variant")
            })
            .collect::<Vec<_>>();
        let attr_names = new_variants
            .iter()
            .map(|v| v.ident.to_string())
            .map(|s| quote! { #s })
            .collect::<Vec<_>>();
        let variant_fields_idents = new_variants
            .iter()
            .map(|v| &v.fields)
            .map(generate_idents_for_fields)
            .collect::<Vec<_>>();
        let variant_match_arms =
            new_variants
                .iter()
                .zip(variant_fields_idents.iter())
                .map(|(v, idents)| {
                    let variant_ident = &v.ident;
                    match &v.fields {
                        Fields::Named(_) => {
                            quote! { Self::#variant_ident{ #(ref #idents),* } => }
                        }
                        Fields::Unnamed(_) => {
                            quote! { Self::#variant_ident( #(ref #idents),* ) => }
                        }
                        Fields::Unit => {
                            quote! { Self::#variant_ident => }
                        }
                    }
                });
        let variant_constructs =
            new_variants
                .iter()
                .zip(variant_fields_idents.iter())
                .map(|(v, idents)| {
                    let variant_ident = &v.ident;
                    match &v.fields {
                        Fields::Named(_) => {
                            quote! { Self::#variant_ident { #(#idents),* } }
                        }
                        Fields::Unnamed(_) => {
                            quote! { Self::#variant_ident( #(#idents),* ) }
                        }
                        Fields::Unit => {
                            quote! { Self::#variant_ident }
                        }
                    }
                });
        let (v_r, v_w): (Vec<Vec<_>>, Vec<Vec<_>>) = new_variants
            .iter()
            .zip(variant_fields_idents.iter())
            .map(|(v, idents)| {
                v.fields
                    .iter()
                    .zip(idents.iter())
                    .map(|(f, i)| {
                        gen_read_and_write(
                            f,
                            i,
                            quote! { crate::ConstantPoolReadWrite },
                            quote! { cp,inner_reader },
                            quote! { cp,inner_writer },
                        )
                    })
                    .collect::<Result<Vec<_>>>()
            })
            .collect::<Result<Vec<Vec<_>>>>()?
            .into_iter()
            .map(|v| v.into_iter().unzip())
            .unzip();

        let estimated_sizes = new_variants.iter().map(|v| {
            v.attrs.iter().find_map(|a| {
                if a.path.to_token_stream().to_string() == "attr_normal_size" {
                    Some(parse2::<Group>(a.tokens.clone()).map(|g| g.stream()))
                } else {
                    None
                }
            })
        });
        let mut variant_write_bodies = vec![];
        for (tk, est) in v_w.iter().zip(estimated_sizes) {
            let def = {
                if let Some(r) = est {
                    let cap = r?;
                    quote! { let mut vec = Vec::with_capacipy(#cap); }
                } else {
                    quote! { let mut vec = Vec::new(); }
                }
            };
            let write = quote! {
                #def
                let mut __inner_writer = std::io::Cursor::new(&mut vec);
                let mut inner_writer = &mut __inner_writer;
                #(#tk)*
                u32::write_to(&(vec.len() as u32), writer)?;
                writer.write_all(&vec)?;
            };
            variant_write_bodies.push(write);
        }

        let variant_read_bodies = v_r.iter().zip(variant_fields_idents.iter()).zip(new_variants.iter().map(|v| &v.ident)).map(|((r,i), id)| {
            quote! {
                let len = u32::read_from(reader)?;
                let mut vec = vec![0; len as usize];
                reader.read_exact(&mut vec)?;
                let mut __inner_reader: &[u8] = vec.as_ref();
                let inner_reader = &mut __inner_reader;
                #(let #i = #r;)*
                if !inner_reader.is_empty() {
                    return Err(crate::error::Error::AttributeLength(len, len - (inner_reader.len() as u32), stringify!(#id)))
                }
            }
        });
        let res = quote! {
            impl #generics crate::ConstantPoolReadWrite for #ident #generics #where_c {
                fn read_from<C: crate::ConstantPoolReader, R: std::io::Read>(cp: &mut C, reader: &mut R) -> crate::Result<Self> {
                    let idx = u16::read_from(reader)?;
                    let attribute_name = cp.read_utf8(idx).ok_or_else(|| crate::error::Error::Invalid("attribute index", Into::into(idx.to_string())))?;
                    match attribute_name.as_ref() {
                        #(
                                    #attr_names => {
                                        #variant_read_bodies
                                        Ok(#variant_constructs)
                                    }
                        )*
                        _ => {
                                        let byte_size = u32::read_from(reader)?;
                                        let mut bytes = vec![0; byte_size as usize];
                                        reader.read_exact(&mut bytes)?;
                                        let raw_attr = crate::prelude::RawAttribute::__new(attribute_name, bytes);
                                        Ok(Self::#raw_variant(raw_attr))
                        }
                    }
                }
                fn write_to<C: crate::ConstantPoolWriter, W: std::io::Write>(&self, cp: &mut C, writer: &mut W) -> crate::Result<()> {
                    match self {
                        #(
                            #variant_match_arms {
                                u16::write_to(&cp.insert_utf8(#attr_names), writer)?;
                                #variant_write_bodies
                                Ok(())
                            }
                        )*
                        Self::#raw_variant(crate::prelude::RawAttribute { keep: true, ref name, ref inner }) => {
                            u16::write_to(&cp.insert_utf8(name.clone()), writer)?;
                            crate::write_to!(&(inner.len() as u32), writer)?;
                            writer.write_all(inner)?;
                            Ok(())
                        }
                        _ => {
                            Ok(())
                        }
                    }
                }
            }
        };
        Ok(res)
    } else {
        Err(Error::new(span, "Must be an enum."))
    }
}

pub(crate) fn generate_idents_for_fields(fields: &Fields) -> Vec<Ident> {
    match fields {
        Fields::Named(FieldsNamed {
            brace_token: _,
            named,
        }) => named
            .iter()
            .map(|f| f.ident.clone().unwrap())
            .collect::<Vec<_>>(),
        Fields::Unnamed(v) => {
            pub(crate) fn num_to_str(mut n: usize) -> Ident {
                let map = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".as_bytes(); // using all caps to avoid clash with keywords
                let size = map.len();
                let mut str = String::new();
                loop {
                    let m = n % size;
                    n /= size;
                    str.push(map[m] as char);
                    if n == 0 {
                        break;
                    }
                }
                Ident::new(&str, Span::call_site())
            }
            (0..v.unnamed.len()).map(num_to_str).collect()
        }
        Fields::Unit => Vec::new(),
    }
}
pub(crate) fn add_one(litint: LitInt) -> LitInt {
    if litint.base10_digits().starts_with('-') {
        let mut str: String = litint.base10_digits()[1..]
            .chars()
            .rev()
            .scan(true, |b, c| {
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
            })
            .collect();
        if &str != "0" {
            str = str.chars().rev().collect();
            if str.as_bytes()[0] == b'0' {
                str.remove(0);
            }
            str.insert(0, '-');
        }
        LitInt::new(&str, litint.span())
    } else {
        let mut str: String = litint
            .base10_digits()
            .chars()
            .rev()
            .scan(true, |b, c| {
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
            })
            .collect();
        str = str.chars().rev().collect();
        if str.as_bytes()[0] == b'0' {
            str.insert(0, '1');
        }
        LitInt::new(&str, litint.span())
    }
}
pub(crate) fn gen_read_and_write<T: ToTokens>(
    f: &Field,
    receiver: &T,
    mut trait_type: TokenStream2,
    mut additional_fields_r: TokenStream2,
    mut additional_fields_w: TokenStream2,
) -> Result<(TokenStream2, TokenStream2)> {
    #[inline]
    pub(crate) fn rw_fncalls<T: ToTokens + Spanned>(
        ty: &T,
        traitty: TokenStream2,
    ) -> (TokenStream2, TokenStream2) {
        let sp = ty.span();
        (
            quote_spanned! { sp => <#ty as #traitty>::read_from },
            quote_spanned! { sp => <#ty as #traitty>::write_to },
        )
    }

    let span = f.span();
    let ty = &f.ty;
    let mut use_normal_rw = false;
    let mut str_type = None;
    let mut str_optional = false;
    for a in &f.attrs {
        let st = a.path.to_token_stream().to_string();
        match st.as_str() {
            "use_normal_rw" => {
                use_normal_rw = true;
            }
            "str_type" => {
                str_type = Some(parse2::<Group>(a.tokens.clone())?.stream().to_string());
            }
            "str_optional" => {
                str_optional = true;
            }
            _ => {}
        }
    }
    let reader: Ident;
    let writer: Ident;
    {
        let mut args_r: Punctuated<Ident, token::Comma> =
            syn::parse::Parser::parse2(Punctuated::parse_terminated, additional_fields_r.clone())?;
        let mut args_w: Punctuated<Ident, token::Comma> =
            syn::parse::Parser::parse2(Punctuated::parse_terminated, additional_fields_w.clone())?;
        reader = args_r.pop().unwrap().into_value();
        writer = args_w.pop().unwrap().into_value();
        if use_normal_rw {
            trait_type = quote! { crate::ReadWrite };
            additional_fields_r = reader.to_token_stream();
            additional_fields_w = writer.to_token_stream();
        }
    }

    let rw = |receiver: TokenStream2, ty: &Type| {
        let (read_fn, write_fn) = rw_fncalls(ty, trait_type);
        match (str_optional, str_type) {
            (true, None) => Ok((
                quote! {{
                let idx = u16::read_from(#reader)?;
                if idx == 0 {
                    None
                } else {
                    Some(cp.read_utf8(idx).ok_or_else(|| crate::error::Error::Invalid("constant pool entry index (expected UTF8)", Into::into(idx.to_string())))?)
                }}},
                quote! {
                    if let Some(s) = #receiver {
                        cp.insert_utf8(s.clone()).write_to(#writer)?;
                    } else {
                        0u16.write_to(#writer)?;
                    }
                },
            )),
            (true, Some(t)) => {
                let tag: u8 = match t.as_str() {
                    "String" => 8,
                    "Class" => 7,
                    "Module" => 19,
                    "Package" => 20,
                    _ => return Err(Error::new(f.span(), "Invalid String type")),
                };
                Ok((
                    quote! {{
                        let idx = u16::read_from(#reader)?;
                        if idx == 0 {
                            None
                        } else {
                            Some(cp.read_indirect_str(#tag, idx).ok_or_else(|| crate::error::Error::Invalid(concat!("constant pool entry index (expected ", #t, ")"), Into::into(idx.to_string())))?)
                        }
                    }},
                    quote! {
                        if let Some(s) = #receiver {
                            cp.insert_indirect_str(#tag, s.clone()).write_to(#writer)?;
                        } else {
                            0u16.write_to(#writer)?;
                        }
                    },
                ))
            }
            (false, Some(t)) => {
                let tag: u8 = match t.as_str() {
                    "String" => 8,
                    "Class" => 7,
                    "Module" => 19,
                    "Package" => 20,
                    _ => return Err(Error::new(f.span(), "Invalid String type")),
                };
                Ok((
                    quote! {{
                        let idx = u16::read_from(#reader)?;
                        cp.read_indirect_str(#tag, idx).ok_or_else(|| crate::error::Error::Invalid(concat!("constant pool entry index (expected ", #t, ")"), Into::into(idx.to_string())))?
                    }},
                    quote! {
                        cp.insert_indirect_str(#tag, #receiver.clone()).write_to(#writer)?;
                    },
                ))
            }
            (false, None) => Ok((
                quote! { #read_fn(#additional_fields_r)? },
                quote! { #write_fn(#receiver, #additional_fields_w)?; },
            )),
        }
    };
    if let Type::Path(p) = ty {
        let segment_last = p.path.segments.last().unwrap();
        if segment_last.ident == "Vec" {
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                colon2_token: _,
                lt_token: _,
                ref args,
                ..
            }) = segment_last.arguments
            {
                if let Some(GenericArgument::Type(ty)) = args.first() {
                    let vec_len_ty = f
                        .attrs
                        .iter()
                        .find(|a| a.path.to_token_stream().to_string() == "vec_len_type")
                        .ok_or_else(|| Error::new(span, "Missing vec_len_type attribute"))
                        .and_then(|attr| parse2::<Group>(attr.tokens.to_owned()))
                        .and_then(|g| parse2::<Ident>(g.stream()))?;

                    let (read_vec_len_fn, write_vec_len_fn) =
                        rw_fncalls(&vec_len_ty, quote! { crate::ReadWrite });
                    let (read, write) = rw(quote!(it), ty)?;
                    return Ok((
                        quote! {
                            {
                                let len = #read_vec_len_fn(#reader)?;
                                let mut vec = Vec::with_capacity(len as usize);
                                for _ in 0..len {
                                    vec.push(#read);
                                }
                                vec
                            }
                        },
                        quote! {
                            #write_vec_len_fn(&(#receiver.len() as #vec_len_ty), #writer)?;
                            for it in #receiver.iter() {
                                #write
                            }
                        },
                    ));
                }
            }
        }
    }
    rw(receiver.to_token_stream(), ty)
}
pub(crate) fn derive_readwrite_inner(
    mut input: DeriveInput,
    trait_ty: TokenStream2,
    fields_r: TokenStream2,
    fields_w: TokenStream2,
    rsig: TokenStream2,
    wsig: TokenStream2,
) -> Result<TokenStream2> {
    let (generics, where_c) = generics(&input);
    let name = &input.ident;
    match &input.data {
        Data::Struct(s) => {
            // just read all fields
            // TODO header
            let (r, w): (Vec<_>, Vec<_>) = s
                .fields
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    let ident = f
                        .ident
                        .as_ref()
                        .map(ToTokens::to_token_stream)
                        .unwrap_or_else(|| syn::Index::from(i).to_token_stream());
                    gen_read_and_write(
                        f,
                        &quote! { (&self.#ident) },
                        trait_ty.clone(),
                        fields_r.clone(),
                        fields_w.clone(),
                    )
                })
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .unzip();

            let construct = match s.fields {
                Fields::Named(_) => {
                    let idents = s.fields.iter().map(|f| f.ident.as_ref().unwrap());
                    quote! { Self { #(#idents: #r),* } }
                }
                Fields::Unnamed(_) => {
                    quote! { Self ( #(#r),* ) }
                }
                Fields::Unit => {
                    quote! { Self }
                }
            };

            Ok(quote! {
                impl #generics #trait_ty for #name #generics #where_c {
                        fn read_from#rsig -> crate::error::Result<Self> {
                            Ok(#construct)
                        }
                        fn write_to#wsig -> crate::error::Result<()> {
                            #( #w )*
                            Ok(())
                        }
                    }
            })
        }
        Data::Enum(e) => {
            let (idx, tag_type): (_, Ident) = input
                .attrs
                .iter()
                .enumerate()
                .find(|(_idx, a)| a.path.is_ident("tag_type"))
                .ok_or_else(|| Error::new(input.span(), "Couldn't find tag_type attribute"))
                .and_then(|(idx, a)| {
                    Ok((idx, parse2(parse2::<Group>(a.tokens.clone())?.stream())?))
                })?;
            input.attrs.remove(idx);
            let variants = &e.variants;

            let tags = variants
                .iter()
                .scan(LitInt::new("0", Span::call_site()), |fallback, v| {
                    let lit = v
                        .attrs
                        .iter()
                        .find(|&a| a.path.is_ident("tag"))
                        .and_then(|a| {
                            parse2::<Group>(a.tokens.clone())
                                .ok()
                                .as_ref()
                                .map(Group::stream)
                                .map(parse2)
                                .map(Result::ok)
                                .flatten()
                        })
                        .or_else(|| {
                            v.discriminant.clone().and_then(|(_, e)| {
                                if let Expr::Lit(ExprLit {
                                    attrs: _,
                                    lit: Lit::Int(l),
                                }) = e
                                {
                                    Some(l)
                                } else {
                                    None
                                }
                            })
                        })
                        .unwrap_or_else(|| fallback.clone());
                    *fallback = add_one(lit.clone());
                    Some(lit)
                })
                .collect::<Vec<_>>();
            let variant_fields_idents = variants
                .iter()
                .map(|v| &v.fields)
                .map(generate_idents_for_fields)
                .collect::<Vec<_>>();
            let variant_match_arms =
                variants
                    .iter()
                    .zip(variant_fields_idents.iter())
                    .map(|(v, idents)| {
                        let variant_ident = &v.ident;
                        match &v.fields {
                            Fields::Named(_) => {
                                quote! { Self::#variant_ident{ #(ref #idents),* } => }
                            }
                            Fields::Unnamed(_) => {
                                quote! { Self::#variant_ident( #(ref #idents),* ) => }
                            }
                            Fields::Unit => {
                                quote! { Self::#variant_ident => }
                            }
                        }
                    });
            let variant_constructs =
                variants
                    .iter()
                    .zip(variant_fields_idents.iter())
                    .map(|(v, idents)| {
                        let variant_ident = &v.ident;
                        match &v.fields {
                            Fields::Named(_) => {
                                quote! { Self::#variant_ident { #(#idents),* } }
                            }
                            Fields::Unnamed(_) => {
                                quote! { Self::#variant_ident( #(#idents),* ) }
                            }
                            Fields::Unit => {
                                quote! { Self::#variant_ident }
                            }
                        }
                    });
            let (v_r, v_w): (Vec<Vec<_>>, Vec<Vec<_>>) = variants
                .iter()
                .zip(variant_fields_idents.iter())
                .map(|(v, idents)| {
                    v.fields
                        .iter()
                        .zip(idents.iter())
                        .map(|(f, i)| {
                            gen_read_and_write(
                                f,
                                i,
                                trait_ty.clone(),
                                fields_r.clone(),
                                fields_w.clone(),
                            )
                        })
                        .collect::<Result<Vec<_>>>()
                })
                .collect::<Result<Vec<Vec<_>>>>()?
                .into_iter()
                .map(|v| v.into_iter().unzip())
                .unzip();

            let variant_write_bodies = v_w.iter().map(|tk| {
                quote! { #(#tk)* }
            });
            let variant_read_bodies = v_r.iter().zip(variant_fields_idents.iter()).map(|(r, i)| {
                quote! { #(let #i = #r;)* }
            });
            let tag_ty = {
                let span = tag_type.span();
                quote_spanned! { span =>
                    <#tag_type as crate::ReadWrite>
                }
            };
            Ok(quote! {
                impl #generics #trait_ty for #name #generics #where_c {
                    fn read_from#rsig -> crate::error::Result<Self> {
                        let tag = #tag_ty::read_from(reader)?;
                        match tag {
                            #(
                                #tags => {
                                    #variant_read_bodies
                                    Ok(#variant_constructs)
                                }
                            )*
                            _ => {
                                Err(crate::error::Error::Invalid("tag", Into::into(tag.to_string())))
                            }
                        }
                    }
                    fn write_to#wsig -> crate::error::Result<()> {
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
        Data::Union(_) => Err(Error::new(
            input.span(),
            "This macro should not be used on a union type",
        )),
    }
}
