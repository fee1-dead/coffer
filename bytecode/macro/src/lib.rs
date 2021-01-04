use proc_macro::TokenStream;
use syn::*;
use syn::spanned::Spanned;
use syn::export::TokenStream2;
use proc_macro2::{Ident, Span, Group};
use quote::{quote, quote_spanned, ToTokens};

#[proc_macro_derive(ConstantPoolReadWrite, attributes(tag_type, tag, attr_enum, raw_variant, attr_normal_size, use_normal_rw, indirect_str))]
pub fn derive_cp_readwrite(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as DeriveInput);
    let is_enum = input.attrs.iter().any(|a| a.path.to_token_stream().to_string() == "attr_enum");
    if is_enum {
        // Enum variants as Attribute names, also `attr_raw` creates an exhaustive match.
        attr_enum(input)
    } else {
        // Not attribute specific, act as a normal derive
        derive_readwrite_inner(
            input,
            quote! { crate::ConstantPoolReadWrite },
            quote! { cp,reader },
            quote! { cp,writer },
            quote! { <C: ConstantPoolReader, R: Read>(cp: &mut C, reader: &mut R) },
            quote! { <C: crate::ConstantPoolWriter, W: std::io::Write>(&self, cp: &mut C, writer: &mut W) })
    }.unwrap_or_else(Error::into_compile_error).into()
}
fn generics(input: &DeriveInput) -> (TokenStream2, TokenStream2) {
    let generics = &input.generics;
    let params = &generics.params;
    let where_clause = &generics.where_clause;
    (if params.is_empty() { Default::default() } else { quote! { <#params> } }, quote! { #where_clause })
}
fn attr_enum(input: DeriveInput) -> Result<TokenStream2> {
    let (generics, where_c) = generics(&input);
    let span = input.span();
    let ident = input.ident;

    if let Data::Enum(e) = input.data {
        let raw_variant = e.variants
            .iter()
            .find(|v| v.attrs.iter().any(|a| a.path.to_token_stream().to_string() == "raw_variant"))
            .ok_or_else(|| Error::new(e.variants.span(), "Expected raw variant annotated with #[raw_variant]"))?;
        let new_variants = e.variants.iter().filter(|v| !v.attrs.iter().any(|a| a.path.to_token_stream().to_string() == "raw_variant")).collect::<Vec<_>>();
        let attr_names = new_variants.iter().map(|v| v.ident.to_string()).map(|s| quote! { #s }).collect::<Vec<_>>();
        let variant_fields_idents = new_variants.iter().map(|v| &v.fields).map(generate_idents_for_fields).collect::<Vec<_>>();
        let variant_match_arms = new_variants.iter().zip(variant_fields_idents.iter()).map(|(v, idents)| {
            let variant_ident = &v.ident;
            match &v.fields {
                Fields::Named(_) => { quote! { Self::#variant_ident{ #(ref #idents),* } => } }
                Fields::Unnamed(_) => { quote! { Self::#variant_ident( #(ref #idents),* ) => } }
                Fields::Unit => { quote! { Self::#variant_ident => } }
            }
        });
        let variant_constructs = new_variants.iter().zip(variant_fields_idents.iter()).map(|(v,idents)| {
            let variant_ident = &v.ident;
            match &v.fields {
                Fields::Named(_) => { quote! { Self::#variant_ident { #(#idents),* } } }
                Fields::Unnamed(_) => { quote! { Self::#variant_ident( #(#idents),* ) } }
                Fields::Unit => { quote! { Self::#variant_ident } }
            }
        });
        let (v_r, v_w) : (Vec<Vec<_>>, Vec<Vec<_>>) = new_variants
            .iter()
            .zip(variant_fields_idents.iter())
            .map(|(v, idents)|
                v.fields
                    .iter()
                    .zip(idents.iter())
                    .map(|(f, i)| gen_read_and_write(f, i, quote! { crate::ConstantPoolReadWrite },
                                                     quote! { cp,inner_reader },
                                                     quote! { cp,inner_writer }))
                    .collect::<Result<Vec<_>>>())
            .collect::<Result<Vec<Vec<_>>>>()?
            .into_iter()
            .map(|v| v.into_iter().unzip())
            .unzip();

        let estimated_sizes = new_variants.iter().map(|v| v.attrs.iter().find_map(|a| if a.path.to_token_stream().to_string() == "attr_normal_size" {
            Some(parse2::<Group>(a.tokens.clone()).map(|g| g.stream()))
        } else { None }));
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
                let inner_writer = std::io::Cursor::new(&mut vec);
                #(#tk)*
                u32::write_to(&(vec.len() as u32), writer)?;
                writer.write_all(&vec)?;
            };
            variant_write_bodies.push(write);
        }

        let variant_read_bodies = v_r.iter().zip(variant_fields_idents.iter()).map(|(r,i)| {
            quote! {
                let len = u32::read_from(reader)?;
                let mut vec = Vec::with_capacity(len as usize);
                reader.read_exact(&mut vec)?;
                let mut inner_reader = &vec;
                #(let #i = #r;)*
                if !inner_reader.is_empty() {
                    return Err(crate::error::Error::AttributeLength(len, len - (inner_reader.len() as u64)))
                }
            }
        });
        let res = quote! {
            impl #generics crate::ConstantPollReadWrite for #ident #generics #where_c {
                fn read_from<C: crate::ConstantPoolReader, R: std::io::Read>(cp: &mut C, reader: &mut R) -> crate::Result<Self> {
                    let idx = u16::read_from(reader)?;
                    let attribute_name = cp.read_utf8(idx).ok_or_else(|| crate::error::Error::Invalid("attribute index", idx.to_string()))?;
                    match attribute_name.as_ref() {
                        #(
                                    #attr_names => {
                                        #variant_read_bodies
                                        Ok(#variant_constructs)
                                    }
                                    _ => {
                                        let byte_size = u32::read_from(reader)?;
                                        let bytes = Vec::with_capacity(byte_size as usize);
                                        reader.read_exact(&mut bytes)?;
                                        let raw_attr = crate::full::RawAttribute::__new(attribute_name, bytes);
                                        Ok(Self::#raw_variant(raw_attr))
                                    }
                        )*
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
                    }
                }
            }
        };
        Ok(res)
    } else {
        Err(Error::new(span, "Must be an enum, for a struct, use #[attr] instead."))
    }
}

#[proc_macro_derive(ReadWrite, attributes(tag_type, tag, vec_len_type))]
pub fn derive_readwrite(item: TokenStream) -> TokenStream {
    //println!("input: \"{}\"", item);
    let input = syn::parse_macro_input!(item as DeriveInput);
    let res = derive_readwrite_inner(input, quote! { crate::ReadWrite }, quote! { reader }, quote! { writer }, quote! { <Reader: std::io::Read>(reader: &mut Reader) }, quote! { <Writer: std::io::Write>(&self, writer: &mut Writer) }).unwrap_or_else(Error::into_compile_error);
    //println!("res: \"{}\"", res.to_string());
    res.into()
}

fn generate_idents_for_fields(fields: &Fields) -> Vec<Ident> {
    match fields {
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
    }
}
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
fn gen_read_and_write<T: ToTokens>(f: &Field, reciever: &T, trait_type: TokenStream2, additional_fields_r: TokenStream2, additional_fields_w: TokenStream2) -> Result<(TokenStream2, TokenStream2)> {
    #[inline]
    fn rw_fncalls<T: ToTokens + Spanned>(ty: &T, traitty: TokenStream2) -> (TokenStream2, TokenStream2) {
        let sp = ty.span();
        (quote_spanned! { sp => <#ty as #traitty>::read_from }, quote_spanned! { sp => <#ty as #traitty>::write_to })
    }
    let span = f.span();
    let ty = &f.ty;
    let use_normal_rw = f.attrs.iter().any(|a| a.path.to_token_stream().to_string() == "use_normal_rw");
    let use_trait: TokenStream2 = if use_normal_rw { quote! { crate::ReadWrite } } else { trait_type };
    if let Type::Path(p) = ty {
        let segment_last = p.path.segments.last().unwrap();
        if segment_last.ident == "Vec" {
            if let PathArguments::AngleBracketed( AngleBracketedGenericArguments { colon2_token: _, lt_token: _, ref args, .. }) = segment_last.arguments {
                if let Some(GenericArgument::Type(ty)) = args.first() {
                    let vec_len_ty = f.attrs
                        .iter()
                        .find(|a| a.path.to_token_stream().to_string() == "vec_len_type")
                        .ok_or_else(|| Error::new(span, "Missing vec_len_type attribute"))
                        .and_then(|attr| parse2::<Group>(attr.tokens.to_owned()))
                        .and_then(|g| parse2::<Ident>(g.stream()))?;
                    let (read_vec_len_fn, write_vec_len_fn) = rw_fncalls(&vec_len_ty, quote! { crate::ReadWrite });
                    let (read_fn, write_fn) = rw_fncalls(ty, use_trait);
                    return Ok((quote! {
                        {
                            let len = #read_vec_len_fn(reader)?;
                            let vec = Vec::with_capacity(len as usize);
                            for _ in 0..len {
                                vec.push(#read_fn(#additional_fields_r)?);
                            }
                            vec
                        }
                    }, quote! {
                        #write_vec_len_fn(&(#reciever.len() as #vec_len_ty), writer)?;
                        for it in #reciever.iter() {
                            #write_fn(&it, #additional_fields_w)?;
                        }
                    }))
                }
            }
        }
    }
    let (read_fn, write_fn) = rw_fncalls(ty, use_trait);
    Ok((quote! { #read_fn(#additional_fields_r)? }, quote! { #write_fn(#reciever, #additional_fields_w)?; }))
}
fn derive_readwrite_inner(mut input: DeriveInput, trait_ty: TokenStream2, fields_r: TokenStream2, fields_w: TokenStream2, rsig: TokenStream2, wsig: TokenStream2) -> Result<TokenStream2> {
    let (generics, where_c) = generics(&input);
    let name = &input.ident;
    match &input.data {
        Data::Struct(s) => {
            // just read all fields
            // TODO header
            let (r, w): (Vec<_>, Vec<_>) = s.fields
                .iter().enumerate()
                .map(|(i, f)| {
                    let ident = f.ident.as_ref().map(ToTokens::to_token_stream).unwrap_or_else(|| i.to_token_stream());
                    gen_read_and_write(f, &quote! { (&self.#ident) }, trait_ty.clone(), fields_r.clone(), fields_w.clone())
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
                        fn read_from<Reader: std::io::Read>(reader: &mut Reader) -> crate::error::Result<Self> {
                            Ok(#construct)
                        }
                        fn write_to<Writer: std::io::Write>(&self, writer: &mut Writer) -> crate::error::Result<()> {
                            #( #w )*
                            Ok(())
                        }
                    }
            })
        }
        Data::Enum(e) => {
            let (idx, tag_type): (_, Ident) = input.attrs
                .iter()
                .enumerate()
                .find(|(_idx, a)| a.path.is_ident("tag_type"))
                .ok_or_else(|| Error::new(input.span(), "Couldn't find tag_type attribute"))
                .and_then(|(idx, a)| Ok((idx, parse2(parse2::<Group>(a.tokens.clone())?.stream())?)) )?;
            input.attrs.remove(idx);
            let variants = &e.variants;

            let tags = variants
                .iter()
                .scan(LitInt::new("0", Span::call_site()), |fallback, v| {
                    let lit = v.attrs.iter().find(|&a| a.path.is_ident("tag"))
                        .and_then(|a| parse2::<Group>(a.tokens.clone()).ok().as_ref().map(Group::stream).map(parse2).map(Result::ok).flatten() )
                        .or_else(|| v.discriminant.clone().and_then(|(_, e)| if let Expr::Lit(ExprLit { attrs:_, lit: Lit::Int(l) }) = e { Some(l) } else { None }))
                        .unwrap_or_else(|| fallback.clone());
                    *fallback = add_one(lit.clone());
                    Some(lit)
                }).collect::<Vec<_>>();
            let variant_fields_idents = variants.iter().map(|v| &v.fields).map(generate_idents_for_fields).collect::<Vec<_>>();
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
            let (v_r, v_w) : (Vec<Vec<_>>, Vec<Vec<_>>) = variants
                .iter()
                .zip(variant_fields_idents.iter())
                .map(|(v, idents)|
                    v.fields
                        .iter()
                        .zip(idents.iter())
                        .map(|(f, i)| gen_read_and_write(f, i, trait_ty.clone(), fields_r.clone(), fields_w.clone()))
                        .collect::<Result<Vec<_>>>())
                .collect::<Result<Vec<Vec<_>>>>()?
                .into_iter()
                .map(|v| v.into_iter().unzip())
                .unzip();

            let variant_write_bodies = v_w.iter().map(|tk| {
                quote! { #(#tk)* }
            });
            let variant_read_bodies = v_r.iter().zip(variant_fields_idents.iter()).map(|(r,i)| {
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
                                    Err(crate::error::Error::Invalid("tag", tag.to_string()))
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
        Data::Union(_) => {
            Err(Error::new(input.span(), "This macro should not be used on a union type"))
        }
    }
}