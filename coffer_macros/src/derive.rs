use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::*;
use synstructure::{Structure, VariantInfo};

#[derive(Default)]
pub struct DeriveConfig {
    pub variants: Vec<VariantConfig>,
    pub raw_variant: Option<usize>,
    pub tag_type: Option<Ident>,
}

#[derive(Default)]
pub struct VariantConfig {
    pub estimated_size: Option<usize>,
    pub tag: Option<u128>,
    pub fields: Vec<FieldConfig>,
}

#[derive(Default)]
pub struct FieldConfig {
    pub as_: Option<Type>,
}

macro_rules! bail {
    ($s: expr, $msg: literal) => {{
        return Err(syn::Error::new_spanned($s, $msg));
    }};
}

fn parse_common<F>(attrs: &[syn::Attribute], mut func: F) -> syn::Result<()>
where
    F: FnMut(Meta) -> syn::Result<()>,
{
    for attr in attrs {
        if !attr.path.is_ident("coffer") {
            continue;
        }
        let meta = attr.parse_meta()?;

        let list = match meta {
            Meta::List(list) => list,
            _ => bail!(meta, "Usage: `#[coffer(options..)]`"),
        };

        for meta in list.nested {
            let meta = match meta {
                NestedMeta::Lit(_) => bail!(meta, "literals are not allowed here"),
                NestedMeta::Meta(meta) => meta,
            };
            func(meta)?;
        }
    }
    Ok(())
}

impl DeriveConfig {
    pub fn parse(s: &Structure) -> syn::Result<Self> {
        let mut cfg = Self::default();
        parse_common(&s.ast().attrs, |meta| {
            match meta {
                Meta::List(MetaList { path, nested, .. }) if path.is_ident("tag_type") => {
                    if nested.len() == 1 {
                        match nested.first().unwrap() {
                            NestedMeta::Meta(Meta::Path(path)) => {
                                if let Some(i) = path.get_ident() {
                                    cfg.tag_type = Some(i.clone());
                                } else {
                                }
                            }
                            _ => bail!(nested, "invalid `tag_type` value"),
                        }
                    } else {
                        bail!(nested, "too many values");
                    }
                }
                _ => bail!(meta, "unrecognized or invalid configuration"),
            }
            Ok(())
        })?;
        for (i, variant) in s.variants().iter().enumerate() {
            let (variant, is_raw) = VariantConfig::parse(variant)?;
            cfg.variants.push(variant);
            if is_raw {
                if let Some(variant) = cfg.raw_variant {
                    bail!(
                        s.variants()[variant].ast().ident,
                        "duplicated raw attribute variant"
                    );
                } else {
                    cfg.raw_variant = Some(i);
                }
            }
        }
        Ok(cfg)
    }
}

impl VariantConfig {
    pub fn parse(v: &VariantInfo) -> syn::Result<(Self, bool)> {
        let mut cfg = Self::default();
        let mut is_raw = false;

        parse_common(v.ast().attrs, |meta| {
            match meta {
                Meta::Path(p) if p.is_ident("raw_variant") => {
                    is_raw = true;
                }
                Meta::NameValue(MetaNameValue {
                    path,
                    lit: Lit::Int(lit_int),
                    ..
                }) if path.is_ident("tag") => {
                    cfg.tag = Some(lit_int.base10_parse()?);
                }
                Meta::NameValue(MetaNameValue {
                    path,
                    lit: Lit::Int(lit_int),
                    ..
                }) if path.is_ident("estimated_size") => {
                    cfg.estimated_size = Some(lit_int.base10_parse()?)
                }
                _ => bail!(meta, "unrecognized or invalid configuration"),
            }
            Ok(())
        })?;

        for f in v.ast().fields.iter() {
            cfg.fields.push(FieldConfig::parse(f)?);
        }

        Ok((cfg, is_raw))
    }
}

impl FieldConfig {
    pub fn parse(f: &Field) -> syn::Result<Self> {
        let mut cfg = FieldConfig::default();
        parse_common(&f.attrs, |meta| {
            match meta {
                Meta::NameValue(MetaNameValue {
                    path,
                    eq_token: _,
                    lit: Lit::Str(s),
                }) if path.is_ident("as") => {
                    cfg.as_ = Some(parse_str(&s.value())?);
                }
                _ => bail!(meta, "unrecognized or invalid configuration"),
            }
            Ok(())
        })?;
        Ok(cfg)
    }
}

pub(crate) fn attr_enum(s: synstructure::Structure) -> Result<TokenStream2> {
    let config = DeriveConfig::parse(&s)?;

    if let Data::Enum(_) = &s.ast().data {
        let raw_variant = config.raw_variant.expect("raw variant");
        let raw_variant_name = &s.variants()[raw_variant].ast().ident;
        let cp = Ident::new("cp", Span::call_site());
        let reader = Ident::new("reader", Span::call_site());
        let writer = Ident::new("writer", Span::call_site());
        let mut variant_index = 0;
        let write_match = s.each_variant(|variant| {
            let v = &config.variants[variant_index];
            let ret = if variant_index == raw_variant {
                let raw_attr = &variant.bindings()[0];
                quote! {
                    if !#raw_attr.keep {
                        return Ok(());
                    }
                    u16::write_to(&#cp.insert_wtf8(#raw_attr.name.clone()), #writer)?;
                    crate::write_to!(&(#raw_attr.inner.len() as u32), #writer)?;
                    #writer.write_all(&#raw_attr.inner)?;
                }
            } else {
                let attr_name = variant.ast().ident;
                let vec = Ident::new("vec", Span::call_site());
                let inner_writer = Ident::new("inner_writer", Span::call_site());
                let construct = if let Some(size) = v.estimated_size {
                    quote! { Vec::with_capacity(#size) }
                } else {
                    quote! { Vec::new() }
                };
                let fields: TokenStream2 = variant.bindings().iter().enumerate().map(|(idx, x)| {
                    let fcfg = &v.fields[idx];
                    let ty = &x.ast().ty;
                    if let Some(as_) = &fcfg.as_ {
                        quote! {
                            <#as_ as crate::helper::ConstantPoolReadWriteAs<#ty>>::write_to(#x, #cp, #inner_writer)?;
                        }
                    } else {
                        quote! {
                            <#ty as crate::rw::ConstantPoolReadWrite>::write_to(#x, #cp, #inner_writer)?;
                        }
                    }
                }).collect();
                quote! {
                    u16::write_to(&#cp.insert_wtf8(::std::borrow::Cow::Borrowed(::wtf_8::Wtf8Str::new(stringify!(#attr_name)))), #writer)?;
                    let mut #vec = #construct;
                    let mut __inner_writer = ::std::io::Cursor::new(&mut #vec);
                    let mut #inner_writer = &mut __inner_writer;
                    #fields
                    u32::write_to(&(#vec.len() as u32), #writer)?;
                    #writer.write_all(&#vec)?;
                }
            };
            variant_index += 1;
            ret
        });
        let mut variant_index = 0;
        let (constants, read_match): (TokenStream2, TokenStream2) = s.variants().iter().filter(|x| x.ast().ident != *raw_variant_name).map(|variant| {
            let v = &config.variants[variant_index];
            let inner_reader = Ident::new("inner_reader", Span::call_site());
            let id = variant.ast().ident;
            let construct = variant.construct(|field, i| {
                let ty = &field.ty;
                let f = &v.fields[i];
                if let Some(as_) = &f.as_ {
                    quote! {
                        <#as_ as crate::helper::ConstantPoolReadWriteAs<#ty>>::read_from(#cp, #inner_reader)?
                    }
                } else {
                    quote! {
                        <#ty as crate::ConstantPoolReadWrite>::read_from(#cp, #inner_reader)?
                    }
                }
            });
            let ident = Ident::new(&format!("WTF8_ATTR_{id}"), Span::call_site());
            let ret = (quote!{
                const #ident: &'static [u8] = stringify!(#id).as_bytes();
            }, quote! {
                #ident => {
                    let len = u32::read_from(#reader)?;
                    let mut vec = vec![0; len as usize];
                    #reader.read_exact(&mut vec)?;
                    let mut __inner_reader: &[u8] = vec.as_ref();
                    let #inner_reader = &mut __inner_reader;
                    let ret = #construct;
                    if !inner_reader.is_empty() {
                        return Err(crate::error::Error::AttributeLength(len, len - (inner_reader.len() as u32), stringify!(#id)))
                    }
                    Ok(ret)
                }
            });
            variant_index += 1;
            ret
        }).unzip();
        Ok(s.gen_impl(quote! {
            gen impl crate::ConstantPoolReadWrite for @Self {
                fn read_from<C: crate::ConstantPoolReader, R: std::io::Read>(cp: &mut C, #reader: &mut R) -> crate::Result<Self> {
                    #constants
                    let idx = u16::read_from(#reader)?;
                    let attribute_name = cp.read_wtf8(idx).ok_or_else(|| crate::error::Error::Invalid("attribute index", Into::into(idx.to_string())))?;
                    match attribute_name.as_bytes() {
                        #read_match
                        _ => {
                            let byte_size = u32::read_from(#reader)?;
                            let mut bytes = vec![0; byte_size as usize];
                            #reader.read_exact(&mut bytes)?;
                            let raw_attr = crate::prelude::RawAttribute::__new(attribute_name, bytes);
                            Ok(Self::#raw_variant_name(raw_attr))
                        }
                    }
                }
                fn write_to<C: crate::ConstantPoolWriter, W: std::io::Write>(&self, cp: &mut C, writer: &mut W) -> crate::Result<()> {
                    match self {
                        #write_match
                    }
                    Ok(())
                }
            }
        }))
    } else {
        Err(Error::new(Span::call_site(), "Must be an enum."))
    }
}

pub(crate) fn derive_readwrite_impl(s: Structure) -> Result<TokenStream2> {
    let cfg = DeriveConfig::parse(&s)?;
    let writer = Ident::new("writer", Span::call_site());
    let reader = Ident::new("reader", Span::call_site());
    match s.ast().data {
        Data::Struct(_) => {
            let variant = &s.variants()[0];
            let cfg = &cfg.variants[0];
            let mut field = 0;
            let write = s.each(|x| {
                let fcfg = &cfg.fields[field];
                field += 1;
                let ty = &x.ast().ty;
                if let Some(as_) = &fcfg.as_ {
                    quote! {
                        <#as_ as crate::helper::ReadWriteAs<#ty>>::write_to(#x, #writer)?;
                    }
                } else {
                    quote! {
                        <#ty as crate::ReadWrite>::write_to(#x, #writer)?;
                    }
                }
            });
            let read = variant.construct(|f, i| {
                let fcfg = &cfg.fields[i];
                let ty = &f.ty;
                if let Some(as_) = &fcfg.as_ {
                    quote! {
                        <#as_ as crate::helper::ReadWriteAs<#ty>>::read_from(#reader)?
                    }
                } else {
                    quote! {
                        <#ty as crate::ReadWrite>::read_from(#reader)?
                    }
                }
            });

            Ok(s.gen_impl(quote! {
                gen impl crate::ReadWrite for @Self {
                    fn read_from<R: std::io::Read>(#reader: &mut R) -> crate::error::Result<Self> {
                        Ok(#read)
                    }
                    fn write_to<W: std::io::Write>(&self, #writer: &mut W) -> crate::error::Result<()> {
                        match self {
                            #write
                        }
                        Ok(())
                    }
                }
            }))
        }
        Data::Enum(_) => {
            // this is a tagged enum
            let tags: Vec<_> = s
                .variants()
                .iter()
                .enumerate()
                .scan(0, |fallback, (i, v)| {
                    // #[coffer(tag = 1)] -> A = 1 -> fallback (prev tag + 1)
                    let tag = if let Some(tag) = cfg.variants[i].tag {
                        tag
                    } else if let Some((
                        _,
                        Expr::Lit(ExprLit {
                            lit: Lit::Int(i), ..
                        }),
                    )) = v.ast().discriminant
                    {
                        i.base10_parse().unwrap()
                    } else {
                        *fallback
                    };
                    *fallback = tag + 1;
                    Some(tag)
                })
                .collect();
            let tag_type = cfg.tag_type.as_ref().expect("expected tag_type on enum");
            let mut v = 0;
            let write = s.each_variant(|x| {
                let vcfg = &cfg.variants[v];
                let tag = tags[v];
                let tag = LitInt::new(&format!("{tag}"), Span::call_site());
                v += 1;
                let mut field = 0;
                let fields = x.bindings().iter().map(|x| {
                    let fcfg = &vcfg.fields[field];
                    let ty = &x.ast().ty;
                    field += 1;
                    if let Some(as_) = &fcfg.as_ {
                        quote! {
                            <#as_ as crate::helper::ReadWriteAs<#ty>>::write_to(#x, #writer)?;
                        }
                    } else {
                        quote! {
                            <#ty as crate::ReadWrite>::write_to(#x, #writer)?;
                        }
                    }
                });
                quote! {
                    #tag_type::write_to(&#tag, #writer)?;
                    #(#fields)*
                }
            });
            let read: TokenStream2 = s
                .variants()
                .iter()
                .enumerate()
                .map(|(i, x)| {
                    let vcfg = &cfg.variants[i];
                    let tag = tags[i];
                    let tag = LitInt::new(&format!("{tag}"), Span::call_site());
                    let construct = x.construct(|field, i| {
                        let fcfg = &vcfg.fields[i];
                        let ty = &field.ty;
                        if let Some(as_) = &fcfg.as_ {
                            quote! {
                                <#as_ as crate::helper::ReadWriteAs<#ty>>::read_from(#reader)?
                            }
                        } else {
                            quote! {
                                <#ty as crate::ReadWrite>::read_from(#reader)?
                            }
                        }
                    });
                    quote! {
                        #tag => {
                            #construct
                        }
                    }
                })
                .collect();
            Ok(s.gen_impl(quote! {
                gen impl crate::ReadWrite for @Self {
                    fn read_from<R: std::io::Read>(#reader: &mut R) -> crate::error::Result<Self> {
                        Ok(match <#tag_type as crate::ReadWrite>::read_from(#reader)? {
                            #read
                            tag => {
                                return Err(crate::error::Error::Invalid("tag", Into::into(tag.to_string())))
                            }
                        })
                    }
                    fn write_to<W: std::io::Write>(&self, #writer: &mut W) -> crate::error::Result<()> {
                        match self {
                            #write
                        }
                        Ok(())
                    }
                }
            }))
        }
        _ => panic!(),
    }
}

pub(crate) fn derive_constant_pool_readwrite_impl(s: Structure) -> Result<TokenStream2> {
    let cfg = DeriveConfig::parse(&s)?;
    let cp = Ident::new("cp", Span::call_site());
    let writer = Ident::new("writer", Span::call_site());
    let reader = Ident::new("reader", Span::call_site());
    match s.ast().data {
        Data::Struct(_) => {
            let variant = &s.variants()[0];
            let cfg = &cfg.variants[0];
            let mut field = 0;
            let write = s.each(|x| {
                let fcfg = &cfg.fields[field];
                field += 1;
                let ty = &x.ast().ty;
                if let Some(as_) = &fcfg.as_ {
                    quote! {
                        <#as_ as crate::helper::ConstantPoolReadWriteAs<#ty>>::write_to(#x, #cp, #writer)?;
                    }
                } else {
                    quote! {
                        <#ty as crate::ConstantPoolReadWrite>::write_to(#x, #cp, #writer)?;
                    }
                }
            });
            let read = variant.construct(|f, i| {
                let fcfg = &cfg.fields[i];
                let ty = &f.ty;
                if let Some(as_) = &fcfg.as_ {
                    quote! {
                        <#as_ as crate::helper::ConstantPoolReadWriteAs<#ty>>::read_from(#cp, #reader)?
                    }
                } else {
                    quote! {
                        <#ty as crate::ConstantPoolReadWrite>::read_from(#cp, #reader)?
                    }
                }
            });

            Ok(s.gen_impl(quote! {
                gen impl crate::ConstantPoolReadWrite for @Self {
                    fn read_from<C: crate::prelude::ConstantPoolReader, R: std::io::Read>(#cp: &mut C, #reader: &mut R) -> crate::error::Result<Self> {
                        Ok(#read)
                    }
                    fn write_to<C: crate::prelude::ConstantPoolWriter, W: std::io::Write>(&self, #cp: &mut C, #writer: &mut W) -> crate::error::Result<()> {
                        match self {
                            #write
                        }
                        Ok(())
                    }
                }
            }))
        }
        Data::Enum(_) => {
            // this is a tagged enum
            let tags: Vec<_> = s
                .variants()
                .iter()
                .enumerate()
                .scan(0, |fallback, (i, v)| {
                    // #[coffer(tag = 1)] -> A = 1 -> fallback (prev tag + 1)
                    let tag = if let Some(tag) = cfg.variants[i].tag {
                        tag
                    } else if let Some((
                        _,
                        Expr::Lit(ExprLit {
                            lit: Lit::Int(i), ..
                        }),
                    )) = v.ast().discriminant
                    {
                        i.base10_parse().unwrap()
                    } else {
                        *fallback
                    };
                    *fallback = tag + 1;
                    Some(tag)
                })
                .collect();
            let tag_type = cfg.tag_type.as_ref().expect("expected tag_type on enum");
            let mut v = 0;
            let write = s.each_variant(|x| {
                let vcfg = &cfg.variants[v];
                let tag = tags[v];
                let tag = LitInt::new(&format!("{tag}"), Span::call_site());
                v += 1;
                let mut field = 0;
                let fields = x.bindings().iter().map(|x| {
                    let fcfg = &vcfg.fields[field];
                    let ty = &x.ast().ty;
                    field += 1;
                    if let Some(as_) = &fcfg.as_ {
                        quote! {
                            <#as_ as crate::helper::ConstantPoolReadWriteAs<#ty>>::write_to(#x, #cp, #writer)?;
                        }
                    } else {
                        quote! {
                            <#ty as crate::ConstantPoolReadWrite>::write_to(#x, #cp, #writer)?;
                        }
                    }
                });
                quote! {
                    #tag_type::write_to(&#tag, #writer)?;
                    #(#fields)*
                }
            });
            let read: TokenStream2 = s.variants().iter().enumerate().map(|(i, x)| {
                let vcfg = &cfg.variants[i];
                let tag = tags[i];
                let tag = LitInt::new(&format!("{tag}"), Span::call_site());
                let construct = x.construct(|field, i| {
                    let fcfg = &vcfg.fields[i];
                    let ty = &field.ty;
                    if let Some(as_) = &fcfg.as_ {
                        quote! {
                            <#as_ as crate::helper::ConstantPoolReadWriteAs<#ty>>::read_from(#cp, #reader)?
                        }
                    } else {
                        quote! {
                            <#ty as crate::ConstantPoolReadWrite>::read_from(#cp, #reader)?
                        }
                    }
                });
                quote! {
                    #tag => {
                        #construct
                    }
                }
            }).collect();
            Ok(s.gen_impl(quote! {
                gen impl crate::ConstantPoolReadWrite for @Self {
                    fn read_from<C: crate::prelude::ConstantPoolReader, R: std::io::Read>(#cp: &mut C, #reader: &mut R) -> crate::error::Result<Self> {
                        Ok(match <#tag_type as crate::ReadWrite>::read_from(#reader)? {
                            #read

                            tag => {
                                return Err(crate::error::Error::Invalid("tag", Into::into(tag.to_string())));
                            }
                        })
                    }
                    fn write_to<C: crate::prelude::ConstantPoolWriter, W: std::io::Write>(&self, #cp: &mut C, #writer: &mut W) -> crate::error::Result<()> {
                        match self {
                            #write
                        }
                        Ok(())
                    }
                }
            }))
        }
        _ => panic!(),
    }
}
/*
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
                                .and_then(Result::ok)
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
}*/
