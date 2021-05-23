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

mod code_block;
mod derive;
use derive::*;

use proc_macro::TokenStream;
use proc_macro2::{Spacing, TokenStream as TokenStream2, TokenTree};
use quote::{quote, ToTokens};
use syn::{DeriveInput, Error};

#[proc_macro]
pub fn code_block(tokens: TokenStream) -> TokenStream {
    code_block_inner(TokenStream2::from(tokens))
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

/// Grammar:
///
/// ```mygrammar
/// +class ""
/// +str ""
/// +int 1
/// +long 2
/// +float 1.0
/// -int
/// -float
/// -double
/// ^int
/// ^long
/// ~int
/// >= 0 int
/// < 0 int
/// >= int
/// 'label:
/// goto 'label
///
/// lookupswitch {
///     1 => 'label,
///     2 => 'label,
///     _ => 'label
/// }
///
/// swap
///
/// tableswitch 1-2 { 'label, 'label } 'label
///
/// invokevirtual "java/lang/String" "intern" "()V"
///
/// ```
#[allow(unused)] // Allow for now
fn code_block_inner(tokens: TokenStream2) -> syn::Result<TokenStream2> {
    let mut iter = tokens.into_iter();
    let mut res: Vec<TokenStream2> = Vec::new();

    while let Some(tt) = iter.next() {
        match tt {
            TokenTree::Ident(op) => todo!(),
            TokenTree::Punct(p) if p.spacing() == Spacing::Alone => {
                if let Some(TokenTree::Ident(label)) = iter.next() {}
            }
            _ => {}
        }
    }
    todo!()
}

/// !Internal use only! Derive a `ConstantPoolReadWrite` trait.
///
/// This will call the functions for the fields, therefore fields must implement `ConstantPoolReadWrite` or `ReadWrite`.
///
/// Attributes available:
///   - `tag_type`: indicates the type of the tag for the enum. This must be an integer type, and is big-endian only.
///   - `tag`: indicates individual tags for enum variants. When the tag attribute is absent, a discriminant is used. When both the tag attribute and a discriminant is missing, the tag is incremented from the last variant.
///   - `attr_enum`: indicates this is an enum that is an attribute. Rather than matching tags, it will match on attribute names. A `raw_variant` must be specified.
///   - `raw_variant`: indicates this is the raw variant of the attribute enum. The field list must be exactly `(RawAttribute)`.
///   - `use_normal_rw`: indicates using normal `ReadWrite` trait instead of `ConstantPoolReadWrite`.
///   - `str_type`: indicates this field is one of the constant pool types that has a string. One of `Package`, `Module`, `String` and `Class` to be exact. Therefore a type must be specified: `#[str_type(Class)]`
///   - `str_optional`: indicates this field is an optional string. `None` represents `0` in byte form. The field must be `Option<Cow<'static, str>>`.
///   - `vec_len_type`: indicates the length type of the vec. if this is `#[vec_len_type(u32)]`, then the 32-bit length `n` is written/read first.
#[proc_macro_derive(
    ConstantPoolReadWrite,
    attributes(
        tag_type,
        tag,
        attr_enum,
        attr_normal_size,
        raw_variant,
        use_normal_rw,
        str_type,
        str_optional,
        vec_len_type
    )
)]
pub fn derive_cp_readwrite(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as DeriveInput);
    let is_enum = input
        .attrs
        .iter()
        .any(|a| a.path.to_token_stream().to_string() == "attr_enum");
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
            quote! { <C: crate::ConstantPoolReader, R: std::io::Read>(cp: &mut C, reader: &mut R) },
            quote! { <C: crate::ConstantPoolWriter, W: std::io::Write>(&self, cp: &mut C, writer: &mut W) })
    }.unwrap_or_else(Error::into_compile_error).into()
}

///  !Internal use only! Derive a `ReadWrite` trait.
///
/// This will call the functions for the fields, therefore fields must implement `ReadWrite`.
///
/// Attributes available:
///   - `tag_type`: indicates the type of the tag for the enum. This must be an integer type, and is big-endian only.
///   - `tag`: indicates individual tags for enum variants. When the tag attribute is absent, a discriminant is used. When both the tag attribute and a discriminant is missing, the tag is incremented from the last variant.
///   - `vec_len_type`: indicates the length type of the vec. if this is `#[vec_len_type(u32)]`, then the 32-bit length `n` is written/read first.
#[proc_macro_derive(ReadWrite, attributes(tag_type, tag, vec_len_type))]
pub fn derive_readwrite(item: TokenStream) -> TokenStream {
    //println!("input: \"{}\"", item);
    let input = syn::parse_macro_input!(item as DeriveInput);
    let res = derive_readwrite_inner(
        input,
        quote! { crate::ReadWrite },
        quote! { reader },
        quote! { writer },
        quote! { <Reader: std::io::Read>(reader: &mut Reader) },
        quote! { <Writer: std::io::Write>(&self, writer: &mut Writer) },
    )
    .unwrap_or_else(Error::into_compile_error);
    //println!("res: \"{}\"", res.to_string());
    res.into()
}
