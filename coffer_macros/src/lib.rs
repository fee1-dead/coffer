mod derive;
use derive::*;
use proc_macro::TokenStream;
use syn::Error;
use synstructure::Structure;

synstructure::decl_derive!(
    [ConstantPoolReadWrite, attributes(coffer)] =>
    /// !Internal use only! Derive a `ConstantPoolReadWrite` trait.
    ///
    /// This will call the functions for the fields, therefore fields must implement
    /// `ConstantPoolReadWrite` or `ReadWrite`.
    ///
    /// Attributes available:
    ///   - `tag_type`: indicates the type of the tag for the enum. This must be an integer type,
    /// and is big-endian only.
    ///
    ///   - `tag`: indicates individual tags for enum variants. When the tag attribute is absent,
    /// a discriminant is used. When both the tag attribute and a discriminant is missing, the tag
    /// is incremented from the last variant.
    ///
    ///   - `attr_enum`: indicates this is an enum that is an attribute. Rather than matching tags,
    /// it will match on attribute names. A `raw_variant` must be specified.
    ///
    ///   - `raw_variant`: indicates this is the raw variant of the attribute enum. The field list
    /// must be exactly `(RawAttribute)`.
    ///
    ///   - `use_normal_rw`: indicates using normal `ReadWrite` trait instead of
    /// `ConstantPoolReadWrite`.
    ///
    ///   - `str_type`: indicates this field is one of the constant pool types that has a string.
    /// One of `Package`, `Module`, `String` and `Class` to be exact. Therefore a type must be
    /// specified: `#[str_type(Class)]`
    ///
    ///   - `str_optional`: indicates this field is an optional string. `None` represents `0` in
    /// byte form. The field must be `Option<Cow<'static, str>>`.
    ///
    ///   - `vec_len_type`: indicates the length type of the vec. if this is `#[vec_len_type(u32)]`,
    /// then the 32-bit length `n` is written/read first.
    derive_cp_readwrite
);
fn derive_cp_readwrite(s: Structure) -> TokenStream {
    derive_constant_pool_readwrite_impl(s)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

synstructure::decl_derive!([AttributeEnum, attributes(coffer)] => derive_attr_enum);
fn derive_attr_enum(s: Structure) -> TokenStream {
    attr_enum(s)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

synstructure::decl_derive!(
    [ReadWrite, attributes(coffer, tag_type, tag, vec_len_type)] =>
    ///  !Internal use only! Derive a `ReadWrite` trait.
    ///
    /// This will call the functions for the fields, therefore fields must implement `ReadWrite`.
    ///
    /// Attributes available:
    ///   - `tag_type`: indicates the type of the tag for the enum. This must be an integer type, and is big-endian only.
    ///   - `tag`: indicates individual tags for enum variants. When the tag attribute is absent, a discriminant is used. When both the tag attribute and a discriminant is missing, the tag is incremented from the last variant.
    ///   - `vec_len_type`: indicates the length type of the vec. if this is `#[vec_len_type(u32)]`, then the 32-bit length `n` is written/read first.
    derive_readwrite
);
fn derive_readwrite(s: Structure) -> TokenStream {
    //println!("input: \"{}\"", item);
    derive_readwrite_impl(s)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}
