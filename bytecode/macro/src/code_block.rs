#![allow(unused)]

use proc_macro2::{Ident, Span};
use syn::parse::{Parse, ParseBuffer, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{braced, token::Brace, Lifetime, LitInt, Token};

struct LabelDecl {
    lifetime: Lifetime,
    colon: Token![:],
}

impl Parse for LabelDecl {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(LabelDecl {
            lifetime: input.parse()?,
            colon: input.parse()?,
        })
    }
}

struct TableSwitch {
    start: LitInt,
    to: Token![-],
    end: LitInt,
    brace: Brace,
    labels: Punctuated<Lifetime, Token![,]>,
    default: Lifetime,
}

impl Parse for TableSwitch {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        let start = input.parse()?;
        let to = input.parse()?;
        let end = input.parse()?;
        let brace = braced!(content in input);
        let labels = content.parse_terminated(Lifetime::parse)?;
        let default = input.parse()?;
        Ok(TableSwitch {
            start,
            to,
            end,
            brace,
            labels,
            default,
        })
    }
}

enum Condition {
    Eq(Token![==]),
    Ne(Token![!=]),
    Ge(Token![>=]),
    Gt(Token![>]),
    Le(Token![<=]),
    Lt(Token![<]),
}

enum CondRhs {
    Null(Span),
    Zero(Span),
}

enum CondType {
    Reference,
    Int,
}

enum CondTy {
    Int(Span),
    Integer(Span),
    Ref(Token![ref]),
    Reference(Span),
    A(Span),
    I(Span),
}

impl Parse for CondTy {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![ref]) {
            Ok(CondTy::Ref(input.parse()?))
            // todo
        } else {
            Err(input.error(
                "expected a condition type (valid types: int, integer, i, a, ref, reference)",
            ))
        }
    }
}

struct Cond {
    t: Token![if],
    ty: Ident,
    cond: Condition,
    rhs: Option<CondRhs>,
}

struct Jump {
    to: Lifetime,
    condition: Option<Cond>,
}

enum Op {
    LabelDecl(LabelDecl),
    TableSwitch(TableSwitch),
}
