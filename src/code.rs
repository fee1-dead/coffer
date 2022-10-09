//! Structures that represent instructions that will be
//! executed when a method is called.

use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, HashMap};
use std::convert::TryFrom;
use std::hash::Hash;
use std::io::{Cursor, Read, Write};
use std::sync::Arc;

use crate::prelude::*;
use crate::{
    read_from, try_cp_read_idx, ConstantPoolReadWrite, ConstantPoolReader, ConstantPoolWriter,
    Error, ReadWrite,
};

mod convert;
mod structure;

use convert::*;
use once_cell::sync::OnceCell;
pub use structure::*;
use wtf_8::Wtf8Str;

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Code {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<Instruction>,
    pub catches: Vec<Catch>,
    pub attrs: Vec<CodeAttribute>,
}

impl ConstantPoolReadWrite for Code {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Self, Error> {
        use std::io::{Seek, SeekFrom};

        use crate::code::Instruction::*;
        use crate::code::{Label as Lbl, LocalVariable as LocalVar};

        struct Labeler<'a, T: ConstantPoolReader> {
            inner: &'a mut T,
            labels: HashMap<u32, Lbl>,
            catches: &'a [Catch],
        }
        impl<'a, T: ConstantPoolReader> ConstantPoolReader for Labeler<'a, T> {
            fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry> {
                self.inner.read_raw(idx)
            }

            fn resolve_later(&mut self, bsm_idx: u16, bsm: Arc<OnceCell<BootstrapMethod>>) {
                self.inner.resolve_later(bsm_idx, bsm)
            }

            fn bootstrap_methods(&mut self, bsms: &[BootstrapMethod]) -> Result<()> {
                self.inner.bootstrap_methods(bsms)
            }

            fn get_label(&mut self, idx: u32) -> Lbl {
                if let Some(v) = self.labels.get(&idx) {
                    *v
                } else {
                    let l = Lbl(self.labels.len() as u32);
                    self.labels.insert(idx, l);
                    l
                }
            }

            fn get_catch(&mut self, idx: u16) -> Option<Catch> {
                self.catches.get(idx as usize).cloned()
            }
        }
        let max_stack = u16::read_from(reader)?;
        let max_locals = u16::read_from(reader)?;
        // Read code to a buffer so we are able to seek.
        let mut code = vec![0; u32::read_from(reader)? as usize];
        reader.read_exact(&mut code)?;
        let mut labeler = Labeler {
            inner: cp,
            labels: HashMap::new(),
            catches: &[],
        };

        let len = code.len() as u64;
        let mut code_reader = Cursor::new(code);
        let mut instructions = Vec::new();
        // Map positions of opcodes to the index to the `instructions`
        let mut pos2idx = HashMap::new();

        while code_reader.position() < len {
            let curpos = code_reader.position();
            pos2idx.insert(curpos as u32, instructions.len());
            let opcode = code_reader.get_ref()[curpos as usize];
            let insn = match opcode {
                // Special opcodes that might contain padding bytes
                crate::constants::insn::TABLESWITCH | crate::constants::insn::LOOKUPSWITCH => {
                    // pad 0-3 bytes to align properly
                    code_reader.seek(SeekFrom::Current((4 - (curpos & 3)) as i64))?;
                    let op = [opcode];
                    let mut chained_reader = (&op).chain(&mut code_reader);
                    crate::insn::Instruction::read_from(&mut chained_reader)?
                }
                _ => crate::insn::Instruction::read_from(&mut code_reader)?,
            };
            let insn = Conv::convert_direct_instruction(insn, &mut labeler, curpos as i64)?;
            instructions.push(insn);
        }
        pos2idx.insert(code_reader.get_ref().len() as u32, instructions.len());
        // ^ the last position that is still valid but will not be covered in the loop

        // Read try-catch blocks.
        let exceptions = u16::read_from(reader)?;
        let mut catches = Vec::with_capacity(exceptions as usize);
        for _ in 0..exceptions {
            let start = read_from!(&mut labeler, reader)?;
            let end = read_from!(&mut labeler, reader)?;
            let handler = read_from!(&mut labeler, reader)?;
            let ty = {
                let idx = u16::read_from(reader)?;
                if idx == 0 {
                    None
                } else {
                    Some(try_cp_read_idx!(labeler, idx, read_class)?)
                }
            };
            catches.push(Catch {
                start,
                end,
                handler,
                catch: ty,
            });
        }
        labeler.catches = &catches;

        // Read Attributes
        let numattrs = u16::read_from(reader)?;
        let mut attrs = Vec::with_capacity(numattrs as usize);

        // Some attributes are instructions that will be inserted to the vector.
        // It should be sorted to avoid ending in the wrong positions.
        let mut to_insert: BTreeMap<usize, Vec<Instruction>> = BTreeMap::new();

        // Local variables have two different types of attributes containing
        // information about them. One has descriptor and other has siganture.
        // A HashMap is used to locate existing local variable data described
        // by an attribute from before.
        let mut local_vars: HashMap<LocalVarKey, LocalVar> = HashMap::new();

        #[derive(Hash, Eq, PartialEq)]
        struct LocalVarKey(Lbl, Lbl, u16, Cow<'static, Wtf8Str>);

        for _ in 0..numattrs {
            match CodeAttr::read_from(&mut labeler, reader)? {
                CodeAttr::LineNumberTable(ln) => {
                    for self::LineNumber(off, line) in ln {
                        to_insert.insert(pos2idx[&(off as u32)], vec![LineNumber(line)]);
                    }
                }
                CodeAttr::LocalVariableTable(localvar) => {
                    for l in localvar {
                        let start = labeler.get_label(l.start as u32);
                        let end = labeler.get_label((l.start + l.len) as u32);

                        let key = LocalVarKey(start, end, l.index, l.name.clone());
                        match local_vars.entry(key) {
                            Entry::Occupied(mut e) => {
                                e.get_mut().descriptor = Some(l.descriptor);
                            }
                            Entry::Vacant(e) => {
                                e.insert(LocalVar {
                                    start,
                                    end,
                                    name: l.name,
                                    descriptor: Some(l.descriptor),
                                    signature: None,
                                    index: l.index,
                                });
                            }
                        }
                    }
                }
                CodeAttr::LocalVariableTypeTable(vartypes) => {
                    for l in vartypes {
                        let start = labeler.get_label(l.start as u32);
                        let end = labeler.get_label((l.start + l.len) as u32);
                        let key = LocalVarKey(start, end, l.index, l.name.clone());
                        match local_vars.entry(key) {
                            Entry::Occupied(mut e) => {
                                e.get_mut().signature = Some(l.signature);
                            }
                            Entry::Vacant(e) => {
                                e.insert(LocalVar {
                                    start,
                                    end,
                                    name: l.name,
                                    descriptor: None,
                                    signature: Some(l.signature),
                                    index: l.index,
                                });
                            }
                        }
                    }
                }
                // Stack map information is ignored for now.
                CodeAttr::StackMapTable(_) => {}
                CodeAttr::RuntimeInvisibleTypeAnnotations(an) => {
                    attrs.push(CodeAttribute::InvisibleTypeAnnotations(an))
                }
                CodeAttr::RuntimeVisibleTypeAnnotations(an) => {
                    attrs.push(CodeAttribute::VisibleTypeAnnotations(an))
                }
                CodeAttr::Raw(r) => attrs.push(CodeAttribute::Raw(r)),
            }
        }
        if !local_vars.is_empty() {
            attrs.push(CodeAttribute::LocalVariables(
                local_vars.into_values().collect(),
            ));
        }
        instructions.reserve(to_insert.len());
        instructions.reserve(labeler.labels.len());
        for (k, v) in labeler.labels {
            to_insert.entry(pos2idx[&k]).or_default().push(Label(v));
        }
        for (k, v) in to_insert.into_iter().rev() {
            for i in v {
                instructions.insert(k, i)
            }
        }
        Ok(Code {
            max_stack,
            max_locals,
            code: instructions,
            catches,
            attrs,
        })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<(), Error> {
        use crate::constants::insn::*;

        self.max_stack.write_to(writer)?;
        self.max_locals.write_to(writer)?;
        let mut buf: Vec<Vec<u8>> = Vec::new();
        let mut jumps: Vec<&Instruction> = Vec::new();
        let mut cursor: Cursor<Vec<u8>> = Cursor::new(Vec::new());
        let mut line_numbers: HashMap<usize, u16> = HashMap::new();
        let mut labels: HashMap<Label, (usize, usize)> = HashMap::new();
        macro_rules! get_label {
            ($label: expr) => {
                *labels.get(&$label).ok_or_else(|| {
                    Error::Invalid("referenced label", $label.0.to_string().into())
                })?
            };
        }

        for insn in self.code.iter() {
            cursor = Conv::write_insn(
                cursor,
                &mut jumps,
                &mut buf,
                &mut labels,
                &mut line_numbers,
                insn,
                cp,
            )?;
        }
        buf.push(cursor.into_inner());
        let mut index_hints = Vec::new();
        let mut last_max_index = 0usize;
        let mut buf_iter = buf.iter();
        // Get minimum/maximum starting index of the next buffer, that is: index_hints[n] is max of buf[n + 1] resulting index.
        for j in &jumps {
            let this_size_max = 1 + match *j {
                Instruction::LookupSwitch { default: _, table } => 11 + table.len() * 8,
                Instruction::TableSwitch {
                    default: _,
                    low: _,
                    offsets,
                } => 15 + offsets.len() * 4, // +3 alignment
                Instruction::Jsr(_) | Instruction::Jump(JumpCondition::Always, _) => 4, // goto_w/jsr_w i32
                Instruction::Jump(_, _) => 7, // conditional jumps can't be wide, so there must be a conversion.
                // other variants are not inserted
                _ => unreachable!(),
            };
            last_max_index += this_size_max + buf_iter.next().unwrap().len();
            index_hints.push(last_max_index);
        }
        /*
        determine the actual size by partially resolving the label through index_hints
            Optimize for possible non-wide jumps, and convert conditional jumps to make it wide-compatible:
            such as:
                ifnull far_away
                ops
            gets converted to
                ifnonnull cont
                goto_w far_away
                cont:
                    ops
        */
        // Where the elements of `buf` end up.
        let mut actual_indices = Vec::new();
        // the index at the opcode byte of the jump instruction.
        let mut last_idx = 0;
        buf_iter = buf.iter();
        let mut sizes_of_jump_insns = Vec::new();
        for j in &jumps {
            last_idx += buf_iter.next().unwrap().len();
            let actual_size = 1 + match *j {
                Instruction::LookupSwitch { default: _, table } => {
                    // Here, `last_idx` is the opcode byte, and `last_idx + 1` is where the
                    // default target begins if there was no padding.

                    // Calculate the modulus and find the padding.
                    // 0 1 2 3 4
                    // * * * * x -> * * * * x (0 -> 0)
                    // * * * x   -> * * * p x (3 -> 1)
                    // * * x     -> * * p p x (2 -> 2)
                    // * x       -> * p p p x (1 -> 3)

                    let modulus = (last_idx + 1) % 4;
                    let padding = if modulus == 0 { 0 } else { 4 - modulus };
                    padding + 8 + table.len() * 8
                }
                Instruction::TableSwitch {
                    default: _,
                    low: _,
                    offsets,
                } => {
                    // see comments above for finding the padding.
                    let modulus = (last_idx + 1) % 4;
                    let padding = if modulus == 0 { 0 } else { 4 - modulus };
                    padding + 12 + offsets.len() * 4
                }
                Instruction::Jsr(target) | Instruction::Jump(JumpCondition::Always, target) => {
                    let (buf_idx, buf_off) = get_label!(target);
                    let target_off = if buf_idx != 0 {
                        index_hints[buf_idx - 1]
                    } else {
                        0
                    } + buf_off;
                    if target_off <= 65535 {
                        2
                    } else {
                        4
                    }
                }
                Instruction::Jump(_, target) => {
                    let (buf_idx, buf_off) = get_label!(target);
                    let target_off = if buf_idx != 0 {
                        index_hints[buf_idx - 1]
                    } else {
                        0
                    } + buf_off;
                    if target_off <= 65535 {
                        2
                    } else {
                        7
                    }
                }
                // SAFETY: other variants are not inserted
                _ => unreachable!(),
            };
            last_idx += actual_size;
            sizes_of_jump_insns.push(actual_size);
            actual_indices.push(last_idx);
        }
        // The index of the second last `buf` element + length of last element.
        let code_len = (buf_iter.next().unwrap().len() + last_idx) as u32;
        code_len.write_to(writer)?; // u4 code_length
        let mut jumps_iter = jumps.into_iter();
        let mut buf_iter = buf.into_iter();
        writer.write_all(&buf_iter.next().unwrap())?;
        for ((i, bytes), (idx, size_of_jump_insn)) in buf_iter
            .enumerate()
            .zip(actual_indices.iter().zip(sizes_of_jump_insns.iter()))
        {
            macro_rules! resolve_label {
                ($label: expr) => {{
                    let (buf_off, inner_off) = get_label!($label);
                    let that_off = (if buf_off == 0 {
                        0
                    } else {
                        actual_indices[buf_off - 1] as u32
                    }) + (inner_off as u32);
                    (that_off as i32).wrapping_sub(*idx as i32) + *size_of_jump_insn as i32
                }};
            }

            macro_rules! wide {
                ($label: ident, $off: ident => $non_wide: expr, $wide: expr) => {{
                    let $off = resolve_label!($label);
                    if let Ok($off) = i16::try_from($off) {
                        $non_wide
                    } else {
                        $wide
                    }
                }};
            }
            let jump = jumps_iter.next().unwrap();
            match jump {
                Instruction::LookupSwitch { default, table } => {
                    LOOKUPSWITCH.write_to(writer)?;
                    let current_idx = actual_indices[i] - sizes_of_jump_insns[i] + 1;
                    let modulus = current_idx % 4;
                    let padding = if modulus == 0 { 0 } else { 4 - modulus };
                    writer.write_all(&vec![0; padding])?; // proper 4 byte alignment
                    write_to!(&resolve_label!(default), writer)?;

                    (table.len() as u32).write_to(writer)?;
                    let mut tbl = table.clone();
                    tbl.sort_keys(); // lookup switch must be sorted
                    for (val, off) in tbl {
                        write_to!(&val, writer)?;
                        write_to!(&resolve_label!(off), writer)?;
                    }
                }
                Instruction::TableSwitch {
                    default,
                    low,
                    offsets,
                } => {
                    TABLESWITCH.write_to(writer)?;
                    let current_idx = actual_indices[i] - sizes_of_jump_insns[i] + 1;
                    let modulus = current_idx % 4;
                    let padding = if modulus == 0 { 0 } else { 4 - modulus };
                    writer.write_all(&vec![0; padding])?; // proper 4 byte alignment
                    write_to!(&resolve_label!(default), writer)?;
                    write_to!(low, writer)?;
                    write_to!(&(low + (offsets.len() - 1) as i32), writer)?;
                    for l in offsets {
                        write_to!(&resolve_label!(l), writer)?;
                    }
                }
                Instruction::Jsr(target) => {
                    wide!(target, off => {
                        JSR.write_to(writer)?;
                        write_to!(&off, writer)?;
                    }, {
                        JSR_W.write_to(writer)?;
                        write_to!(&off, writer)?;
                    })
                }
                Instruction::Jump(JumpCondition::Always, target) => {
                    wide!(target, off => {
                        GOTO.write_to(writer)?;
                        write_to!(&off, writer)?;
                    }, {
                        GOTO_W.write_to(writer)?;
                        write_to!(&off, writer)?;
                    })
                }
                Instruction::Jump(cond, target) => {
                    wide!(target, off => {
                        u8::write_to(&(*cond).into(), writer)?;
                        write_to!(&off, writer)?;
                    }, {
                        // JumpCondition::Always is matched before this
                        u8::write_to(&(-cond).unwrap().into(), writer)?;
                        write_to!(&5i32, writer)?;
                        GOTO_W.write_to(writer)?;
                        write_to!(&off, writer)?;
                    })
                }
                // other variants are not inserted
                _ => unreachable!(),
            }
            writer.write_all(&bytes)?;
        }

        struct Labeler<'a, T: ConstantPoolWriter> {
            indices: &'a Vec<usize>,
            labels: &'a HashMap<Label, (usize, usize)>,
            writer: &'a mut T,
            catches: &'a Vec<Catch>,
        }

        impl<'a, T: ConstantPoolWriter> ConstantPoolWriter for Labeler<'a, T> {
            #[inline]
            fn insert_raw(&mut self, value: RawConstantEntry) -> u16 {
                self.writer.insert_raw(value)
            }

            #[inline]
            fn insert_bsm(&mut self, bsm: BootstrapMethod) -> u16 {
                self.writer.insert_bsm(bsm)
            }

            fn label(&mut self, lbl: &Label) -> u16 {
                let (buf_off, inner_off) = *self.labels.get(lbl).unwrap();
                (if buf_off == 0 {
                    0
                } else {
                    self.indices[buf_off - 1] as u16
                }) + (inner_off as u16)
            }

            fn catch(&mut self, catch: &Catch) -> Option<u16> {
                self.catches
                    .iter()
                    .position(|c| c == catch)
                    .map(|n| n as u16)
            }
        }

        (self.catches.len() as u16).write_to(writer)?;
        let mut labeler = Labeler {
            indices: &actual_indices,
            labels: &labels,
            writer: cp,
            catches: &self.catches,
        };
        for Catch {
            start,
            end,
            handler,
            catch,
        } in &self.catches
        {
            labeler.label(start).write_to(writer)?;
            labeler.label(end).write_to(writer)?;
            labeler.label(handler).write_to(writer)?;
            if let Some(s) = catch {
                labeler.insert_class(s.clone())
            } else {
                0u16
            }
            .write_to(writer)?;
        }
        let mut extra_attrs = 0;
        let mut attributes_writer = Vec::new();

        for a in &self.attrs {
            match a {
                CodeAttribute::VisibleTypeAnnotations(a) => {
                    CodeAttr::RuntimeVisibleTypeAnnotations(a.clone())
                }
                CodeAttribute::InvisibleTypeAnnotations(a) => {
                    CodeAttr::RuntimeInvisibleTypeAnnotations(a.clone())
                }
                CodeAttribute::LocalVariables(l) => {
                    let mut ty: Vec<LocalVarType> = vec![];
                    let mut var: Vec<LocalVar> = vec![];
                    for lc in l {
                        if let Some(ref desc) = lc.descriptor {
                            let start = labeler.label(&lc.start);
                            let len = labeler.label(&lc.end) - start;
                            var.push(LocalVar {
                                start,
                                len,
                                name: lc.name.clone(),
                                descriptor: desc.clone(),
                                index: lc.index,
                            })
                        }
                        if let Some(ref sig) = lc.signature {
                            let start = labeler.label(&lc.start);
                            let len = labeler.label(&lc.end) - start;
                            ty.push(LocalVarType {
                                start,
                                len,
                                name: lc.name.clone(),
                                signature: sig.clone(),
                                index: lc.index,
                            })
                        }
                    }
                    match (ty.is_empty(), var.is_empty()) {
                        (true, true) => {
                            return Err(Error::Invalid(
                                "local variables",
                                "no localvariable type or descriptor present".into(),
                            ))
                        }
                        (false, true) => CodeAttr::LocalVariableTypeTable(ty),
                        (true, false) => CodeAttr::LocalVariableTable(var),
                        (false, false) => {
                            extra_attrs += 1;
                            CodeAttr::LocalVariableTable(var)
                                .write_to(&mut labeler, &mut attributes_writer)?;
                            CodeAttr::LocalVariableTypeTable(ty)
                        }
                    }
                }
                CodeAttribute::Raw(r) => CodeAttr::Raw(r.clone()),
            }
            .write_to(&mut labeler, &mut attributes_writer)?;
        }
        (self.attrs.len() as u16 + extra_attrs).write_to(writer)?;
        writer.write_all(&attributes_writer)?;
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, ConstantPoolReadWrite)]
#[coffer(tag_type(u8))]
pub enum VerificationType {
    Top,
    Int,
    Float,
    Long,
    Double,
    Null,
    UninitializedThis,
    Object(#[coffer(as = "h::Class")] Cow<'static, Wtf8Str>),
    /// Following the label, must be a `NEW` instruction.
    UninitializedVariable(Label),
}

impl VerificationType {
    pub const fn is_wide(&self) -> bool {
        matches!(self, VerificationType::Double | VerificationType::Long)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum RawFrame {
    Same(u16),
    SameLocalsOneStack(u16, VerificationType),
    /// Chop up to three.
    Chop(u16, u8),
    /// At most three items.
    Append(u16, Vec<VerificationType>),
    /// Locals and then stack values.
    Full(u16, Vec<VerificationType>, Vec<VerificationType>),
}

impl ConstantPoolReadWrite for RawFrame {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> crate::Result<Self> {
        let tag = u8::read_from(reader)?;
        Ok(match tag {
            0..=63 => RawFrame::Same(tag as u16),
            64..=127 => RawFrame::SameLocalsOneStack(
                (tag - 64) as u16,
                VerificationType::read_from(cp, reader)?,
            ),
            128..=246 => {
                return Err(Error::Invalid(
                    "tag (is reserved for future use)",
                    tag.to_string().into(),
                ))
            }
            247 => RawFrame::SameLocalsOneStack(
                u16::read_from(reader)?,
                VerificationType::read_from(cp, reader)?,
            ),
            248..=250 => RawFrame::Chop(u16::read_from(reader)?, 251 - tag),
            251 => RawFrame::Same(u16::read_from(reader)?),
            252..=254 => RawFrame::Append(u16::read_from(reader)?, {
                let mut vec = Vec::with_capacity((tag - 251) as usize);
                for _ in 251..tag {
                    vec.push(VerificationType::read_from(cp, reader)?)
                }
                vec
            }),
            _ => RawFrame::Full(
                u16::read_from(reader)?,
                {
                    let locals = u16::read_from(reader)?;
                    let mut local = Vec::with_capacity(locals as usize);
                    for _ in 0..locals {
                        local.push(VerificationType::read_from(cp, reader)?);
                    }
                    local
                },
                {
                    let stacks = u16::read_from(reader)?;
                    let mut stack = Vec::with_capacity(stacks as usize);
                    for _ in 0..stacks {
                        stack.push(VerificationType::read_from(cp, reader)?);
                    }
                    stack
                },
            ),
        })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> crate::Result<()> {
        match self {
            RawFrame::Same(off @ 0..=63) => (*off as u8).write_to(writer)?,
            RawFrame::Same(off) => {
                251u8.write_to(writer)?;
                off.write_to(writer)?;
            }
            RawFrame::SameLocalsOneStack(off @ 0..=63, veri) => {
                (*off as u8 + 64).write_to(writer)?;
                veri.write_to(cp, writer)?;
            }
            RawFrame::SameLocalsOneStack(off, veri) => {
                247u8.write_to(writer)?;
                off.write_to(writer)?;
                veri.write_to(cp, writer)?;
            }
            RawFrame::Chop(off, chop @ 1..=3) => {
                (251 - *chop as u8).write_to(writer)?;
                off.write_to(writer)?;
            }
            RawFrame::Chop(_, c) => return Err(Error::Invalid("Chop value", c.to_string().into())),
            RawFrame::Append(off, locals) if locals.len() <= 3 => {
                (locals.len() as u8 + 251).write_to(writer)?;
                off.write_to(writer)?;
                for local in locals {
                    local.write_to(cp, writer)?;
                }
            }
            RawFrame::Append(_, _) => {
                return Err(Error::Invalid("Append", "locals length > 3".into()))
            }
            RawFrame::Full(off, locals, stack) => {
                255u8.write_to(writer)?;
                off.write_to(writer)?;
                (locals.len() as u16).write_to(writer)?;
                for local in locals {
                    local.write_to(cp, writer)?;
                }
                (stack.len() as u16).write_to(writer)?;
                for s in stack {
                    s.write_to(cp, writer)?;
                }
            }
        }
        Ok(())
    }
}
