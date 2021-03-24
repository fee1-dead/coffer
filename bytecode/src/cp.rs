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

use std::hash::{Hash, Hasher};
use std::borrow::Cow;
use std::collections::HashMap;
use crate::{ReadWrite, ConstantPoolReader, ConstantPoolWriter};
use crate::prelude::{Read, Write, Result, BootstrapMethod, LazyBsm};
use std::mem::transmute;
use std::rc::Rc;
use std::collections::hash_map::Entry;

#[derive(ReadWrite, Debug, Clone)]
#[tag_type(u8)]
pub enum RawConstantEntry {
    #[tag(1)]
    UTF8(Cow<'static, str>),
    #[tag(3)]
    Int(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(u16),
    String(u16),
    Field(u16, u16),
    Method(u16, u16),
    InterfaceMethod(u16, u16),
    NameAndType(u16, u16),
    #[tag(15)]
    MethodHandle(u8, u16),
    MethodType(u16),
    Dynamic(u16, u16),
    InvokeDynamic(u16, u16),
    Module(u16),
    Package(u16)
}
impl Hash for RawConstantEntry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            RawConstantEntry::UTF8(ref s) => { s.hash(state) }
            RawConstantEntry::Int(ref i) => { i.hash(state) }
            RawConstantEntry::Float(ref f) => { f.to_bits().hash(state) }
            RawConstantEntry::Long(ref l) => { l.hash(state) }
            RawConstantEntry::Double(ref d) => { d.to_bits().hash(state) }
            RawConstantEntry::Class(ref u) | RawConstantEntry::String(ref u) | RawConstantEntry::MethodType(ref u) | RawConstantEntry::Module(ref u) | RawConstantEntry::Package(ref u) => { u.hash(state) }
            RawConstantEntry::Field(ref u1, ref u2) | RawConstantEntry::Method(ref u1, ref u2) | RawConstantEntry::InterfaceMethod(ref u1, ref u2) | RawConstantEntry::NameAndType(ref u1, ref u2) | RawConstantEntry::Dynamic(ref u1, ref u2) | RawConstantEntry::InvokeDynamic(ref u1, ref u2) => {
                u1.hash(state);
                u2.hash(state);
            }
            RawConstantEntry::MethodHandle(b, u) => {
                b.hash(state);
                u.hash(state);
            }

        }
    }
}

impl RawConstantEntry {
    /// Returns `true` if this entry is a Long/Double constant, which takes 2 indices.
    #[inline]
    pub fn is_wide(&self) -> bool {
        matches!(self, RawConstantEntry::Long(_) | RawConstantEntry::Double(_))
    }
}

/// A simple constant pool implementation using hashmaps for constant entries and bootstrap methods.
///
/// This structure may only be read and not written.
pub struct ReadOnlyConstantPool(pub HashMap<u16, RawConstantEntry>, HashMap<u16, Vec<Rc<LazyBsm>>>);

impl<'a> ReadWrite for ReadOnlyConstantPool {
    fn read_from<T: Read>(reader: &mut T) -> Result<Self> {
        unimplemented!()
    }

    fn write_to<T: Write>(&self, writer: &mut T) -> Result<()> {
        panic!("Cannot write ReadOnlyConstantPool")
    }
}

impl<'a> ConstantPoolReader for ReadOnlyConstantPool {
    fn read_raw(&mut self, idx: u16) -> Option<RawConstantEntry> {
        self.0.get(&idx).cloned()
    }

    fn resolve_later(&mut self, bsm_idx: u16, bsm: Rc<LazyBsm>) {
        self.1.entry(bsm_idx).or_default().push(bsm);
    }

    fn bootstrap_methods(&mut self, bsms: Vec<BootstrapMethod>) {
        for (i, b) in bsms.into_iter().enumerate() {
            if let Some(bsm) = self.1.get(&(i as _)) {
                for reg in bsm {
                    reg.fill(b.clone()).unwrap()
                }
            }
        }
    }
}

impl<'a> ConstantPoolWriter for ReadOnlyConstantPool {
    fn insert_raw(&mut self, value: RawConstantEntry) -> u16 {
        unimplemented!()
    }

    fn insert_bsm(&mut self, bsm: BootstrapMethod) -> u16 {
        unimplemented!()
    }
}
