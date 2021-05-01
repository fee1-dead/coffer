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

use crate::prelude::*;
use std::cell::UnsafeCell;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BootstrapMethod {
    pub handle: MethodHandle,
    pub arguments: Vec<OrDynamic<Constant>>,
}

impl ConstantPoolReadWrite for BootstrapMethod {
    fn read_from<C: ConstantPoolReader, R: Read>(
        cp: &mut C,
        reader: &mut R,
    ) -> Result<Self, Error> {
        let handle = try_cp_read!(cp, reader, read_method_handle)?;
        let num_arguments = u16::read_from(reader)?;
        let mut arguments = Vec::with_capacity(num_arguments as usize);
        for _ in 0..num_arguments {
            let idx = u16::read_from(reader)?;
            arguments.push(try_cp_read!(
                idx,
                cp.read_or_dynamic(idx, ConstantPoolReader::read_constant)
            )?)
        }
        Ok(BootstrapMethod { handle, arguments })
    }

    fn write_to<C: ConstantPoolWriter, W: Write>(
        &self,
        cp: &mut C,
        writer: &mut W,
    ) -> Result<(), Error> {
        cp.insert_method_handle(self.handle.clone())
            .write_to(writer)?;
        (self.arguments.len() as u16).write_to(writer)?;
        for arg in self.arguments.iter().cloned() {
            cp.insert_ordynamic(arg, ConstantPoolWriter::insert_constant)
                .write_to(writer)?;
        }
        Ok(())
    }
}

/// A dynamic computed constant.
///
/// Note: dynamic computed constants are syntactically allowed to refer to themselves via the bootstrap method table but it will fail during resolution.
/// Rust ownership rules prevent us from doing so.
#[derive(Debug, PartialEq, Hash)]
pub struct Dynamic {
    pub(crate) bsm: Rc<LazyBsm>,
    /// The name of the bootstrap method that will compute the constant value.
    pub name: Cow<'static, str>,
    /// The descriptor of the dynamically computed value. Must be a field descriptor.
    pub descriptor: Type,
}

impl Dynamic {
    /// Creates a new dynamic computed constant.
    pub fn new<N: Into<Cow<'static, str>>, D: Into<Type>>(
        bsm: BootstrapMethod,
        name: N,
        descriptor: D,
    ) -> Dynamic {
        Self {
            bsm: Rc::new(bsm.into()),
            name: name.into(),
            descriptor: descriptor.into(),
        }
    }
    /// Returns an immutable reference to the bootstrap method of this dynamic computed constant.
    pub fn bsm(&self) -> &BootstrapMethod {
        self.bsm.get().expect("Expected bsm to be populated")
    }

    /// Returns a mutable reference to the bootstrap method of this dynamic computed constant.
    pub fn bsm_mut(&mut self) -> &mut BootstrapMethod {
        // This is safe because the Rc would only have one owner since the only other owner would be dropped
        // After reading the entire class file.
        unsafe { self.bsm.__get_mut() }.expect("Expected bsm to be populated")
    }

    pub fn into_inner(self) -> (Option<BootstrapMethod>, Cow<'static, str>, Type) {
        (
            Rc::try_unwrap(self.bsm).unwrap().into_inner(),
            self.name,
            self.descriptor,
        )
    }
}

impl Clone for Dynamic {
    fn clone(&self) -> Self {
        Self {
            bsm: Rc::new(self.bsm.as_ref().clone()),
            name: self.name.clone(),
            descriptor: self.descriptor.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum OrDynamic<T> {
    Dynamic(Dynamic),
    Static(T),
}

impl<T> OrDynamic<T> {
    pub fn map_static<F, N>(self, f: F) -> OrDynamic<N>
    where
        F: FnOnce(T) -> N,
    {
        match self {
            Self::Dynamic(d) => OrDynamic::Dynamic(d),
            Self::Static(t) => OrDynamic::Static(f(t)),
        }
    }
}

impl<T> From<Dynamic> for OrDynamic<T> {
    #[inline]
    fn from(d: Dynamic) -> Self {
        OrDynamic::Dynamic(d)
    }
}

impl From<Constant> for OrDynamic<Constant> {
    #[inline]
    fn from(t: Constant) -> Self {
        OrDynamic::Static(t)
    }
}

impl From<MemberRef> for OrDynamic<MemberRef> {
    #[inline]
    fn from(t: MemberRef) -> Self {
        OrDynamic::Static(t)
    }
}

/// A lazily populated bootstrap method.
#[derive(Debug)]
pub struct LazyBsm {
    inner: UnsafeCell<Option<BootstrapMethod>>,
}

impl LazyBsm {
    pub const fn new() -> LazyBsm {
        Self {
            inner: UnsafeCell::new(None),
        }
    }
    pub fn get(&self) -> Option<&BootstrapMethod> {
        unsafe { &*self.inner.get() }.as_ref()
    }
    pub fn get_mut(&mut self) -> Option<&mut BootstrapMethod> {
        unsafe { &mut *self.inner.get() }.as_mut()
    }
    unsafe fn __get_mut(&self) -> Option<&mut BootstrapMethod> {
        (&mut *self.inner.get()).as_mut()
    }
    pub fn into_inner(self) -> Option<BootstrapMethod> {
        self.inner.into_inner()
    }
    pub(crate) fn fill(&self, value: BootstrapMethod) -> Result<(), BootstrapMethod> {
        let slot = unsafe { &*self.inner.get() };
        if slot.is_some() {
            return Err(value);
        }
        let slot = unsafe { &mut *self.inner.get() };
        *slot = Some(value);

        Ok(())
    }
}

impl Hash for LazyBsm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get().hash(state);
    }
}

impl PartialEq for LazyBsm {
    fn eq(&self, other: &Self) -> bool {
        self.get().eq(&other.get())
    }
}

impl Clone for LazyBsm {
    fn clone(&self) -> Self {
        Self {
            inner: UnsafeCell::new(unsafe { &*self.inner.get() }.clone()),
        }
    }
}

impl From<BootstrapMethod> for LazyBsm {
    fn from(b: BootstrapMethod) -> Self {
        Self {
            inner: UnsafeCell::new(Some(b)),
        }
    }
}
