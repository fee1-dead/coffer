use crate::prelude::*;
use once_cell::sync::OnceCell;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone)]
pub struct Dynamic {
    pub bsm: Arc<OnceCell<BootstrapMethod>>,
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
            bsm: Arc::new(bsm.into()),
            name: name.into(),
            descriptor: descriptor.into(),
        }
    }

    /// Returns an immutable reference to the bootstrap method of this dynamic computed constant.
    pub fn get_bsm(&self) -> &BootstrapMethod {
        self.bsm.get().expect("Expected bsm to be populated")
    }
}

impl PartialEq for Dynamic {
    fn eq(&self, other: &Self) -> bool {
        self.bsm.eq(&other.bsm)
            && self.name.eq(&other.name)
            && self.descriptor.eq(&other.descriptor)
    }
}

impl Eq for Dynamic {}

impl Hash for Dynamic {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bsm.get().hash(state);
        self.name.hash(state);
        self.descriptor.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
