use std::hash::{Hash, Hasher};
use std::{cmp, fmt};


#[derive(Clone, Copy)]
pub struct TotalF32(pub f32);

#[derive(Clone, Copy)]
pub struct TotalF64(pub f64);

impl fmt::Debug for TotalF32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for TotalF64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for TotalF32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for TotalF64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq for TotalF32 {
    fn eq(&self, other: &Self) -> bool {
        self.0.total_cmp(&other.0).is_eq()
    }
}

impl PartialEq for TotalF64 {
    fn eq(&self, other: &Self) -> bool {
        self.0.total_cmp(&other.0).is_eq()
    }
}

impl PartialOrd for TotalF32 {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd for TotalF64 {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for TotalF32 {}

impl Eq for TotalF64 {}

impl Ord for TotalF32 {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.0.total_cmp(&other.0)
    }
}

impl Ord for TotalF64 {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.0.total_cmp(&other.0)
    }
}

impl Hash for TotalF32 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}

impl Hash for TotalF64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}

impl From<f32> for TotalF32 {
    fn from(x: f32) -> Self {
        Self(x)        
    }
}

impl From<f64> for TotalF64 {
    fn from(x: f64) -> Self {
        Self(x)
    }
}

impl From<TotalF32> for f32 {
    fn from(x: TotalF32) -> Self {
        x.0
    }
}

impl From<TotalF64> for f64 {
    fn from(x: TotalF64) -> Self {
        x.0
    }
}

impl PartialEq<f32> for TotalF32 {
    fn eq(&self, other: &f32) -> bool {
        self.0.total_cmp(other).is_eq()
    }
}

impl PartialEq<f64> for TotalF64 {
    fn eq(&self, other: &f64) -> bool {
        self.0.total_cmp(other).is_eq()
    }
}

impl PartialEq<TotalF32> for f32 {
    fn eq(&self, other: &TotalF32) -> bool {
        self.total_cmp(&other.0).is_eq()
    }
}

impl PartialEq<TotalF64> for f64 {
    fn eq(&self, other: &TotalF64) -> bool {
        self.total_cmp(&other.0).is_eq()
    }
}
