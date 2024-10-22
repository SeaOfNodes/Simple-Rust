use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::{fmt, ptr};

use crate::sea_of_nodes::types::{Struct, Type};

/// A reference to an interned type.
/// Equality and hashing is based on the value of the pointer.
/// `Option<Ty>` should still be 8 bytes because the reference is never null.
#[derive(Copy, Clone, Eq)]
pub struct Ty<'t>(&'t Type<'t>);

const _: () = assert!(size_of::<Ty>() == size_of::<Option<Ty>>());

impl<'t> Ty<'t> {
    pub fn new(t: &'t Type<'t>) -> Self {
        Self(t)
    }
    pub fn inner(&self) -> &'t Type<'t> {
        self.0
    }
}

impl PartialEq<Self> for Ty<'_> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0 as *const Type, other.0 as *const Type)
    }
}

impl Hash for Ty<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const Type).hash(state)
    }
}

impl Debug for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.0, f)
    }
}

impl<'t> Display for Ty<'t> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.0, f)
    }
}

impl<'a> Deref for Ty<'a> {
    type Target = Type<'a>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct TyStruct<'t>(Ty<'t>);

impl<'a> TryFrom<Ty<'a>> for TyStruct<'a> {
    type Error = ();

    fn try_from(value: Ty<'a>) -> Result<Self, Self::Error> {
        match &*value {
            Type::Struct(_) => Ok(Self(value)),
            _ => Err(()),
        }
    }
}

impl<'a> Deref for TyStruct<'a> {
    type Target = Struct<'a>;
    fn deref(&self) -> &Self::Target {
        match &*self.0 {
            Type::Struct(s) => s,
            _ => unreachable!(),
        }
    }
}
