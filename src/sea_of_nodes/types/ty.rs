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

macro_rules! impl_subtype {
    ($Subtype:ident<$t:lifetime>($Variant:ident) { $cast:ident } ) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub struct $Subtype<$t>(Ty<$t>);

        impl<$t> $Subtype<$t> {
            pub fn data(self) -> &$t $Variant<$t> {
                match self.0.inner() {
                    Type::$Variant(x) => x,
                    _ => unreachable!(),
                }
            }
        }

        impl<$t> TryFrom<Ty<$t>> for $Subtype<$t> {
            type Error = ();

            fn try_from(value: Ty<$t>) -> Result<Self, Self::Error> {
                match &*value {
                    Type::$Variant(_) => Ok(Self(value)),
                    _ => Err(()),
                }
            }
        }
        impl<'t> Ty<'t> {
            pub fn $cast(self) -> Option<$Subtype<'t>> {
                self.try_into().ok()
            }
        }

        impl<$t> Deref for $Subtype<$t> {
            type Target = Ty<$t>;
            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

    };
}

impl_subtype!(TyStruct<'t>(Struct) { to_struct });
