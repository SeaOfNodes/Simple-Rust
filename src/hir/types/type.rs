use std::hash::{Hash, Hasher};
use std::sync::Arc;

use crate::hir::location::Location;
use crate::hir::types::{Ty, Types};
use crate::modules::ParsedModule;

/// A concrete hir type.
/// This includes values such as ranges of integers for constant propagation.
#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum Type<'a> {
    Error,
    Integer(TyInteger),
    Boolean(Option<bool>),
    Inferred,
    NullTerminatedString,
    FunctionSignature(TyFunctionSignature<'a>),
    Function(TyFunction<'a>),
    DuplicateDefinition,
    Unit,
    Void,
    Pointer(Ty<'a>),
    Enum(TyEnum<'a>),
    Struct(TyStruct<'a>),
    Module(TyModule<'a>),
    Unresolved,
}

impl<'t> Type<'t> {
    pub fn unwrap_struct(&'t self) -> &TyStruct<'t> {
        match self {
            Type::Struct(s) => &s,
            _ => unreachable!(),
        }
    }
    pub fn unwrap_module(&'t self) -> &TyModule<'t> {
        match self {
            Type::Module(m) => &m,
            _ => unreachable!(),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct TyInteger {
    pub bits: u8,
    pub signed: bool,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct TyFunctionSignature<'a> {
    pub parameters: Vec<Ty<'a>>,
    pub return_type: Ty<'a>,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct TyFunction<'a> {
    pub name: String,
    pub signature: Ty<'a>,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct TyEnum<'a> {
    pub location: Location<'a>,
    pub parent: Ty<'a>,
    pub members: Option<Vec<(String, i64)>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TyStruct<'a> {
    pub location: Location<'a>,
}

impl TyStruct<'_> {
    pub fn member<'a>(&'a self, name: &str, types: &'a Types) -> Option<&(String, Ty)> {
        todo!()
        // self.members.get().and_then(|members| members.iter().find(|m| m.0 == name))
        //     .or_else(|| match &*self.parent {
        //         Type::Struct(stru) => {
        //             stru.member(name, types)
        //         }
        //         _ => None
        //     })
    }
}

#[derive(Clone, Debug)]
pub struct TyModule<'a> {
    pub parent: Option<Ty<'a>>,
    pub name: String,
    pub parsed_module: Option<Arc<ParsedModule>>,
}

impl Eq for TyModule<'_> {}

impl PartialEq for TyModule<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.parent == other.parent && self.name == other.name
    }
}

impl Hash for TyModule<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.parent.hash(state);
        self.name.hash(state);
    }
}

