use std::fmt::{Display, Formatter};

use crate::hir::id::Id;
use crate::hir::location::Location;
use crate::hir::operation::Operation;
use crate::hir::types::Ty;
use crate::hir::Hir;

#[derive(Clone, Debug)]
pub struct Node<'t> {
    pub id: Id,
    pub control: Option<Id>,
    pub operation: Operation,
    pub origin: Option<Location<'t>>,
    pub ty: Ty<'t>,
    pub outputs: Vec<Id>,
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:3} ", self.id.index())?;
        if let Some(control) = self.control {
            write!(f, "{:3} ", control)?;
        } else {
            write!(f, "    ")?;
        }
        match &self.operation {
            it => {
                write!(f, "{:?}", it)?;
            }
        }
        write!(f, " > ")?;
        for (i, output) in self.outputs.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", output)?;
        }
        if let Some(origin) = &self.origin {
            write!(f, " @ {}", origin)?;
        }
        Ok(())
    }
}

impl<'t> Node<'t> {
    fn identity(id: Id, hir: &Hir<'t>) -> Id {
        id
    }

    fn ideal(root: Id, hir: &mut Hir<'t>) -> Option<Id> {
        None
    }

    fn value(id: Id, hir: &Hir<'t>) -> Ty<'t> {
        hir.nodes[id.index()].ty
    }

    pub fn for_each_dependency<F: FnMut(Id)>(&self, mut f: F) {
        self.control.iter().for_each(|x| f(*x));
        self.operation.for_each_dependency(&mut f);
    }

    pub fn starts_basic_block(&self) -> bool {
        match &self.operation {
            Operation::Root { .. } | Operation::Start { .. } | Operation::Region { .. } => true,
            _ => false,
        }
    }
}
