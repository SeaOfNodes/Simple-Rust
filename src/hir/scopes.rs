use std::collections::HashMap;

use crate::hir::id::Id;
use crate::hir::types::{Ty, Types};
use crate::hir::Hir;
use crate::syntax::ast::Type;

pub struct ItemPath<'a, 'b> {
    pub segments: &'a [&'b str],
}

pub struct Scopes<'t> {
    pub control: Option<Id>,
    module: Ty<'t>,
    scopes: Vec<LocalScope>,
}

impl<'t> Scopes<'t> {
    pub fn new(module: Ty<'t>) -> Self {
        Self {
            control: None,
            module,
            scopes: Vec::new(),
        }
    }

    pub fn lookup(
        &mut self,
        hir: Option<&Hir<'t>>,
        types: &mut Types<'t>,
        path: &ItemPath,
    ) -> Option<Ty<'t>> {
        if let Some(hir) = hir {
            if let Some((first, rest)) = path.segments.split_first() {
                if let Some(local) = self.lookup_local(first) {
                    let ty = hir.nodes[local.definition.index()].ty;
                    return types.lookup(ty, rest);
                }
            }
        }

        return types
            .lookup(self.module, &path.segments)
            .or_else(|| types.lookup(types.ty_root_module, &path.segments))
            .or_else(|| types.lookup(types.ty_std_module, &path.segments));
    }

    pub fn lookup_type(
        &mut self,
        hir: Option<&Hir<'t>>,
        types: &mut Types<'t>,
        typ: &Type,
    ) -> Option<Ty<'t>> {
        match typ {
            Type::Identifier(i) => {
                let segments = &[i.value.as_str()];
                let path = ItemPath { segments };
                self.lookup(hir, types, &path)
            }
            Type::Pointer(t) => self
                .lookup_type(hir, types, t)
                .map(|ty| types.get_pointer(ty)),
        }
    }

    pub fn lookup_local(&mut self, name: &str) -> Option<&Local> {
        for scope in self.scopes.iter().rev() {
            if let Some(local) = scope.locals.get(name) {
                return Some(local);
            }
        }
        return None;
    }

    pub fn add_local(&mut self, name: String, local: Local) {
        let result = self.scopes.last_mut().unwrap().locals.insert(name, local);
        assert!(result.is_none());
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(LocalScope {
            locals: Default::default(),
        });
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop().unwrap();
    }
}

struct LocalScope {
    locals: HashMap<String, Local>,
}

pub struct Local {
    pub definition: Id,
    pub last_relevant_memory: Id,
}
