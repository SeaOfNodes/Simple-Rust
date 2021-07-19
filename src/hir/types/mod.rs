use std::collections::HashMap;
use std::sync::Arc;

use crate::arena::Arena;
use crate::hir::location::Location;
pub use crate::hir::types::r#type::*;
pub use crate::hir::types::ty::Ty;
use crate::modules::ParsedModule;

mod ty;
mod r#type;

/// Every compilation unit has its own set of types.
/// They are interned, so that equality checks and hashing are cheap.
/// This requires every compilation unit to re-parse all imported types.
pub struct Types<'a> {
    interner: Interner<'a>,
    entries: HashMap<Ty<'a>, HashMap<&'a str, Ty<'a>>>,

    pub ty_bool: Ty<'a>,
    pub ty_inferred: Ty<'a>,
    pub ty_null_terminated_string: Ty<'a>,
    pub ty_i32: Ty<'a>,
    pub ty_i64: Ty<'a>,
    pub ty_unit: Ty<'a>,
    pub ty_void: Ty<'a>,
    pub ty_root_module: Ty<'a>,
    pub ty_std_module: Ty<'a>,
    pub ty_unresolved: Ty<'a>,
}

struct Interner<'a> {
    arena: &'a Arena<Type<'a>>,
    type_to_ty: HashMap<&'a Type<'a>, Ty<'a>>,
}

impl<'t> Interner<'t> {
    fn intern(&mut self, t: Type<'t>) -> Ty<'t> {
        *self.type_to_ty.raw_entry_mut().from_key(&t).or_insert_with(|| {
            let copy = &*self.arena.alloc(t);
            let ty = Ty::new(copy);
            (copy, ty)
        }).1
    }
}

impl<'a> Types<'a> {
    pub fn new(arena: &'a Arena<Type<'a>>) -> Self {
        let mut interner = Interner {
            arena,
            type_to_ty: Default::default(),
        };

        let ty_bool = interner.intern(Type::Boolean(None));
        let ty_inferred = interner.intern(Type::Inferred);
        let ty_null_terminated_string = interner.intern(Type::NullTerminatedString);
        let ty_i32 = interner.intern(Type::Integer(TyInteger { bits: 32, signed: true }));
        let ty_i64 = interner.intern(Type::Integer(TyInteger { bits: 64, signed: true }));
        let ty_unit = interner.intern(Type::Unit);
        let ty_void = interner.intern(Type::Void);
        let ty_root_module = interner.intern(Type::Module(TyModule {
            parent: None,
            name: "root".to_string(),
            parsed_module: None,
        }));
        let ty_std_module = interner.intern(Type::Module(TyModule {
            parent: Some(ty_root_module),
            name: "std".to_string(),
            parsed_module: None,
        }));
        let ty_unresolved = interner.intern(Type::Unresolved);

        Self {
            interner,
            entries: HashMap::from([
                (ty_root_module, HashMap::from([
                    ("std", ty_std_module)
                ])),
                (ty_std_module, HashMap::from([
                    ("bool", ty_bool),
                    ("i32", ty_i32),
                    ("i64", ty_i64),
                ]))
            ]),
            ty_bool,
            ty_inferred,
            ty_null_terminated_string,
            ty_i32,
            ty_i64,
            ty_unit,
            ty_void,
            ty_root_module,
            ty_std_module,
            ty_unresolved,
        }
    }

    pub fn entries(&mut self, ty: Ty<'a>) -> Option<&mut HashMap<&'a str, Ty<'a>>> {
        self.entries.get_mut(&ty)
    }

    pub fn lookup(&mut self, start: Ty<'a>, segments: &[&str]) -> Option<Ty<'a>> {
        let mut ty = start;
        for segment in segments.iter() {
            if let Some(&found) = self.entries(ty).and_then(|it| it.get(segment)) {
                ty = found;
            } else {
                return None;
            }
        }
        Some(ty)
    }

    pub fn get_pointer(&mut self, ty: Ty<'a>) -> Ty<'a> {
        self.interner.intern(Type::Pointer(ty))
    }

    pub fn get_function_signature(&mut self, parameters: Vec<Ty<'a>>, return_type: Ty<'a>) -> Ty<'a> {
        self.interner.intern(Type::FunctionSignature(TyFunctionSignature { parameters, return_type }))
    }

    pub fn get_enum(&mut self, location: Location<'a>, parent: Option<Ty<'a>>, members: Option<Vec<(String, i64)>>) -> Ty<'a> {
        // TODO what should the default enum type be?
        let parent = parent.unwrap_or_else(|| self.ty_i32);
        self.interner.intern(Type::Enum(TyEnum { location, parent, members }))
    }

    pub fn get_struct(&mut self, location: Location<'a>) -> Ty<'a> {
        self.interner.intern(Type::Struct(TyStruct { location }))
    }

    pub fn get_module(&mut self, parsed_module: Arc<ParsedModule>, parent: Ty<'a>) -> Result<Ty<'a>, ()> {
        let ptr = Arc::as_ptr(&parsed_module);
        let ty = self.interner.intern(Type::Module(TyModule {
            name: parsed_module.ast.name.clone(),
            parent: Some(parent),
            parsed_module: Some(parsed_module),
        }));
        let ptr2 = Arc::as_ptr(ty.unwrap_module().parsed_module.as_ref().unwrap());
        if ptr.eq(&ptr2) {
            Ok(ty)
        } else {
            Err(())
        }
    }

    pub fn is_assignable_to(&self, source: Ty<'a>, target: Ty<'a>) -> bool {
        if source == target {
            true
        } else if let Type::Enum(enu) = &*source {
            self.is_assignable_to(enu.parent, target)
        } else {
            false
        }
    }

    pub fn byte_align(&self, ty: Ty) -> usize {
        match &*ty {
            Type::Error => { unreachable!() }
            Type::Integer(_) => { self.byte_size(ty) }
            Type::Boolean(_) => { self.byte_size(ty) }
            Type::NullTerminatedString => { self.byte_size(ty) }
            Type::Function(_) => { todo!() }
            Type::Unit => { self.byte_size(ty) }
            Type::Pointer(_) => { self.byte_size(ty) }
            Type::Enum(_) => { self.byte_size(ty) }
            Type::Struct(stru) => { todo!("stru.align") }
            Type::Inferred => { unreachable!() }
            Type::Void => { 0 }
            Type::DuplicateDefinition => { 0 }
            Type::Module(_) => { 0 }
            Type::FunctionSignature(_) => { unreachable!() }
            Type::Unresolved => { unreachable!() }
        }.max(1)
    }

    pub fn byte_size(&self, ty: Ty) -> usize {
        match &*ty {
            Type::Error => { unreachable!() }
            Type::Integer(int) => {
                // (int.bits as usize).next_power_of_two()
                8 // TODO use real size after we handle 32 bit mov
            }
            Type::Boolean(_) => {
                8 // TODO reduce size once we have the correct mov instructions
            }
            Type::NullTerminatedString => { 8 }
            Type::Function(_) => { todo!() }
            Type::Unit => { 0 }
            Type::Pointer(_) => { 8 }
            Type::Enum(enu) => { self.byte_size(enu.parent) }
            Type::Struct(stru) => { todo!("stru.size") }
            Type::Inferred => { unreachable!() }
            Type::Void => { 0 }
            Type::DuplicateDefinition => { 0 }
            Type::Module(_) => { 0 }
            Type::FunctionSignature(_) => { unreachable!() }
            Type::Unresolved => { unreachable!() }
        }
    }
}
