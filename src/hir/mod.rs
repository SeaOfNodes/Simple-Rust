//!
//! A Simple Graph-Based Intermediate Representation:
//! https://www.oracle.com/technetwork/java/javase/tech/c2-ir95-150110.pdf
//! https://wiki.openjdk.org/display/HotSpot/C2+IR+Graph+and+Nodes
//!
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};

use crate::hir::id::Id;
use crate::hir::node::Node;
use crate::hir::operation::{ConstantValue, Operation};
use crate::hir::scopes::{Local, Scopes};
use crate::hir::types::{Ty, Types};
use crate::syntax::ast::{Block, Expression, Function, Statement};

mod dominators;
pub mod id;
pub mod location;
pub mod lower;
pub mod node;
pub mod operation;
pub mod scopes;
pub mod types;

pub struct Hir<'t> {
    nodes: Vec<Node<'t>>,
    worklist_queue: VecDeque<Id>,
    worklist_set: Vec<bool>,
    root: Id,
    pub symbol: String,
    pub is_extern: bool,
}

impl<'t> Hir<'t> {
    pub fn dummy() -> Self {
        Self {
            nodes: vec![],
            worklist_queue: Default::default(),
            worklist_set: vec![],
            root: Id::from(0),
            symbol: "dummy".to_string(),
            is_extern: false,
        }
    }
    pub fn from_source(
        function: &Function,
        types: &mut Types<'t>,
        scopes: &mut Scopes<'t>,
    ) -> Result<Hir<'t>, ()> {
        let mut hir = Hir {
            nodes: vec![],
            worklist_queue: Default::default(),
            worklist_set: vec![],
            root: Id::from(0),
            symbol: function.name.value.clone(),
            is_extern: function.body.is_none(),
        };

        let root = hir.add_node(None, Operation::Root {}, types.ty_unit, vec![]);

        let start = hir.add_node(Some(root), Operation::Start {}, types.ty_unit, vec![]);

        for parameter in function.parameters.iter() {
            let ty = scopes
                .lookup_type(Some(&hir), types, &parameter.typ)
                .ok_or(())?;
            let name = parameter.name.value.clone();
            let parameter = hir.add_node(
                Some(start),
                Operation::Parameter { name: name.clone() },
                ty,
                vec![],
            );
            scopes.add_local(
                name,
                Local {
                    definition: parameter,
                    last_relevant_memory: start,
                },
            );
        }

        if let Some(block) = &function.body {
            hir.add_block(types, scopes, block)?;
        }

        Ok(hir)
    }

    fn add_block(
        &mut self,
        types: &mut Types<'t>,
        scopes: &mut Scopes,
        block: &Block,
    ) -> Result<(), ()> {
        scopes.begin_scope();
        let result = self.add_statements(types, scopes, &block.statements);
        scopes.end_scope();
        result
    }

    fn add_statements(
        &mut self,
        types: &mut Types<'t>,
        scopes: &mut Scopes,
        statements: &[Statement],
    ) -> Result<(), ()> {
        for statement in statements {
            self.add_statement(types, scopes, statement)?;
        }
        Ok(())
    }

    fn add_statement(
        &mut self,
        types: &mut Types<'t>,
        scopes: &mut Scopes,
        statement: &Statement,
    ) -> Result<(), ()> {
        match statement {
            Statement::Expression(_) => {}
            Statement::Return(ret) => {
                let value = self.add_expression(types, scopes, &ret.value)?;
                let dummy = value;
                self.add_node(
                    scopes.control,
                    Operation::Return {
                        value: Some(value),
                        io: dummy,
                        memory: dummy,
                        frame_pointer: dummy,
                        return_address: dummy,
                    },
                    types.ty_void,
                    vec![],
                );
            }
            Statement::If(_) => {}
            Statement::Var(_) => {}
        }
        Ok(())
    }

    fn add_expression(
        &mut self,
        types: &mut Types<'t>,
        scopes: &mut Scopes,
        expression: &Expression,
    ) -> Result<Id, ()> {
        let id = match expression {
            Expression::Immediate(immediate) => self.add_node(
                None,
                Operation::Constant {
                    value: ConstantValue::Integer(*immediate),
                },
                types.ty_i64,
                vec![],
            ),
            _ => todo!("Implement {:?}", expression),
        };
        Ok(id)
    }

    fn add_node(
        &mut self,
        control: Option<Id>,
        operation: Operation,
        ty: Ty<'t>,
        outputs: Vec<Id>,
    ) -> Id {
        let id = Id::from(self.nodes.len());
        self.nodes.push(Node {
            id,
            control,
            operation,
            origin: None,
            ty,
            outputs,
        });
        id
    }
}

fn update_node(id: Id, hir: &mut Hir) {
    let node = hir.nodes[id.index()].clone();

    // if all operands constant: replace with constant
    match &node.operation {
        Operation::Constant { .. } => {}
        Operation::Call {
            function,
            arguments,
        } => {
            // lookup function and check if it can be constant folded
            if true {
                let all_constant = arguments.iter().all(|it| {
                    matches!(hir.nodes[it.index()].operation, Operation::Constant { .. })
                });
                return;
            }
        }
        Operation::Region { .. } => {
            // if no control dependence
            // delete this region and everyone that depends on us
        }
        Operation::Phi { .. } => {}
        Operation::If { .. } => {
            // if condition is constant
            // set taken branch control to self.control
            // remove not taken branch control
            // enqueue both branches
        }
        Operation::Projection { .. } => {}
        Operation::Parameter { .. } => {}
        Operation::Start { .. } => {}
        Operation::Return { .. } => {}
        Operation::Local {} => {}
        Operation::Global {} => {}
        Operation::Load { .. } => {}
        Operation::Store { .. } => {}
        Operation::MergeMemory { .. } => {}
        Operation::Root { .. } => {}
    }
}

fn foo(hir: &mut Hir) {
    while let Some(id) = hir.worklist_queue.pop_front() {
        hir.worklist_set[id.index()] = false;
        update_node(id, hir);
    }
}

impl<'t> Display for Hir<'t> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Hir root={}", self.root)?;
        for n in self.nodes.iter() {
            writeln!(f, "{}", n)?;
        }
        Ok(())
    }
}
