use std::cmp::Ordering;
use std::collections::HashMap;

use crate::hir::id::Id;
use crate::hir::node::Node;
use crate::hir::operation::{ConstantValue, Operation};
use crate::hir::Hir;
use crate::lir::{BlockId, Lir, Register};

impl<'t> Hir<'t> {
    pub fn lower(&self) -> Lir<'t> {
        // println!("Lowering {}", self);
        let mut lir = Lir::new();

        struct BB {
            label: BlockId,
            instructions: Vec<Id>,
        }

        let mut basic_blocks = self
            .nodes
            .iter()
            .filter(|it| Node::starts_basic_block(it) && it.id != self.root)
            .map(|it| {
                (
                    it.id,
                    BB {
                        label: lir.new_block_id(),
                        instructions: Vec::from([it.id]),
                    },
                )
            })
            .collect::<HashMap<_, _>>();

        if basic_blocks.len() != 1 {
            todo!();
        }
        let start = *basic_blocks.iter().next().unwrap().0;
        for x in self.nodes.iter() {
            if !x.starts_basic_block() {
                basic_blocks
                    .get_mut(&start)
                    .unwrap()
                    .instructions
                    .push(x.id);
            }
        }

        // TODO for this to work correctly we need to visit basic blocks in domination order
        let mut result: Vec<Option<Register>> = vec![None; self.nodes.len()];

        for (id, bb) in basic_blocks {
            // println!("lowering block {:?} with instructions {:?}", id, bb.instructions);

            let block = bb.label;
            let mut blocked = bb.instructions;
            let mut available = Vec::new();

            for _ in 0..blocked.len() {
                // find available instructions
                let mut i = 0;
                while i < blocked.len() {
                    let mut is_available = true;
                    self.nodes[blocked[i].index()].for_each_dependency(|d| {
                        if blocked.contains(&d) {
                            is_available = false;
                        }
                    });
                    if is_available {
                        let removed = blocked.swap_remove(i);
                        available.push(removed);
                    } else {
                        i += 1;
                    }
                }

                // pick the best one
                let best_index = available
                    .iter()
                    .enumerate()
                    .max_by(|(_, a), (_, b)| {
                        let na = &self.nodes[a.index()];
                        let nb = &self.nodes[b.index()];

                        match (&na.operation, &nb.operation) {
                            // first:
                            _ if na.starts_basic_block() => Ordering::Greater,
                            _ if nb.starts_basic_block() => Ordering::Less,
                            (Operation::Phi { .. }, _) => Ordering::Greater,
                            (_, Operation::Phi { .. }) => Ordering::Less,
                            (Operation::Call { .. }, _) => Ordering::Greater,
                            (_, Operation::Call { .. }) => Ordering::Less,
                            // last:
                            (Operation::Return { .. }, _) => Ordering::Less,
                            (_, Operation::Return { .. }) => Ordering::Greater,
                            (Operation::If { .. }, _) => Ordering::Less,
                            (_, Operation::If { .. }) => Ordering::Greater,
                            _ => nb.id.index().cmp(&na.id.index()), // arbitrarily pick the smaller id
                        }
                    })
                    .unwrap()
                    .0;
                let best_id = available.swap_remove(best_index);

                // lower it
                let best = &self.nodes[best_id.index()];

                match best.operation {
                    Operation::Root { .. } => unreachable!(),
                    Operation::Start { .. } => {}
                    Operation::Constant { value } => match value {
                        ConstantValue::Integer(i) => {
                            let reg = lir.imm64(block, i, best.origin);
                            result[best_id.index()] = Some(reg);
                        }
                    },
                    Operation::Return { value, .. } => {
                        if let Some(value) = value {
                            let src = result[value.index()].unwrap();
                            lir.mov(block, Lir::RETURN_REGISTER, src, best.origin);
                        }

                        // leave
                        lir.ret(block, best.origin);
                    }
                    _ => todo!("implement lowering for {}", best),
                }
            }
            assert!(blocked.is_empty());
        }

        // println!("{lir}");

        lir
    }
}
