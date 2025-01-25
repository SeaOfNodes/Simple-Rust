use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::node::{Load, TypedNode};
use crate::sea_of_nodes::nodes::{Cfg, Node, Nodes, Op};
use std::collections::HashSet;

/// Control Flow Graph Nodes
///
///  CFG nodes have a immediate dominator depth (idepth) and a loop nesting
///  depth(loop_depth).
pub struct CfgData {
    /// Immediate dominator tree depth, used to approximate a real IDOM during
    /// parsing where we do not have the whole program, and also peepholes
    /// change the CFG incrementally.
    ///
    /// See <a href="https://en.wikipedia.org/wiki/Dominator_(graph_theory)">Wikipedia: Dominator</a>
    pub idepth: u32,
    ///  loop_depth is computed after optimization as part of scheduling.
    pub loop_depth: u32,
    pub anti: Option<Load>,
}
impl CfgData {
    pub fn new() -> Self {
        Self {
            idepth: 0,
            loop_depth: 0,
            anti: None,
        }
    }
}

impl Cfg {
    pub fn cfg(self, idx: usize, sea: &Nodes) -> Option<Cfg> {
        self.inputs(sea)[idx].map(|n| n.to_cfg(sea).unwrap())
    }

    pub fn idepth(self, sea: &mut Nodes) -> u32 {
        if sea.cfg[self].idepth != 0 {
            return sea.cfg[self].idepth;
        }
        let d = match sea[*self] {
            Op::Start(_) => return 0, // uncached
            Op::Stop | Op::Region => (0..self.inputs(sea).len())
                .map(|i| self.cfg(i, sea).map(|d| d.idepth(sea)))
                .flatten()
                .max()
                .unwrap(),
            _ => self.idom(sea).unwrap().idepth(sea),
        } + 1;
        sea.cfg[self].idepth = d;
        d
    }

    pub fn idom(self, sea: &mut Nodes) -> Option<Cfg> {
        self.idom_dep(None, sea)
    }

    /// Return the immediate dominator of this Node and compute dom tree depth.
    pub fn idom_dep(self, dep: Option<Node>, sea: &mut Nodes) -> Option<Cfg> {
        match sea[*self] {
            Op::Loop => self.cfg(1, sea),
            Op::Region => {
                let mut lca = None;
                // Walk the LHS & RHS idom trees in parallel until they match, or either fails.
                // Because this does not cache, it can be linear in the size of the program.
                for i in 1..self.inputs(sea).len() {
                    lca = Some(self.cfg(i, sea).unwrap().idom_2(lca, dep, sea));
                }
                lca
            }
            Op::Start(_) | Op::Stop => None,
            _ => self.cfg(0, sea),
        }
    }

    /// Return the LCA of two idoms
    pub fn idom_2(self, rhs: Option<Cfg>, dep: Option<Node>, sea: &mut Nodes) -> Cfg {
        let Some(mut rhs) = rhs else {
            return self;
        };
        let mut lhs = self;
        while lhs != rhs {
            let comp = lhs.idepth(sea) as i64 - rhs.idepth(sea) as i64;
            if comp >= 0 {
                if let Some(dep) = dep {
                    lhs.add_dep(dep, sea);
                }
                lhs = lhs.idom(sea).unwrap()
            }
            if comp <= 0 {
                if let Some(dep) = dep {
                    rhs.add_dep(dep, sea);
                }
                rhs = rhs.idom(sea).unwrap()
            }
        }
        lhs
    }

    // Loop nesting depth
    pub fn loop_depth(self, sea: &mut Nodes) -> u32 {
        if sea.cfg[self].loop_depth == 0 {
            sea.cfg[self].loop_depth = match self.downcast(&sea.ops) {
                TypedNode::Start(_) | TypedNode::Stop(_) => 1,
                TypedNode::Loop(l) => {
                    let d = sea.cfg[l.entry(sea)].loop_depth + 1; // Entry depth plus one

                    // One-time tag loop exits
                    let mut idom = l.back(sea);
                    while idom != self {
                        // Walk idom in loop, setting depth
                        sea.cfg[idom].loop_depth = d;
                        // Loop exit hits the CProj before the If, instead of jumping from
                        // Region directly to If.

                        if let Some(proj) = idom.to_cproj(sea) {
                            let i = proj.inputs(sea)[0].unwrap();
                            debug_assert!(i.is_if(sea), "expected loop exit test");

                            // Find the loop exit CProj, and set loop_depth
                            for use_ in 0..sea.outputs[i].len() {
                                let use_ = sea.outputs[i][use_];
                                if use_ != Node::DUMMY {
                                    if let Some(proj2) = use_.to_cproj(sea) {
                                        let proj2 = proj2.to_cfg();
                                        if proj2 != idom {
                                            sea.cfg[proj2].loop_depth = d - 1;
                                        }
                                    }
                                }
                            }
                        }

                        idom = idom.idom(sea).unwrap();
                    }
                    d
                }
                TypedNode::Region(_) => self.cfg(1, sea).unwrap().loop_depth(sea),
                _ => self.cfg(0, sea).unwrap().loop_depth(sea),
            }
        }
        sea.cfg[self].loop_depth
    }

    pub fn walk_unreach(
        self,
        visit: &mut IdSet<Node>,
        unreach: &mut HashSet<Cfg>,
        sea: &mut Nodes,
    ) {
        if visit.get(*self) {
            return;
        }
        visit.add(*self);

        match self.downcast(&sea.ops) {
            TypedNode::If(_) => {
                for proj in &sea.outputs[self] {
                    let proj = proj.to_cproj(sea).unwrap().to_cfg();
                    if sea.cfg[proj].loop_depth == 0 {
                        unreach.insert(proj);
                    }
                }
                self.cfg(0, sea).unwrap().walk_unreach(visit, unreach, sea);
            }
            TypedNode::Region(_) | TypedNode::Loop(_) => {
                for i in 1..self.inputs(sea).len() {
                    self.cfg(i, sea).unwrap().walk_unreach(visit, unreach, sea);
                }
            }
            TypedNode::Start(_) => {}
            _ => self.cfg(0, sea).unwrap().walk_unreach(visit, unreach, sea),
        }

        unreach.remove(&self); // Since we reached here... Node was not unreachable
    }
}
