use crate::sea_of_nodes::nodes::index::Load;
use crate::sea_of_nodes::nodes::{Node, Nodes, Op, OpVec};
use std::ops::{Index, IndexMut};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Cfg(Node);

impl Index<Cfg> for Nodes<'_> {
    type Output = CfgData;

    fn index(&self, index: Cfg) -> &Self::Output {
        &self.cfg_data[index.0]
    }
}
impl IndexMut<Cfg> for Nodes<'_> {
    fn index_mut(&mut self, index: Cfg) -> &mut Self::Output {
        &mut self.cfg_data[index.0]
    }
}
impl Cfg {
    pub fn node(self) -> Node {
        self.0
    }
}

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
    idepth: u32,
    ///  loop_depth is computed after optimization as part of scheduling.
    loop_depth: u32,
    anti: Option<Load>,
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

impl Node {
    pub fn to_cfg(self, ops: &OpVec) -> Option<Cfg> {
        match ops[self] {
            Op::CProj(_)
            | Op::If(_)
            | Op::Loop
            | Op::Region
            | Op::Return
            | Op::Start(_)
            | Op::Stop
            | Op::XCtrl => Some(Cfg(self)),
            Op::Constant(_)
            | Op::Add
            | Op::Sub
            | Op::Mul
            | Op::Div
            | Op::Minus
            | Op::Scope(_)
            | Op::Bool(_)
            | Op::Not
            | Op::Proj(_)
            | Op::Phi(_)
            | Op::Cast(_)
            | Op::Load(_)
            | Op::Store(_)
            | Op::New(_) => None,
        }
    }
}

impl Cfg {
    pub fn cfg(self, idx: usize, sea: &Nodes) -> Option<Cfg> {
        self.0.inputs(sea)[idx].map(|n| n.to_cfg(&sea.ops).unwrap())
    }

    fn idepth(self, sea: &mut Nodes) -> u32 {
        if sea[self].idepth != 0 {
            return sea[self].idepth;
        }
        let d = match sea[self.0] {
            Op::Start(_) => return 0, // uncached
            Op::Stop | Op::Region => (0..self.0.inputs(sea).len())
                .map(|i| self.cfg(i, sea).map(|d| d.idepth(sea)))
                .flatten()
                .max()
                .unwrap(),
            _ => {
                let idom = self.idom(sea).unwrap();
                sea[idom].idepth
            }
        } + 1;
        sea[self].idepth = d;
        d
    }

    /// Return the immediate dominator of this Node and compute dom tree depth.
    pub fn idom(self, sea: &mut Nodes) -> Option<Cfg> {
        match sea[self.0] {
            Op::Loop => self.cfg(1, sea),
            Op::Region => {
                let mut lca = None;
                // Walk the LHS & RHS idom trees in parallel until they match, or either fails.
                // Because this does not cache, it can be linear in the size of the program.
                for i in 1..self.0.inputs(sea).len() {
                    lca = Some(self.cfg(i, sea).unwrap()._idom(lca, sea));
                }
                lca
            }
            Op::Start(_) | Op::Stop => None,
            _ => self.cfg(0, sea),
        }
    }

    /// Return the LCA of two idoms
    fn _idom(self, rhs: Option<Cfg>, sea: &mut Nodes) -> Cfg {
        let Some(mut rhs) = rhs else {
            return self;
        };
        let mut lhs = self;
        while lhs != rhs {
            let comp = lhs.idepth(sea) - rhs.idepth(sea);
            if comp >= 0 {
                lhs = lhs.idom(sea).unwrap()
            }
            if comp <= 0 {
                rhs = rhs.idom(sea).unwrap()
            }
        }
        lhs
    }
}
