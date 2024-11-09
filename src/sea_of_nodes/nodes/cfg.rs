use crate::sea_of_nodes::nodes::index::Load;
use crate::sea_of_nodes::nodes::{Node, Op, OpVec};

pub struct Cfg(Node);

/// Control Flow Graph Nodes
///
///  CFG nodes have a immediate dominator depth (idepth) and a loop nesting
///  depth(loop_depth).
pub struct CfgData {
    ///  idepth is computed lazily upon first request, and is valid even in the
    ///  Parser, and is used by peepholes during parsing and afterward.
    idepth: u32,
    ///  loop_depth is computed after optimization as part of scheduling.
    loop_depth: u32,
    anti: Option<Load>,
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
