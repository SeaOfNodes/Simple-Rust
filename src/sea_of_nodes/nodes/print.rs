use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;

use crate::datastructures::id::Id;
use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::node::MemOpKind;
use crate::sea_of_nodes::nodes::{NodeId, Nodes, Op};
use crate::sea_of_nodes::types::Type;

pub struct PrintNodes<'a, 't> {
    node: Option<NodeId>,
    nodes: &'a Nodes<'t>,
    visited: RefCell<IdSet<NodeId>>,
}

impl Display for PrintNodes<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        PrintNodes2 {
            node: self.node,
            nodes: self.nodes,
            visited: &self.visited,
        }
        .fmt(f)
    }
}

pub struct PrintNodes2<'a, 'b, 't> {
    node: Option<NodeId>,
    nodes: &'a Nodes<'t>,
    visited: &'b RefCell<IdSet<NodeId>>,
}

impl<'t> Nodes<'t> {
    pub(crate) fn print(&self, node: Option<NodeId>) -> PrintNodes<'_, 't> {
        PrintNodes {
            node,
            nodes: self,
            visited: RefCell::new(IdSet::zeros(self.len())),
        }
    }

    // Unique label for graph visualization, e.g. "Add12" or "Region30" or "EQ99"
    pub fn unique_name(&self, node: NodeId) -> String {
        match &self[node] {
            Op::Constant(_) => format!("Con_{node}"),
            Op::Cast(_) => format!("Cast_{node}"),
            // Get rid of $ as graphviz doesn't like it
            _ => format!("{}{}", self[node].label().replace('$', ""), node),
        }
    }
}

impl Display for PrintNodes2<'_, '_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(node) = self.node else {
            return write!(f, "<?>");
        };

        let nodes = self.nodes;
        let inputs = &nodes.inputs[node];

        if self.visited.borrow().get(node) && !matches!(&nodes[node], Op::Constant(_)) {
            return write!(f, "{}", nodes[node].label());
        }
        self.visited.borrow_mut().add(node);

        let print = |node| Self {
            node,
            nodes,
            visited: self.visited,
        };
        let input = |index| print(inputs[index]);
        let mut binary = |op: &str| write!(f, "({}{}{})", input(1), op, input(2));

        match &nodes[node] {
            _ if nodes.is_dead(node) => write!(f, "{}:DEAD", nodes.unique_name(node)),
            Op::Add => binary("+"),
            Op::Sub => binary("-"),
            Op::Mul => binary("*"),
            Op::Div => binary("/"),
            Op::Bool(op) => binary(op.str()),
            Op::Return => write!(f, "return {};", input(1)),
            Op::Constant(ty) => write!(f, "{}", ty),
            n @ Op::Start { .. } => write!(f, "{}", n.label()),
            Op::Minus => write!(f, "(-{})", input(1)),
            Op::Scope(_) => {
                write!(f, "Scope[ ")?;
                let names = nodes.to_scope(node).unwrap().reverse_names(nodes);

                for (i, &n) in names.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}:", n.unwrap())?;
                    let mut node = inputs[i];
                    while node.is_some_and(|n| matches!(&nodes[n], Op::Scope(_))) {
                        write!(f, "Lazy_")?;
                        node = nodes.inputs[node.unwrap()][i];
                    }
                    write!(f, "{}", print(node))?;
                }
                write!(f, "]")
            }
            Op::Not => write!(f, "(!{})", input(1)),
            Op::Proj(proj) => write!(f, "{}", proj.label),
            Op::If => write!(f, "if( {} )", input(1)),
            Op::Phi(_) => {
                if !nodes.instanceof_region(inputs[0])
                    || Nodes::in_progress(
                        &nodes.nodes,
                        &nodes.inputs,
                        nodes.inputs[node][0].unwrap(),
                    )
                {
                    write!(f, "Z")?;
                }
                write!(f, "Phi(")?;
                for (i, input) in inputs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    if input.is_none() {
                        write!(f, "____")?;
                    } else {
                        write!(f, "{}", print(*input))?;
                    }
                }
                write!(f, ")")
            }
            n @ Op::Region { .. } | n @ Op::Loop => write!(f, "{}{}", n.label(), node.index()),
            Op::Stop => {
                if let ret @ Some(_) = nodes.unique_input(node) {
                    write!(f, "{}", print(ret))
                } else {
                    write!(f, "Stop[ ")?;
                    for ret in inputs {
                        write!(f, "{} ", print(*ret))?;
                    }
                    write!(f, "]")
                }
            }
            n @ Op::Cast(_) => {
                write!(f, "{}{}", n.label(), input(1))
            }
            Op::New(t) => {
                let Type::Pointer(p) = &**t else {
                    unreachable!()
                };
                write!(f, "new {}", p.to.str())
            }
            Op::MemOp(m) => match m.kind {
                MemOpKind::Load { .. } => write!(f, ".{}", m.name),
                MemOpKind::Store => write!(f, ".{}={};", m.name, input(3)),
            },
        }
    }
}
