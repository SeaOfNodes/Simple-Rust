use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;

use crate::datastructures::id::Id;
use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::{Node, NodeId, Nodes};

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
            Node::Constant(_) => format!("Con_{}", node),
            _ => format!("{}{}", self[node].label(), node),
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

        if self.visited.borrow().get(node) {
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
            Node::Add => binary("+"),
            Node::Sub => binary("-"),
            Node::Mul => binary("*"),
            Node::Div => binary("/"),
            Node::Bool(op) => binary(op.str()),
            Node::Return => write!(f, "return {};", input(1)),
            Node::Constant(ty) => write!(f, "{}", ty),
            n @ Node::Start { .. } => write!(f, "{}", n.label()),
            Node::Minus => write!(f, "(-{})", input(1)),
            Node::Scope(_) => {
                write!(f, "Scope[ ")?;
                let names = nodes.scope_reverse_names(node);

                for (i, n) in names.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}:", n.as_ref().unwrap())?;
                    let mut node = inputs[i];
                    while node.is_some_and(|n| matches!(&nodes[n], Node::Scope(_))) {
                        write!(f, "Lazy_")?;
                        node = nodes.inputs[node.unwrap()][i];
                    }
                    write!(f, "{}", print(node))?;
                }
                write!(f, "]")
            }
            Node::Not => write!(f, "(!{})", input(1)),
            Node::Proj(proj) => write!(f, "{}", proj.label),
            Node::If => write!(f, "if( {} )", input(1)),
            Node::Phi(_) => {
                if nodes.in_progress(nodes.inputs[node][0].unwrap()) {
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
            n @ Node::Region { .. } | n @ Node::Loop => write!(f, "{}{}", n.label(), node.index()),
            Node::Stop => {
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
        }
    }
}
