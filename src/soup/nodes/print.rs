use std::fmt;
use std::fmt::Display;

use crate::data::id_set::IdSet;
use crate::soup::nodes::{Node, NodeId, Nodes};

pub struct PrintNodes<'a, 't> {
    node: Option<NodeId>,
    nodes: &'a Nodes<'t>,
}

impl Display for PrintNodes<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut visited = IdSet::zeros(self.nodes.nodes.len());
        self.nodes.fmt(self.node, f, &mut visited)
    }
}
impl<'t> Nodes<'t> {
    pub(crate) fn print(&self, node: Option<NodeId>) -> PrintNodes {
        PrintNodes { node, nodes: self }
    }

    fn fmt(
        &self,
        node: Option<NodeId>,
        f: &mut fmt::Formatter,
        visited: &mut IdSet<NodeId>,
    ) -> fmt::Result {
        let Some(node) = node else {
            return write!(f, "<?>");
        };
        visited.add(node);
        let inputs = &self.inputs[node];
        match &self[node] {
            _ if self.is_dead(node) => write!(f, "{}:DEAD", self.unique_name(node)),
            Node::Return => {
                write!(f, "return ")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, ";")
            }
            Node::Add => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "+")?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Constant(ty) => write!(f, "{}", ty),
            n @ Node::Start { .. } => write!(f, "{}", n.label()),
            Node::Sub => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "-")?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Mul => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "*")?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Div => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "*")?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Minus => {
                write!(f, "(-")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, ")")
            }
            n @ Node::Scope(scope) => {
                write!(f, "{}", n.label())?;
                for s in &scope.scopes {
                    write!(f, "[")?;
                    for (i, (name, input)) in s.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{name}:")?;
                        self.fmt(inputs[*input], f, visited)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Node::Bool(op) => {
                write!(f, "(")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, "{}", op.str())?;
                self.fmt(inputs[2], f, visited)?;
                write!(f, ")")
            }
            Node::Not => {
                write!(f, "(!")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, ")")
            }
            Node::Proj(proj) => {
                write!(f, "{}", proj.label)
            }
            Node::If => {
                write!(f, "if( ")?;
                self.fmt(inputs[1], f, visited)?;
                write!(f, " )")
            }
            Node::Phi(_) => {
                write!(f, "Phi(")?;
                for (i, input) in inputs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    self.fmt(*input, f, visited)?;
                }
                write!(f, ")")
            }
            n @ Node::Region => {
                write!(f, "{}{}", n.label(), node.index())
            }
            Node::Stop => {
                if let Some(ret) = self.unique_input(node) {
                    self.fmt(Some(ret), f, visited)
                } else {
                    write!(f, "Stop[ ")?;
                    for ret in inputs {
                        self.fmt(*ret, f, visited)?;
                        write!(f, " ")?;
                    }
                    write!(f, "]")
                }
            }
        }
    }
}
