use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;

use crate::datastructures::id::Id;
use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::node::IfOp;
use crate::sea_of_nodes::nodes::{Node, Nodes, Op};
use crate::sea_of_nodes::types::Type;

pub struct PrintNodes<'a, 't> {
    node: Option<Node>,
    sea: &'a Nodes<'t>,
    visited: RefCell<IdSet<Node>>,
}

impl Display for PrintNodes<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        PrintNodes2 {
            node: self.node,
            sea: self.sea,
            visited: &self.visited,
        }
        .fmt(f)
    }
}

pub struct PrintNodes2<'a, 'b, 't> {
    node: Option<Node>,
    sea: &'a Nodes<'t>,
    visited: &'b RefCell<IdSet<Node>>,
}

impl Node {
    pub(crate) fn print<'a, 't>(self, sea: &'a Nodes<'t>) -> PrintNodes<'a, 't> {
        PrintNodes {
            node: Some(self),
            sea,
            visited: RefCell::new(IdSet::zeros(sea.len())),
        }
    }

    // Unique label for graph visualization, e.g. "Add12" or "Region30" or "EQ99"
    pub fn unique_name(self, sea: &Nodes) -> String {
        match &sea[self] {
            Op::Constant(_) => format!("Con_{self}"),
            Op::Cast(_) => format!("Cast_{self}"),
            // Get rid of $ as graphviz doesn't like it
            _ => format!("{}{}", sea[self].label().replace('$', ""), self),
        }
    }
}

impl Display for PrintNodes2<'_, '_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some(node) = self.node else {
            return write!(f, "<?>");
        };

        let sea = self.sea;
        let inputs = &sea.inputs[node];

        if self.visited.borrow().get(node) && !matches!(&sea[node], Op::Constant(_)) {
            return write!(f, "{}", sea[node].label());
        }
        self.visited.borrow_mut().add(node);

        let print = |node| Self {
            node,
            sea,
            visited: self.visited,
        };
        let input = |index| print(inputs[index]);
        let mut binary = |op: &str| write!(f, "({}{}{})", input(1), op, input(2));

        match &sea[node] {
            _ if node.is_dead(sea) => write!(f, "{}:DEAD", node.unique_name(sea)),
            Op::Add | Op::AddF => binary("+"),
            Op::Sub | Op::SubF => binary("-"),
            Op::Mul | Op::MulF => binary("*"),
            Op::Div | Op::DivF => binary("/"),
            Op::And => binary("&"),
            Op::Or => binary("|"),
            Op::Xor => binary("^"),
            Op::Sar => binary(">>"),
            Op::Shl => binary("<<"),
            Op::Shr => binary(">>>"),
            Op::Bool(op) => binary(op.str()),
            Op::Return => write!(f, "return {};", input(1)),
            Op::Constant(ty) => write!(f, "{}", ty),
            Op::XCtrl => write!(f, "Xctrl"),
            n @ Op::Start { .. } => write!(f, "{}", n.label()),
            Op::Minus | Op::MinusF => write!(f, "(-{})", input(1)),
            Op::RoundF32 => write!(f, "((f32){})", input(1)),
            Op::ToFloat => write!(f, "((flt){})", input(1)),
            Op::ReadOnly => write!(f, "((const){})", input(1)),
            Op::Scope(_) => {
                write!(f, "Scope[ ")?;
                let names = sea.to_scope(node).unwrap().reverse_names(sea);

                for (i, &n) in names.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}:", n.unwrap())?;
                    let mut node = inputs[i];
                    while node.is_some_and(|n| matches!(&sea[n], Op::Scope(_))) {
                        write!(f, "Lazy_")?;
                        node = sea.inputs[node.unwrap()][i];
                    }
                    write!(f, "{}", print(node))?;
                }
                write!(f, "]")
            }
            Op::ScopeMin => todo!(),
            Op::Struct => todo!(),
            Op::Not => write!(f, "(!{})", input(1)),
            Op::Proj(proj) | Op::CProj(proj) => write!(f, "{}", proj.label),
            Op::If(op) => match op {
                IfOp::Cond => write!(f, "if( {} )", input(1)),
                IfOp::Never => write!(f, "Never"),
            },
            Op::Phi(_) => {
                if !sea.instanceof_region(inputs[0])
                    || Nodes::in_progress(&sea.ops, &sea.inputs, sea.inputs[node][0].unwrap())
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
                if let ret @ Some(_) = node.unique_input(sea) {
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
            Op::Load(l) => write!(f, ".{}", l.name),
            Op::Store(s) => write!(f, ".{}={};", s.name, input(3)),
            Op::Cfg => unreachable!(),
        }
    }
}
