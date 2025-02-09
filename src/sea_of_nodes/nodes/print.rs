use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;

use crate::datastructures::id_set::IdSet;
use crate::sea_of_nodes::nodes::node::IfOp;
use crate::sea_of_nodes::nodes::{Node, Nodes, Op};

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
            Op::ToFloat => write!(f, "(flt){}", input(1)),
            Op::ReadOnly => write!(f, "((const){})", input(1)),
            Op::Scope(_) => {
                //         sb.append("Scope[ ");
                //         int j=1;
                //         for( int i=0; i<nIns(); i++ ) {
                //             if( j < _lexSize._len && i == _lexSize.at(j) ) { sb.append("| "); j++; }
                //             Var v = _vars.get(i);
                //         }
                write!(f, "Scope[ ")?;
                let op = &sea[node.to_scope(sea).unwrap()];
                let mut j = 1;
                for i in 0..inputs.len() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if Some(&i) == op.lex_size.get(j) {
                        write!(f, "| ")?;
                        j += 1;
                    }
                    let v = &op.vars[i];
                    // differs from java. We can't resolve the type here
                    write!(f, "{} ", v.ty)?;
                    if v.final_field {
                        write!(f, "!")?;
                    }
                    write!(f, "{}=", v.name)?;
                    let mut n = inputs[i];
                    while let Some(loop_) = n.and_then(|n| n.to_scope(sea)) {
                        write!(f, "Lazy_")?;
                        n = loop_.inputs(sea)[i];
                    }
                    if n.is_none() {
                        write!(f, "___")?;
                    } else {
                        write!(f, "{}", print(n))?;
                    }
                }
                //         sb.setLength(sb.length()-2);
                write!(f, "]")
            }
            Op::ScopeMin => {
                write!(f, "MEM[")?;
                for j in 2..inputs.len() {
                    write!(f, " {j}:")?;
                    let mut n = inputs[j];
                    while let Some(loop_) = n.and_then(|n| n.to_scope(sea)) {
                        write!(f, "Lazy_")?;
                        n = loop_.inputs(sea)[j];
                    }
                    if n.is_none() {
                        write!(f, "___")?;
                    } else {
                        write!(f, "{}", print(n))?;
                    }
                }
                write!(f, "]")
            }
            Op::Struct(_) => todo!(),
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
            n @ Op::Region { .. } | n @ Op::Loop => write!(f, "{}", n.label()),
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
            Op::New(t) => write!(f, "new {}", t.data().to.str()),
            Op::Load(l) => write!(f, ".{}", l.name),
            Op::Store(s) => write!(f, ".{}={};", s.name, input(4)),
            Op::Cfg => unreachable!(),
        }
    }
}
