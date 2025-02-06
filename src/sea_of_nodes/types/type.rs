use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;

use crate::sea_of_nodes::types::{Float, Int, Mem, MemPtr, Struct, Tuple, Ty, Types};

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub enum Type<'a> {
    Bot,   // all
    Top,   // any
    Ctrl,  // control flow bottom
    XCtrl, //  ctrl flow top (mini-lattice: any-xctrl-ctrl-all)
    Int(Int),
    Float(Float),
    Tuple(Tuple<'a>),
    Struct(Struct<'a>),
    MemPtr(MemPtr<'a>),
    Mem(Mem<'a>),
}

impl<'t> Type<'t> {
    pub fn unwrap_int(&'t self) -> i64 {
        match self {
            Type::Int(Int::Constant(value)) => *value,
            _ => unreachable!(),
        }
    }

    // This is used by error messages, and is a shorted print.
    pub fn str(&self) -> Cow<str> {
        use Cow::*;
        match self {
            Type::Bot => Borrowed("Bot"),
            Type::Top => Borrowed("Top"),
            Type::Ctrl => Borrowed("Ctrl"),
            Type::XCtrl => Borrowed("~Ctrl"),
            Type::Int(i) => match i {
                Int::Bot => Borrowed("int"),
                Int::Top => Borrowed("~int"),
                Int::Constant(c) => Owned(c.to_string()),
            },
            Type::Tuple { .. } => Owned(self.to_string()),
            Type::Struct(s) => Borrowed(s.name()),
            Type::MemPtr(p) => {
                if *p.to.data() == Struct::Top && p.nil {
                    Borrowed("null")
                } else if *p.to.data() == Struct::Bot && !p.nil {
                    Borrowed("*void")
                } else {
                    Owned(format!("*{}{}", p.to.str(), if p.nil { "?" } else { "" }))
                }
            }
            Type::Mem(_) => Owned(self.to_string()),
        }
    }
}

impl<'t> Display for Type<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Type::Bot => "Bot",
            Type::Top => "Top",
            Type::Ctrl => "Ctrl",
            Type::XCtrl => "~Ctrl",
            Type::Int(Int::Bot) => "IntBot",
            Type::Int(Int::Top) => "IntTop",
            Type::Int(Int::Constant(c)) => return write!(f, "{c}"),
            Type::Tuple(types) => {
                write!(f, "[")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{ty}")?;
                }
                return write!(f, "]");
            }
            Type::Struct(s) => match s {
                Struct::Bot => "$BOT",
                Struct::Top => "$TOP",
                Struct::Struct { name, fields } => {
                    writeln!(f, "{name} {{")?;
                    for (name, ty) in fields.iter() {
                        writeln!(f, "  {name}:{ty};")?;
                    }
                    return write!(f, "}}");
                }
            },
            Type::MemPtr(p) => match *p {
                MemPtr { to, nil: true } if *to.data() == Struct::Top => "null",
                MemPtr { to, nil: false } if *to.data() == Struct::Bot => "*void",
                MemPtr { to, nil } => {
                    return write!(f, "*{to}{}", if nil { "?" } else { "" });
                }
            },
            Type::Mem(m) => match m {
                Mem::Bot => "MEM#BOT",
                Mem::Top => "MEMTOP",
                Mem::Alias(a) => return write!(f, "MEM#{a}"),
            },
        })
    }
}

impl<'t> Type<'t> {
    pub fn is_fref(&self) -> bool {
        match self {
            Type::Bot => false,
            Type::Top => false,
            Type::Ctrl => false,
            Type::XCtrl => false,
            Type::Int(_) => false,
            Type::Tuple(_) => false,
            Type::Struct(s) => todo!("is struct {s:?} an fref?"),
            Type::MemPtr(m) => m.to.is_fref(),
            Type::Mem(_) => false,
        }
    }
}

impl<'t> Ty<'t> {
    /// Is high or on the lattice centerline.
    pub fn is_high(self, tys: &Types<'t>) -> bool {
        match self.data() {
            Type::Top | Type::XCtrl => true,
            Type::Int(i) => i.min > i.max,
            Type::Float(f) => f.sz < 0,
            Type::MemPtr(_) => self == *tys.ptr_top,
            Type::Bot | Type::Ctrl | Type::Tuple(_) | Type::Struct(_) | Type::Mem(_) => false,
        }
    }

    /// Is high or on the lattice centerline.
    pub fn is_high_or_constant(self, tys: &Types<'t>) -> bool {
        match self.data() {
            Type::Top | Type::XCtrl => true,
            Type::Int(i) => i.min >= i.max,
            Type::Float(f) => f.sz <= 0,
            Type::MemPtr(_) => self == *tys.ptr_top || self == *tys.ptr_null,
            Type::Bot | Type::Ctrl | Type::Tuple(_) | Type::Struct(_) | Type::Mem(_) => false,
        }
    }

    /// Strict constant values, things on the lattice centerline.
    /// Excludes both high and low values
    pub fn is_constant(self, tys: &Types<'t>) -> bool {
        match self.data() {
            Type::Int(i) => i.min == i.max,
            Type::Float(f) => f.sz == 0,
            Type::MemPtr(_) => self == *tys.ptr_null,
            Type::Top | Type::Bot => false,
            Type::XCtrl | Type::Ctrl => false,
            Type::Tuple(_) | Type::Struct(_) | Type::Mem(_) => false,
        }
    }

    pub fn meet(self, that: Ty<'t>, tys: &Types<'t>) -> Ty<'t> {
        match (*self, *that) {
            (_, _) if self == that => self, // Shortcut for the self case

            // Bot wins, Top looses
            (Type::Bot, _) | (_, Type::Top) => self,
            (Type::Top, _) | (_, Type::Bot) => that,

            // Ctrl sub-lattice: Ctrl meets ~Ctrl is Ctrl
            (Type::Ctrl, Type::XCtrl) => self,
            (Type::XCtrl, Type::Ctrl) => that,

            // Float sub-lattice
            (Type::Float(_), Type::Float(_)) => {
                *self.to_float().unwrap().meet(that.to_float().unwrap(), tys)
            }

            // Int sub-lattice
            (Type::Int(a), Type::Int(b)) => *tys.make_int(a.min.min(b.min), a.max.max(b.max)),

            // Tuple sub-lattice
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                if t1.len() != t2.len() {
                    tys.bot
                } else {
                    tys.get_tuple_from_slice(
                        &t1.iter()
                            .zip(t2.iter())
                            .map(|(x, y)| x.meet(*y, tys))
                            .collect::<Vec<_>>(),
                    )
                }
            }

            // Struct sub-lattice
            (Type::Struct(_), Type::Struct(_)) => *self
                .to_struct()
                .unwrap()
                .meet(that.to_struct().unwrap(), tys),

            // Pointer sub-lattice
            (Type::MemPtr(s), Type::MemPtr(t)) => {
                *tys.get_mem_ptr(s.to.meet(t.to, tys), s.nil | t.nil)
            }

            // Memory sub-lattice
            (Type::Mem(s), Type::Mem(t)) => {
                if self == *tys.mem_bot || that == *tys.mem_top {
                    self
                } else if that == *tys.mem_bot || self == *tys.mem_top {
                    that
                } else {
                    let alias = if s.alias == t.alias {
                        s.alias
                    } else {
                        u32::MAX
                    };
                    let mt = s.t.meet(t.t, tys);
                    *tys.get_mem(alias, mt)
                }
            }

            // different sub-lattices meet at bottom
            _ => tys.bot,
        }
    }
}
