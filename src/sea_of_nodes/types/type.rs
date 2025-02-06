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
            (Type::Float(_), Type::Float(_)) => *self.as_float().meet(that.as_float(), tys),

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
            (Type::Struct(_), Type::Struct(_)) => *self.as_struct().meet(that.as_struct(), tys),

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

    pub fn dual(self, tys: &Types<'t>) -> Ty<'t> {
        match self.data() {
            Type::Bot => tys.top,
            Type::Top => tys.bot,
            Type::Ctrl => tys.xctrl,
            Type::XCtrl => tys.ctrl,
            Type::Float(f) => {
                if f.sz == 0 {
                    self // Constants are a self-dual
                } else {
                    *tys.make_float(-f.sz, 0.0)
                }
            }
            Type::Int(i) => *tys.make_int(i.max, i.min),
            Type::Tuple(types) => {
                tys.get_tuple_from_slice(&types.iter().map(|t| t.dual(tys)).collect::<Vec<_>>())
            }
            Type::Struct(s) => *self.as_struct().dual(tys),
            Type::MemPtr(p) => *tys.get_mem_ptr(p.to.dual(tys), !p.nil),
            Type::Mem(m) => *tys.get_mem(m.alias, m.t.dual(tys)),
        }
    }

    /// Our lattice is defined with a MEET and a DUAL.
    /// JOIN is dual of meet of both duals.
    pub fn join(self, that: Ty<'t>, tys: &Types<'t>) -> Ty<'t> {
        if self == that {
            self
        } else {
            self.dual(tys).meet(that.dual(tys), tys).dual(tys)
        }
    }

    /// True if this "isa" t; e.g. 17 isa TypeInteger.BOT
    pub fn isa(self, t: Ty<'t>, tys: &Types<'t>) -> bool {
        self.meet(t, tys) == t
    }

    /// Compute greatest lower bound in the lattice
    pub fn glb(self, tys: &Types<'t>) -> Ty<'t> {
        match self.data() {
            Type::Bot | Type::Top => tys.bot,
            Type::Ctrl => tys.ctrl,
            Type::XCtrl => tys.bot, // why?
            Type::Int(_) => *tys.int_bot,
            Type::Float(_) => *tys.float_bot,
            Type::Tuple(types) => {
                let types = types.iter().map(|ty| ty.glb(tys)).collect::<Vec<_>>();
                tys.get_tuple_from_slice(&types)
            }
            Type::Struct(_) => *self.as_struct().glb(tys),
            Type::MemPtr(MemPtr { to, .. }) => *tys.get_mem_ptr(to.glb(tys), true),
            Type::Mem(m) => *tys.get_mem(m.alias, m.t.glb(tys)),
        }
    }

    /// Compute least upper bound in the lattice
    pub fn lub(self, tys: &Types<'t>) -> Ty<'t> {
        match self.data() {
            Type::Bot | Type::Top => tys.top,
            Type::Ctrl => tys.xctrl,
            Type::XCtrl => tys.top, // why?
            Type::Int(_) => *tys.int_top,
            Type::Float(_) => *tys.float_top,
            Type::Tuple(types) => tys.top, // why?
            Type::Struct(_) => *self.as_struct().lub(tys),
            Type::MemPtr(MemPtr { to, .. }) => *tys.get_mem_ptr(to.lub(tys), false),
            Type::Mem(m) => *tys.get_mem(m.alias, m.t.lub(tys)),
        }
    }

    /// Make an initial/default version of this type.  Typically, 0 for integers
    /// and null for nullable pointers.
    pub fn make_init(self, tys: &Types<'t>) -> Ty<'t> {
        match self.data() {
            Type::Int(_) => *tys.int_zero,
            Type::Float(_) => *tys.float_zero,
            Type::MemPtr(m) => {
                if m.nil {
                    *tys.ptr_null
                } else {
                    tys.top
                }
            }
            _ => self,
        }
    }
    /// Make a zero version of this type, 0 for integers and null for pointers.
    pub fn make_zero(self, tys: &Types<'t>) -> Ty<'t> {
        match self.data() {
            Type::Int(_) => *tys.int_zero,
            Type::Float(_) => *tys.float_zero,
            Type::MemPtr(_) => *tys.ptr_null,
            _ => unreachable!(),
        }
    }

    /// Make a non-zero version of this type, if possible.  Integers attempt to
    /// exclude zero from their range and pointers become not-null.
    pub fn non_zero(self, tys: &Types<'t>) -> Ty<'t> {
        match self.data() {
            Type::Int(i) => {
                if i.min > i.max {
                    self
                } else if i.min == 0 {
                    *tys.make_int(1, i.max.max(1)) // specifically good on BOOL
                } else if i.max == 0 {
                    *tys.make_int(i.min, -1)
                } else {
                    self
                }
            }
            Type::MemPtr(_) => *tys.ptr_void,
            _ => self.glb(tys),
        }
    }

    /// Is forward-reference
    pub fn is_fref(self) -> bool {
        match self.data() {
            Type::Struct(s) => s.fields.is_none(),
            Type::MemPtr(m) => m.to.data().fields.is_none(),
            _ => false,
        }
    }
}
