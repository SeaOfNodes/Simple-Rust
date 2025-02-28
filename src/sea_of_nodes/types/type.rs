use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;

use crate::sea_of_nodes::types::{Field, Float, Int, Mem, MemPtr, Struct, Tuple, Ty, Types};

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
            Type::Int(i) if i.min == i.max => i.min,
            _ => unreachable!(),
        }
    }

    /// This is used by error messages, and is a shorted print.
    pub fn str(&self) -> Cow<'t, str> {
        use Cow::*;
        Borrowed(match self {
            Type::Bot => "Bot",
            Type::Top => "Top",
            Type::Ctrl => "Ctrl",
            Type::XCtrl => "~Ctrl",
            Type::Float(f) => match (f.sz, f.con()) {
                (-64, 0.0) => "~flt",
                (-32, 0.0) => "~f32",
                (32, 0.0) => "f32",
                (64, 0.0) => "flt",
                _ => return Owned(format!("{}{}", f.con(), if f.is_f32() { "f" } else { "" })),
            },
            Type::Int(i) => match (i.min, i.max) {
                (i64::MAX, i64::MIN) => "~int",
                (i64::MIN, i64::MAX) => "int",
                (0, 1) => "bool",
                (-128, 127) => "i8",
                (-32768, 32767) => "i16",
                (-2147483648, 2147483647) => "i32",
                (0, 255) => "u8",
                (0, 65535) => "u16",
                (0, 4294967295) => "u32",
                (min, max) => {
                    return Owned(if min == max {
                        format!("{}", min)
                    } else {
                        format!("[{}-{}]", min, max)
                    })
                }
            },
            Type::Tuple(ts) => {
                let mut sb = "[  ".to_string();
                for t in *ts {
                    sb.push_str(t.str().as_ref());
                    sb.push_str(", ");
                }
                sb.pop();
                sb.pop();
                sb.push(']');
                return Owned(sb);
            }
            Type::Struct(s) => s.name,
            Type::MemPtr(p) => {
                if p.nil && p.to.name() == "$TOP" && p.to.data().fields == Some(&[]) {
                    "null"
                } else if !p.nil && p.to.name() == "$BOT" && p.to.data().fields == Some(&[]) {
                    "*void"
                } else {
                    return Owned(format!("*{}{}", p.to.str(), if p.nil { "?" } else { "" }));
                }
            }
            Type::Mem(_) => return Owned(self.to_string()),
        })
    }
}

impl<'t> Display for Type<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Type::Bot => "Bot",
            Type::Top => "Top",
            Type::Ctrl => "Ctrl",
            Type::XCtrl => "~Ctrl",
            Type::Float(float) => match (float.sz, float.con()) {
                (-64, 0.0) => "FltTop",
                (-32, 0.0) => "F32Top",
                (32, 0.0) => "F32Bot",
                (64, 0.0) => "FltBot",
                (_, c) => return write!(f, "{:?}", c),
            },
            Type::Int(_) => return f.write_str(self.str().as_ref()),
            Type::Tuple(types) => {
                write!(f, "[")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", {ty}")?;
                    } else {
                        write!(f, "  {ty}")?;
                    }
                }
                return write!(f, "]");
            }
            Type::Struct(s) => {
                write!(f, "{}", s.name)?;
                // Forward reference struct, just print the name
                if let Some(fs) = s.fields {
                    write!(f, " {{")?;
                    for field in fs {
                        write!(f, "{} ", field.ty)?;
                        if !field.final_field {
                            write!(f, "!")?;
                        }
                        write!(f, "{}; ", field.fname)?;
                    }
                    write!(f, "}}")?;
                }
                return Ok(());
            }
            Type::MemPtr(p) => {
                if p.nil && p.to.name() == "$TOP" && p.to.data().fields == Some(&[]) {
                    "null"
                } else if !p.nil && p.to.name() == "$BOT" && p.to.data().fields == Some(&[]) {
                    "*void"
                } else {
                    return write!(f, "*{}{}", p.to, if p.nil { "?" } else { "" });
                }
            }
            Type::Mem(m) => match m.alias {
                0 => "MEM#TOP",
                u32::MAX => "MEM#BOT",
                _ => return write!(f, "MEM#{}:{}", m.alias, m.t),
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
                    *tys.get_tuple_from_slice(
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
                *tys.get_tuple_from_slice(&types.iter().map(|t| t.dual(tys)).collect::<Vec<_>>())
            }
            Type::Struct(_) => *self.as_struct().dual(tys),
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
                *tys.get_tuple_from_slice(&types)
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
            Type::Tuple(_) => tys.top, // why?
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

    /// All reachable struct Fields are final
    pub fn is_final(self) -> bool {
        match self.data() {
            Type::MemPtr(m) => m.to.is_final(),
            Type::Struct(s) => match s.fields {
                None => true,
                Some(fields) => fields.iter().all(|f| f.final_field && f.ty.is_final()),
            },
            _ => true,
        }
    }

    /// Make all reachable struct Fields final
    pub fn make_ro(self, tys: &Types<'t>) -> Ty<'t> {
        match self.data() {
            Type::MemPtr(m) => {
                let to = m.to.make_ro(tys);
                if to == *m.to {
                    self
                } else {
                    *tys.get_mem_ptr(to.as_struct(), m.nil)
                }
            }
            Type::Struct(s) => match s.fields {
                _ if self.is_final() => self,
                None => unreachable!("is_final"),
                Some(fields) => {
                    let new_fields = fields
                        .iter()
                        .map(|f| Field {
                            fname: f.fname,
                            ty: if f.final_field {
                                f.ty
                            } else {
                                f.ty.make_ro(tys)
                            },
                            alias: f.alias,
                            final_field: true,
                        })
                        .collect::<Vec<_>>();
                    *tys.get_struct(s.name, &new_fields)
                }
            },
            _ => self,
        }
    }

    // Size in bits to hold an instance of this type.
    // Sizes are expected to be between 1 and 64 bits.
    // Size 0 means this either takes no space (such as a known-zero field)
    // or isn't a scalar to be stored in memory.
    pub fn log_size(self, tys: &Types<'t>) -> usize {
        match self.data() {
            Type::Int(_) => {
                if [*tys.int_i8, *tys.int_u8, *tys.int_bool].contains(&self) {
                    0 // 1<<0 == 1 bytes
                } else if [*tys.int_i16, *tys.int_u16].contains(&self) {
                    1 // 1<<1 == 2 bytes
                } else if [*tys.int_i32, *tys.int_u32].contains(&self) {
                    2 // 1<<2 == 4 bytes
                } else if self == *tys.int_bot {
                    3 // 1<<3 == 8 bytes
                } else if self.is_high_or_constant(tys) {
                    0
                } else {
                    todo!()
                }
            }
            Type::Float(f) => {
                let sz = if f.sz == 0 {
                    if self.as_float().is_f32() {
                        32
                    } else {
                        64
                    }
                } else {
                    f.sz.abs()
                };
                if sz == 32 {
                    2
                } else {
                    3
                }
            }
            Type::MemPtr(_) => 2, // (1<<2)==4-byte pointers
            _ => todo!(),
        }
    }
}
