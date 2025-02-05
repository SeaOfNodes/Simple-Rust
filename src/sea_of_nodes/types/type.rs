use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;

use crate::sea_of_nodes::types::ty::TyFloat;
use crate::sea_of_nodes::types::{Field, Ty, TyStruct};

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

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Int {
    pub min: i64,
    pub max: i64,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Float {
    sz: i8,
    /// f64 bits as integer for easier eq/hashing
    con: u64,
}

impl Float {
    pub fn new(sz: i8, con: f64) -> Self {
        Self {
            sz,
            con: con.to_bits(),
        }
    }

    pub fn con(&self) -> f64 {
        f64::from_bits(self.con)
    }
}

impl<'t> TyFloat<'t> {
    pub fn is_f32(&self) -> bool {
        let v = self.data().con();
        v as f32 as f64 == v
    }
}

pub type Tuple<'t> = &'t [Ty<'t>];

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Struct<'t> {
    pub name: &'t str,
    pub fields: &'t [Field<'t>],
}

impl<'t> TyStruct<'t> {
    pub fn name(self) -> &'t str {
        self.data().name
    }

    pub fn fields(self) -> &'t [Field<'t>] {
        self.data().fields
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct MemPtr<'t> {
    pub to: TyStruct<'t>,
    pub nil: bool,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Mem<'t> {
    pub alias: u32,
    /// Memory contents, some scalar type
    pub t: Ty<'t>,
}

impl<'t> Type<'t> {
    /// Is high or on the lattice centerline.
    pub fn is_high_or_constant(&'t self) -> bool {
        match self {
            Type::Bot => false,
            Type::Top => true,
            Type::Ctrl => false,
            Type::XCtrl => true,
            Type::Int(i) => !matches!(i, Int::Bot),
            Type::Tuple { .. } => false,
            Type::Struct(_) => false,
            Type::MemPtr(_) => false,
            Type::Mem(_) => false,
        }
    }

    pub fn is_constant(&'t self) -> bool {
        matches!(self, Type::Int(Int::Constant(_)))
    }

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
