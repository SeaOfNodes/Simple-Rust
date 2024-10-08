use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;

use crate::sea_of_nodes::types::Ty;

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub enum Type<'a> {
    Bot,   // all
    Top,   // any
    Ctrl,  // control flow bottom
    XCtrl, //  ctrl flow top (mini-lattice: any-xctrl-ctrl-all)
    Int(Int),
    Tuple { types: &'a [Ty<'a>] },
    Struct(Struct<'a>),
    Pointer(MemPtr<'a>),
    Memory(Mem),
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub enum Int {
    Bot,
    Top,
    Constant(i64),
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub enum Struct<'t> {
    Bot,
    Top,
    Struct {
        name: &'t str,
        fields: &'t [(&'t str, Ty<'t>)],
    },
}

impl<'t> Struct<'t> {
    pub fn name(&self) -> &'t str {
        match self {
            Struct::Bot => "$BOT",
            Struct::Top => "$TOP",
            Struct::Struct { name, .. } => name,
        }
    }

    pub fn fields(&self) -> &[(&'t str, Ty<'t>)] {
        match self {
            Struct::Bot | Struct::Top => &[],
            Struct::Struct { fields, .. } => fields,
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct MemPtr<'t> {
    /// points to Type::Struct
    pub to: Ty<'t>,
    pub nil: bool,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub enum Mem {
    Bot,
    Top,
    Alias(u32),
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
            Type::Pointer(_) => false,
            Type::Memory(_) => false,
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
            Type::Pointer(p) => {
                if *p.to == Type::Struct(Struct::Top) && p.nil {
                    Borrowed("null")
                } else if *p.to == Type::Struct(Struct::Bot) && !p.nil {
                    Borrowed("*void")
                } else {
                    Owned(format!("*{}{}", p.to.str(), if p.nil { "?" } else { "" }))
                }
            }
            Type::Memory(_) => Owned(self.to_string()),
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
            Type::Tuple { types } => {
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
            Type::Pointer(p) => match *p {
                MemPtr { to, nil: true } if *to == Type::Struct(Struct::Top) => "null",
                MemPtr { to, nil: false } if *to == Type::Struct(Struct::Bot) => "*void",
                MemPtr { to, nil } => {
                    return write!(f, "*{to}{}", if nil { "?" } else { "" });
                }
            },
            Type::Memory(m) => match m {
                Mem::Bot => "MEM#BOT",
                Mem::Top => "MEMTOP",
                Mem::Alias(a) => return write!(f, "MEM#{a}"),
            },
        })
    }
}
