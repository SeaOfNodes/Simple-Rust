use crate::sea_of_nodes::types::TyStruct;

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct MemPtr<'t> {
    pub to: TyStruct<'t>,
    pub nil: bool,
}
