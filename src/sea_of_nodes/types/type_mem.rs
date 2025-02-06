use crate::sea_of_nodes::types::Ty;

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Mem<'t> {
    pub alias: u32,
    /// Memory contents, some scalar type
    pub t: Ty<'t>,
}
