use crate::sea_of_nodes::types::Ty;

/// Represents a field in a struct. This is not a Type in the type system.
/// The pair {fieldName,type} uniquely identifies a field.
pub struct Field<'t> {
    /// Field name
    pub fname: &'t str,
    /// Type of the field
    pub ty: Ty<'t>,
    /// Unique memory alias, not sensibly part of a "type" but very convenient here.
    pub alias: usize,
    /// Field must be written to exactly once, no more, no less
    pub final_field: bool,
}
