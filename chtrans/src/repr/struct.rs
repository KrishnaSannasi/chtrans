
//! The specification for struct memory layout
//! 
//! A representation can be chosen, one of [`ReprC`](crate::repr::ReprC) or [`ReprPacked`](crate::repr::ReprPacked).
//! 
//! If `ReprC` is picked, then fields are laid out in order, with the following algorithm:
//! 
//! ```rust
//! # use std::iter::repeat;
//! # struct Field { align: usize }
//! # #[derive(Clone, Copy)] enum Slot { Uninit }
//! # impl Field { fn slots(&self) -> impl Iterator<Item = Slot> { std::iter::empty() } }
//! # fn c_layout(fields: &[Field]) -> Vec<Slot> {
//! let mut memory = Vec::<Slot>::new();
//! for field in fields {
//!     let offset = memory.len();
//!     let padding = field.align - offset % field.align;
//!     memory.extend(repeat(Slot::Uninit, padding));
//!     memory.extend(field.slots());
//! }
//! return memory;
//! # }
//! ```
//! 
//! If `ReprPacked` is picked, then fields are laid out in order, with the following algorithm:
//! 
//! ```rust
//! # struct Field { align: usize }
//! # #[derive(Clone, Copy)] enum Slot { Uninit }
//! # impl Field { fn slots(&self) -> impl Iterator<Item = Slot> { std::iter::empty() } }
//! # fn c_layout(fields: &[Field]) -> Vec<Slot> {
//! let mut memory = Vec::<Slot>::new();
//! for field in fields {
//!     memory.extend(field.slots());
//! }
//! return memory;
//! # }
//! ```

use super::*;

/// The representation of structs
/// 
/// see module docs for details
pub struct Struct<Repr, F> {
    _repr: Repr,
    _fields: F
}

impl<Repr, F> crate::Representation for Struct<Repr, F>
where
    F: StructFieldList<Repr>,
    F::Slots: Pad<Repr, F::Align>,
{
    type Align = F::Align;
    type Slots = Padded<Repr, F::Slots, F::Align>;
}

pub trait StructFieldList<Repr> {
    type Align: PowerOfTwo;
    type Slots: SlotList;
}

impl<Repr> StructFieldList<Repr> for Nil {
    type Align = U1;
    type Slots = Self;
}

impl<Repr, P: StructFieldList<Repr>, T: Type> StructFieldList<Repr> for Cons<P, T>
where
    P::Slots: Pad<Repr, crate::Align<T>>,
    Padded<Repr, P::Slots, crate::Align<T>>: Append<crate::Slots<T>>,
    P::Align: Max<crate::Align<T>>,
    Maximum<P::Align, crate::Align<T>>: PowerOfTwo,
    Appended<Padded<Repr, P::Slots, crate::Align<T>>, crate::Slots<T>>: SlotList,
{
    type Align = Maximum<P::Align, crate::Align<T>>;
    type Slots = Appended<Padded<Repr, P::Slots, crate::Align<T>>, crate::Slots<T>>;
}
