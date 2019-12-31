
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
//!     let padding = repeat(Slot::Uninit).take(padding % field.align);
//!     memory.extend(padding);
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
pub struct Struct<Repr, MinAlign, F> {
    _repr: Repr,
    _fields: F,
    _min_align: MinAlign,
}

impl<Repr, MinAlign, F> crate::Representation for Struct<Repr, MinAlign, F>
where
    MinAlign: PowerOfTwo,
    F: StructFieldList<Repr>,
    F::Slots: Pad<Repr, F::Align>,
    F::Align: Max<MinAlign>,
    Maximum<F::Align, MinAlign>: PowerOfTwo
{
    type Align = Maximum<F::Align, MinAlign>;
    type Slots = Padded<Repr, F::Slots, F::Align>;
}

/// A list of fields in a trait. All fields must be `Type`s and will be
/// properly aligned with respect to `Repr`
/// 
/// to see the algorithm, see module docs
pub trait StructFieldList<Repr> {
    /// The minimum alignment of this type
    type Align: PowerOfTwo;

    /// The in memory representation of the type
    type Slots: SlotList;
}

impl<Repr> StructFieldList<Repr> for Nil {
    type Align = U1;
    type Slots = Self;
}

impl<P: StructFieldList<ReprC>, T: Type> StructFieldList<ReprC> for Cons<P, T>
where
    P::Slots: Pad<ReprC, crate::Align<T>>,
    Padded<ReprC, P::Slots, crate::Align<T>>: Append<crate::Slots<T>>,
    P::Align: Max<crate::Align<T>>,
    Maximum<P::Align, crate::Align<T>>: PowerOfTwo,
    Appended<Padded<ReprC, P::Slots, crate::Align<T>>, crate::Slots<T>>: SlotList,
{
    type Align = Maximum<P::Align, crate::Align<T>>;
    type Slots = Appended<Padded<ReprC, P::Slots, crate::Align<T>>, crate::Slots<T>>;
}

impl<P: StructFieldList<ReprPacked>, T: Type> StructFieldList<ReprPacked> for Cons<P, T>
where
    P::Slots: Append<crate::Slots<T>>,
    Appended<P::Slots, crate::Slots<T>>: SlotList,
{
    type Align = U1;
    type Slots = Appended<P::Slots, crate::Slots<T>>;
}
