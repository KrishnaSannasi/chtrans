//! The specification for the high level representation of types
//! 
//! [struct](crate::repr::struct) contains the specification for structs
//! 

use crate::hlist::*;
use crate::{Type, slots::{self, Uninit, SlotList}};
use typenum::{*, marker_traits::PowerOfTwo, operator_aliases::Mod};
use core::ops::{Rem, Sub};

/// specifies that the fields of a type must be laid out with `C`'s
/// field layout algorithm.
pub enum ReprC {}

/// specifies that the fields of a type must be laid out without any
/// padding bytes
pub enum ReprPacked {}

pub mod r#struct;

/// A helper to get the output of [`Pad`](crate::repr::Pad)
pub type Padded<Repr, T, Align> = <T as Pad<Repr, Align>>::Output;

/// Pads a type to the specified alignment using the specified representation
/// 
/// For example, with `ReprC`, `HList!(Init)` when aligned to `U4`, will produce `HList!(Init, Uninit, Uninit, Uninit)`
/// 
/// With `ReprPacked` this operation is a no-op.
/// 
/// You can use [`Padded<Self, Repr, Align>`](crate::repr::Padded) to easily get the output
pub trait Pad<Repr, Align> {
    type Output: SlotList;
}

impl<Align> Pad<ReprC, Align> for Nil {
    type Output = Nil;
}

impl<Align, P, T> Pad<ReprC, Align> for Cons<P, T>
where
    Self: SlotList + Push<Uninit, Mod<Align::Output, Align>>,
    slots::Size<Self>: Rem<Align>,
    Align: PowerOfTwo + Sub<Mod<slots::Size<Self>, Align>>,
    Align::Output: Rem<Align>,
    PushTo<Self, Uninit, Mod<Align::Output, Align>>: SlotList,
{
    type Output = PushTo<Self, Uninit, Mod<Align::Output, Align>>;
}

impl<Align, L: SlotList> Pad<ReprPacked, Align> for L {
    type Output = L;
}
