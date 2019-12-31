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

pub type Padded<Repr, T, Align> = <T as Pad<Repr, Align>>::Output;

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
