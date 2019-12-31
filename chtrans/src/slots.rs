
//! The specification of the memory layout
//! 
//! Memory is a linked list of bytes or markers, bytes take up a single byte of
//! space, and markers take up no space at all. Memory bytes do not have a notion
//! of alignment. The are a simple list of bytes. Bytes and markers collectively
//! are called slots. Furthermore, bytes can either be initialized or uninitialized.
//! 
//! Makers are stored in the [`Markers`](crate::slots::Marker) type, which is itself a slot.
//! The types [`Init`](crate::slots::Init) and [`Uninit`](crate::slots::Uninit) represent
//! initialized an uninitialized bytes of memory. Each is a single byte wide.
//! 
//! A [`SlotList`](crate::slots::SlotList) is a linked list where every element is a [`Slot`](crate::slots::Slot).

use crate::hlist::*;
use typenum::{*, operator_aliases::Sum};
use core::ops::Add;

/// The size of a [`SlotList`](crate::slots::SlotList)
pub type Size<L> = <L as SlotList>::Size;

/// Either an [`Init`](crate::slots::Init), [`Uninit`](crate::slots::Uninit), [`Marker`](crate::slots::Marker)
/// 
/// see [module](crate::slots) docs for details
pub trait Slot {
    /// The size of a slot, must be 0 or 1
    type Size: Bit;
}

/// A linked list of [`Slot`](crate::slots::Slot)s
pub trait SlotList {
    /// The total size of all slots in the list
    type Size: Unsigned;
}

/// A property of the directly bytes that follow it
/// 
/// `bool` means the next byte is either a 0 or a 1
/// 
/// `&'a T` means that the next pointer width number of bytes
/// represents a valid shared reference to `T` with lifetime `'a`
/// 
/// `&'a mut T` means that the next pointer width number of bytes
/// represents a valid unique reference to `T` with lifetime `'a`
/// 
/// [`NonZero<N>`](crate::slots::NonZero) see type docs
pub trait Marker {}

/// A linked list of [`Marker`](crate::slots::Marker)s
pub trait MarkerList {}

impl SlotList for Nil {
    type Size = U0;
}

impl<P: SlotList, T: Slot> SlotList for Cons<P, T>
where
    P::Size: Add<T::Size>,
    Sum<P::Size, T::Size>: Unsigned,
{
    type Size = Sum<P::Size, T::Size>;
}

/// Initialized memory
pub struct Init;

/// Uninitialized memory
pub struct Uninit;

/// Stores a linked list of [`Marker`](crate::slots::Marker)s
pub struct Markers<T>(T);

/// A marker that states that the next `N` bytes are guaranteed to contain at
/// least 1 bit that is set. (i.e. if the next `N` bytes are interpreted as
/// an integer, that integer would be non-zero)
pub struct NonZero<N>(N);

impl Slot for Init {
    type Size = B1;
}

impl Slot for Uninit {
    type Size = B1;
}

impl<P: MarkerList, T: Marker> Slot for Markers<Cons<P, T>> {
    type Size = B0;
}

impl MarkerList for Nil {}
impl<P: MarkerList, T: Marker> MarkerList for Cons<P, T> {}

impl<N> Marker for NonZero<N> {}
impl Marker for bool {}
impl<T> Marker for &T {}
impl<T> Marker for &mut T {}