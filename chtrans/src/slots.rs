use crate::hlist::*;
use typenum::{*, operator_aliases::Sum};
use core::ops::Add;

pub type Size<L> = <L as SlotList>::Size;

pub trait Slot {
    type Size: Unsigned;
}

pub trait SlotList {
    type Size: Unsigned;
}

pub trait Marker {}

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

pub struct Init;
pub struct Uninit;
pub struct NonZero<N>(N);

impl Slot for Init {
    type Size = U1;
}

impl Slot for Uninit {
    type Size = U1;
}

pub struct Markers<T>(T);

impl<P: MarkerList, T: Marker> Slot for Markers<Cons<P, T>> {
    type Size = U0;
}

impl MarkerList for Nil {}
impl<P: MarkerList, T: Marker> MarkerList for Cons<P, T> {}

impl<N> Marker for NonZero<N> {}
impl Marker for bool {}
impl<T> Marker for &T {}
impl<T> Marker for &mut T {}