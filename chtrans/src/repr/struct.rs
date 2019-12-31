use super::*;
use crate::hlist::{Append, Appended};

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

pub type Padded<Repr, T, Align> = <T as Pad<Repr, Align>>::Output;

pub trait Pad<Repr, Align> {
    type Output: SlotList;
}

impl<Repr, Align> Pad<Repr, Align> for Nil {
    type Output = Nil;
}

impl<Align, P, T> Pad<ReprC, Align> for Cons<P, T>
where
    Self: SlotList + Append<Repeated<Uninit, Mod<Align::Output, Align>>>,
    Align: PowerOfTwo + Sub<Mod<slots::Size<Self>, Align>>,
    Align::Output: Rem<Align>,
    slots::Size<Self>: Rem<Align>,
    Uninit: Repeat<Mod<Align::Output, Align>>,
    Appended<Self, Repeated<Uninit, Mod<Align::Output, Align>>>: SlotList,
{
    type Output = Appended<Self, Repeated<Uninit, Mod<Align::Output, Align>>>;
}
