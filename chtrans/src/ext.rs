use crate::{*, slots::*, hlist::*};
use typenum::{Unsigned, consts::*};
use core::num::*;

macro_rules! primitive {
    ($($type:ty { align: $align:ty, slots: $slots:ty })*) => {$(
        unsafe impl Type for $type {
            type Repr = Self;
        }

        impl Representation for $type {
            type Slots = $slots;
            type Align = $align;
        }
    )*};
    ($($a:ty => $b:ty),* $(,)?) => {$(
        unsafe impl Type for $a {
            type Repr = $b;
        }
    )*}
}

primitive! {
    bool { align: U1, slots: HList!(Init, Markers<HList!(bool)>) }
    
    u8  { align: U1, slots: HList!(Init) }
    u16 { align: U2, slots: HList!(Init, Init) }
    u32 { align: U4, slots: HList!(Init, Init, Init, Init) }
    u64 { align: U8, slots: HList!(Init, Init, Init, Init, Init, Init, Init, Init) }
    
    NonZeroU8  { align: U1, slots: HList!(Init, Markers<HList!(NonZero<Next>)>) }
    NonZeroU16 { align: U2, slots: HList!(Init, Init, Markers<HList!(NonZero<Next2>)>) }
    NonZeroU32 { align: U4, slots: HList!(Init, Init, Init, Init, Markers<HList!(NonZero<Next4>)>) }
    NonZeroU64 { align: U8, slots: HList!(Init, Init, Init, Init, Init, Init, Init, Init, Markers<HList!(NonZero<Next8>)>) }
}

primitive! {
    i8 => u8,
    i16 => u16,
    i32 => u32,
    i64 => u64,
    f32 => u32,
    f64 => u64,
    
    NonZeroI8 => NonZeroU8,
    NonZeroI16 => NonZeroU16,
    NonZeroI32 => NonZeroU32,
    NonZeroI64 => NonZeroU64,
    
    isize => PointerRepr,
    usize => PointerRepr,
    NonZeroIsize => NZPointerRepr,
    NonZeroUsize => NZPointerRepr,
    
    Option<NonZeroI8> => u8,
    Option<NonZeroI16> => u16,
    Option<NonZeroI32> => u32,
    Option<NonZeroI64> => u64,
    Option<NonZeroIsize> => PointerRepr,
    
    Option<NonZeroU8> => u8,
    Option<NonZeroU16> => u16,
    Option<NonZeroU32> => u32,
    Option<NonZeroU64> => u64,
    Option<NonZeroUsize> => PointerRepr,
}

#[cfg(target_pointer_width = "16")]
type PointerRepr = u16;
#[cfg(target_pointer_width = "16")]
type NZPointerRepr = NonZeroU16;

#[cfg(target_pointer_width = "32")]
type PointerRepr = u32;
#[cfg(target_pointer_width = "32")]
type NZPointerRepr = NonZeroU32;

#[cfg(target_pointer_width = "64")]
type PointerRepr = u64;
#[cfg(target_pointer_width = "64")]
type NZPointerRepr = NonZeroU64;

macro_rules! type_array {
    ($($len:ty),* $(,)?) => {$(
        unsafe impl<T: Type> Type for [T; <$len as Unsigned>::USIZE]
        where
            crate::Slots<T>: Cyclic<$len>,
            crate::hlist::Cycle<crate::Slots<T>, $len>: SlotList
        {
            type Repr = [T::Repr; <$len as Unsigned>::USIZE];
        }

        impl<T: Representation> Representation for [T; <$len as Unsigned>::USIZE]
        where
            T::Slots: Cyclic<$len>,
            Cycle<T::Slots, $len>: SlotList,
        {
            type Slots = Cycle<T::Slots, $len>;
            type Align = T::Align;
        }
    )*};
}

type_array! {
    U0,  U1,  U2,  U3,  U4,  U5,  U6,  U7,
    U8,  U9,  U10, U11, U12, U13, U14, U15,
    U16, U17, U18, U19, U20, U21, U22, U23,
    U24, U25, U26, U27, U28, U29, U30, U31,
    U32,
}

unsafe impl<T> Type for *const T {
    type Repr = PointerRepr;
}

unsafe impl<T> Type for *mut T {
    type Repr = PointerRepr;
}

unsafe impl<T> Type for &T {
    type Repr = Self;
}

unsafe impl<T> Type for &mut T {
    type Repr = Self;
}

unsafe impl<T> Type for Option<&T> {
    type Repr = Self;
}

unsafe impl<T> Type for Option<&mut T> {
    type Repr = Self;
}

impl<T> Representation for &T {
    type Align = PointerAlign;
    #[allow(clippy::type_complexity)]
    type Slots = Appended<Repeated<Init, PointerSize>, HList!(Markers<HList!(NonZero<PointerNext>, Self)>)>;
}

impl<T> Representation for &mut T {
    type Align = PointerAlign;
    #[allow(clippy::type_complexity)]
    type Slots = Appended<Repeated<Init, PointerSize>, HList!(Markers<HList!(NonZero<PointerNext>, Self)>)>;
}

impl<'a, T> Representation for Option<&'a T> {
    type Align = PointerAlign;
    #[allow(clippy::type_complexity)]
    type Slots = Appended<Repeated<Init, PointerSize>, HList!(Markers<HList!(&'a T)>)>;
}

impl<'a, T> Representation for Option<&'a mut T> {
    type Align = PointerAlign;
    #[allow(clippy::type_complexity)]
    type Slots = Appended<Repeated<Init, PointerSize>, HList!(Markers<HList!(&'a mut T)>)>;
}
