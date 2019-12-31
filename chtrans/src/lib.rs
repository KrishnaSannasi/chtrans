#![no_std]

pub mod hlist;
pub mod slots;
pub mod repr;
pub mod compat;
mod ext;

pub use chtrans_derive::repr;

pub struct Next<T = End>(T);
pub enum End {}

type Next2<N = End> = Next<Next<N>>;
type Next4<N = End> = Next2<Next2<N>>;
type Next8<N = End> = Next4<Next4<N>>;

#[cfg(target_pointer_width = "16")]
type PointerNext<N = End> = Next2<N>;

#[cfg(target_pointer_width = "32")]
type PointerNext<N = End> = Next4<N>;

#[cfg(target_pointer_width = "64")]
type PointerNext<N = End> = Next8<N>;

#[cfg(target_pointer_width = "16")]
pub type PointerSize = typenum::U2;
#[cfg(target_pointer_width = "16")]
pub type PointerAlign = typenum::U2;

#[cfg(target_pointer_width = "32")]
pub type PointerSize = typenum::U4;
#[cfg(target_pointer_width = "32")]
pub type PointerAlign = typenum::U4;

#[cfg(target_pointer_width = "64")]
pub type PointerSize = typenum::U8;
#[cfg(target_pointer_width = "64")]
pub type PointerAlign = typenum::U8;

pub type Repr<T> = <T as Type>::Repr;
pub type Slots<T> = <Repr<T> as Representation>::Slots;
pub type Align<T> = <Repr<T> as Representation>::Align;
pub type Size<T> = slots::Size<Slots<T>>;

pub unsafe trait Type {
    type Repr: Representation;
}

pub trait Representation {
    type Slots: slots::SlotList;
    type Align: typenum::marker_traits::PowerOfTwo;
}

impl<T: Type> Reinterpret for T {}
pub trait Reinterpret: Sized + Type {
    #[inline(always)]
    fn reinterp_into<T>(self) -> T
    where
        T: Type,
        Slots<T>: self::compat::CanConvertFrom<Slots<Self>>
    {
        let value = core::mem::ManuallyDrop::new(self);

        unsafe { core::mem::transmute_copy(&value) }
    }

    #[inline(always)]
    fn reinterp_as<T>(&self) -> &T
    where
        T: Type,
        Slots<T>: self::compat::CanConvertFrom<Slots<Self>>
    {
        #[allow(clippy::transmute_ptr_to_ptr)]
        unsafe { core::mem::transmute(self) }
    }

    #[inline(always)]
    fn reinterp_as_mut<T>(&mut self) -> &mut T
    where
        T: Type,
        Slots<T>: self::compat::CanConvertFrom<Slots<Self>>
    {
        #[allow(clippy::transmute_ptr_to_ptr)]
        unsafe { core::mem::transmute(self) }
    }
}
