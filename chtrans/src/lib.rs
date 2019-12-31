#![no_std]

//! This crate is heavily inspired by [typic](https://github.com/jswrenn/typic)
//! 
//! An alternative to typic for compile time checked transmute via encoding the
//! memory representation of types in the type system. (AKA an super complex encoding of
//! a linked list!, or how bad could it get, linked list edition (now staring types)!)
//! 
//! The crate relies on `typenum` for type-level numerals, but can be shifted to
//! const-generics once they are sufficiently expressive. It also uses a bit of 
//! peano numerals as seen in `Next*`. The reason for this is clarity. It is
//! easy to encode a strictly decreasing sequence with peano numerals as opposed 
//! to binary numerals. `typenum` is used when things need to change by more than
//! an increment.
//! 
//! This crate level doc will provide a high level overview of the `chtrans` (checked
//! transmute) and how it's encoding works. Each module will be documented with more 
//! details about how it fits in the machine.
//! 
//! # Encoding
//! 
//! Types are encoded in a few different ways in this crate, at the hightest level
//! we have `repr::r#struct::Struct` which takes a representation (`repr(C)` or
//! `repr(packed(N))`), and a field list to encode at a high level what a type's
//! shape is. This can then be lowered into it's in memory representation.
//! 
//! ## `Reinterpret`
//! 
//! This trait provides the checked transmute functions. They are guarded by the `Type`
//! trait, which specifies the layout of these types.
//! 
//! ## `Type` and `Representation`
//! 
//! `Type` represents the type which will be reinterpreted into another type. For
//! example, `#[chtrans::repr(C)] struct Foo { bar: i8, quax: u16 }` could implement 
//! `Type`. (see trait docs for more details)
//! 
//! `Representation` represents a high level representation of a type. For example,
//! `Foo` above would have a `Representation` of `Struct<ReprC, Cons<Cons<Nil, i8>, u16>>`
//!  because it has a `repr(C)` attribute and it has two fields `i8` and `u16`.
//!  (see trait docs for more details)
//! 
//! A `Representation` can then be lowered to a `SlotList` (with `Representation::Slots`), 
//! a close representation of the in memory representation of these types.
//! 
//! ## `Slot` and `SlotList`
//! 
//! A `SlotList` is a linked list of `Slot`s.
//! 
//! A `Slot` can be one of three things,
//! 
//! * `Init` - 1 initialized byte of memory
//! * `Uninit` - 1 (possibly) uninitialized byte of memory
//! * `Markers<M>` - a list of `Marker`s (zero-sized)
//! 
//! Ok, so what's a `Marker`? It is a propery of the associated `Init` bytes.
//! For example, the `bool` `Marker` says that the next byte is either a `0`
//! or a `1`, while the `NonZero<N>` `Marker` says the next `N` bytes contain
//! at least 1 bit which is `1`.
//! 
//! ## Linked Lists
//! 
//! Now, you may be looking back `Representation` and looking at the example funny,
//! why is that linked list backwards?!? Well, that's because the properies of fields
//! and memory lend itself to this representation more readily.
//! 
//! For example, adding a field will to a field list `L` is just `Cons<L, FieldType>`,
//! similarly, adding `3` uninit bytes to slot list `M` is just `Cons<Cons<Cons<M, Uninit>, Uninit>, Uninit>`.
//! No need to worry about what's inside that linked list, because that is guaranteed 
//! to not change because of adding fields.
//! 
//! Similarly the `SlotList` linked list is stored backwards, for `Foo` above
//! 
//! The high level representation was: `Struct<ReprC, Cons<Cons<Nil, i8>, u16>>`
//! 
//! This gets lowered to
//! 
//! ```text
//! Cons<Cons<Cons<Cons<Nil, Init>, Uninit>, Init>, Init>
//!                          ^ i8   ^padding ^ u16
//! ```
//! As seen here,
//! ```rust
//! use chtrans::hlist::{Cons, Nil};
//! use chtrans::slots::{Init, Uninit};
//! use chtrans::repr::{ReprC, r#struct::Struct};
//! use chtrans::Representation;
//! 
//! type Repr = Struct<ReprC, Cons<Cons<Nil, i8>, u16>>;
//! type Slots = Cons<Cons<Cons<Cons<Nil, Init>, Uninit>, Init>, Init>;
//! 
//! fn check_same_type(mut slots: Slots, repr_slots: <Repr as Representation>::Slots) {
//!     // assignment needs the exact same type on both
//!     // sides, so the fact that this compiles shows that
//!     // `Slots` and `Repr::Slots` are indeed the same
//!     slots = repr_slots;
//! }
//! ```
//! 
//! For convience and readability I will use the `HList` macro to describe linked lists
//! from now on. 
//! 
//! Now, what's the funny buisness around `Marker`?
//! 
//! ## `Marker` and `Markers`
//! 
//! What if we have a type which has specific requirements? For eaxmple, `bool`, which
//! requires that it is represented by either a 0 or 1. Then it should be impossible to
//! reinterpret a `u8` as a `bool` even though they are both a single byte.
//! 
//! Well, here come `Markers`!
//! 
//! `Markers<M>` stores a linked list `M`. Each `Marker` is a property of the
//! following memory. For example, `bool`'s `Slots` looks like this:
//! ```rust
//! HList!(Init, Markers<HList!(bool)>)
//! ```
//! Wait, I thought only markers could go in `Markers`. 
//! 
//! Well, turns out I didn't want to create a whole new type that 
//! represents `0` or `1`, so I just reused `bool`.
//! 
//! Also, why is `Init` before `Markers` when you just said \
//! "Each `Marker` is a property of the following memory"
//! 
//! Well, since the linked list is backwards, we iterate through the linked list
//! backwards. This means that `Markers` that are placed later in the linked list 
//! will be encounted before the memory they guard.
//! 
//! To see this in action, let's look into an error, trying to reinterpret a `u8`
//! to a `bool`. Here is a trace:
//! 
//! ```rust text
//! 0_u8.reinterp_into::<bool>();
//! 
//! // This will be lowered to
//! 
//! type BoolSlots = HList!(Init, Markers<HList!(bool)>);
//! type U8Slots = HList!(Init);
//! 
//! BoolSlots: CanConvertFrom<U8Slots>
//! 
//! // Ohh, what's `CanConvertFrom`? (see it's docs for details)
//! 
//! HList!(Init, Markers<HList!(bool)>) : CanConvertFrom<HList!(Init)>
//!     HList!(bool): VerifyMarkers<HList!(Init)>
//!         HList!(Init): IsBool // this fails to compile
//!     HList!(Init) : CanConvertFrom<HList!(Init)>
//!         HList!() : CanConvertFrom<HList!()>
//!             // trivially true
//! ```
//! 
//! Because `IsBool` is looking for a `bool` marker in `HList!(Init)`, and can't find
//! one, it does not allow this conversion
//! 
//! How about the trivial case?
//! 
//! ```rust text
//! false.reinterp_into::<bool>();
//! 
//! // This will be lowered to
//! 
//! type BoolSlots = HList!(Init, Markers<HList!(bool)>);
//! 
//! BoolSlots: CanConvertFrom<BoolSlots>
//! 
//! HList!(Init, Markers<HList!(bool)>) : CanConvertFrom<HList!(Init, Markers<HList!(bool)>)>
//!     HList!(bool): VerifyMarkers<HList!(Init, Markers<HList!(bool)>)>
//!         HList!(Init, Markers<HList!(bool)>): IsBool // This compiles
//!     HList!(Init) : CanConvertFrom<HList!(Init)>
//!         HList!() : CanConvertFrom<HList!()>
//!             // trivially true
//! ```
//! 
//! `IsBool` passes in this case because we can find a `Markers` list containing a `bool`.
//! 
//! Other `Marker`s work by the same principle, they just check a property.
//! Note how `VerifyMarkers` doesn't even check if bytes are initialized
//! just that a `Marker` exists in the source type. These sort of omitions
//! allow `Marker` to compose nicely, because they each check for just 1
//! property.
//! 

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
