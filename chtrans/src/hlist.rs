
//! Type level linked lists
//! 
//! # `chtrans::hlist`
//! 
//! Linked lists are represented by a cons list,
//! 
//! In `Cons<P, T>`, `P` is the rest of the list, and `T` is the current element
//! `Nil` represents the end of the list.
//! 
//! There are a few helper traits to make working with linked lists easier.
//! Each trait comes with a type alias that gets the output
//! 

#[macro_export]
macro_rules! HList {
    () => { $crate::hlist::Nil };

    (..$first:ty $(,)?) => { $first };

    ($first:ty $(, $type:ty)* $(,)?) => {
        $crate::HList!(..$crate::hlist::Cons<$crate::hlist::Nil, $first>, $($type),*)
    };
    (..$prefix:ty, $first:ty $(, $type:ty)* $(,)?) => {
        $crate::HList!(..$crate::hlist::Cons<$prefix, $first>, $($type),*)
    };
}

use typenum::{*, operator_aliases::{Eq, Diff}};
use core::ops::Sub;

pub struct Cons<P, T> {
    _prev: P,
    _next: T,
}

pub enum Nil {}

/// Gets the output of [`Repeat`](crate::hlist::Repeat)
pub type Repeated<T, N> = <T as Repeat<N>>::Output;

/// The appends the value `Self` to the end of empty list `N` times
/// 
/// You can easily access `Output` using [`Repeated<Self, N>`](crate::hlist::Repeated)
/// 
/// For example `Repeated<u32, U3>` yields `HList!(u32, u32, u32)`
pub trait Repeat<N> {
    type Output;
}

impl<T, N> Repeat<N> for T
where
    Nil: Push<T, N>,
{
    type Output = PushTo<Nil, T, N>;
}

/// Gets the output of [`Append`](crate::hlist::Append)
pub type Appended<A, B> = <A as Append<B>>::Output;

/// The appends linked list `T` to the end of linked list `Self`
/// 
/// You can easily access `Output` using [`Appended<Self, T>`](crate::hlist::Appended)
/// 
/// For example, `Appended<HList!(u32, i32), HList!(f32, bool)>` yields `HList!(u32, i32, f32, bool)`
pub trait Append<T> {
    type Output;
}

impl<L> Append<Nil> for L {
    type Output = L;
}

impl<L, P, T> Append<Cons<P, T>> for L
where
    L: Append<P>,
{
    type Output = Cons<Appended<L, P>, T>;
}

/// Gets the output of [`Cyclic`](crate::hlist::Cyclic)
pub type Cycle<L, N> = <L as Cyclic<N>>::Output;

/// This appends the linked list `A` `N` times to the empty list,
/// where `N` is an unsigned integer (typenum).
/// 
/// You can easily access `Output` using [`Cycle<Self, N>`](crate::hlist::Cycle)
/// 
/// For example `Cycle<HList!(u32, i32), U3>` yields `HList!(u32, i32, u32, i32, u32, i32)`
pub trait Cyclic<N> {
    type Output;
}

impl<T, N> Cyclic<N> for T
where
    N: IsEqual<U0>,
    T: private::ImplCycle<N, Eq<N, U0>>,
{
    type Output = T::ImplOutput;
}

/// Get the output of [`Push`](crate::hlist::Push)
pub type PushTo<L, T, N> = <L as Push<T, N>>::Output;

/// Pushes the value `T` onto the end of the list `N` times,
/// where `N` is an unsigned integer (typenum).
/// 
/// You can easily access `Output` using [`PushTo<Self, T, N>`](crate::hlist::PushTo)
/// 
/// For example `PushTo<HList!(i32, f32), u32, U3>` yields `HList!(i32, f32, u32, u32, u32)`
pub trait Push<T, N> {
    type Output;
}

impl<T, N, L> Push<T, N> for L
where
    N: IsEqual<U0>,
    L: private::ImplPush<T, N, Eq<N, U0>>,
{
    type Output = L::ImplOutput;
}

mod private {
    use super::*;

    pub trait ImplCycle<N, IsZero> {
        type ImplOutput;
    }
    
    impl<T> ImplCycle<U0, B1> for T {
        type ImplOutput = Nil;
    }
    
    impl<T, N> ImplCycle<N, B0> for T
    where
        N: Sub<U1>,
        T: Cyclic<N::Output>,
        Cycle<T, N::Output>: Append<T>,
    {
        type ImplOutput = Appended<Cycle<T, N::Output>, T>;
    }
    
    pub trait ImplPush<T, N, IsZero> {
        type ImplOutput;
    }

    impl<L, T> ImplPush<T, U0, B1> for L {
        type ImplOutput = Self;
    }

    impl<L, T, N> ImplPush<T, N, B0> for L
    where
        N: Sub<U1>,
        N: IsEqual<U1>,
        Self: ImplPush<T, Diff<N, U1>, Eq<N, U1>>,
    {
        #[allow(clippy::type_complexity)]
        type ImplOutput = Cons<<Self as ImplPush<T, Diff<N, U1>, Eq<N, U1>>>::ImplOutput, T>;
    }
}