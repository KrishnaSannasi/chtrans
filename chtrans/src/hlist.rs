
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

pub type Repeated<T, N> = <T as Repeat<N>>::Output;
pub trait Repeat<N> {
    type Output;
}

impl<T, N> Repeat<N> for T
where
    N: Unsigned + IsEqual<U0>,
    T: RepeatImpl<N, Eq<N, U0>>,
{
    type Output = T::ImplOutput;
}

pub trait RepeatImpl<N, IsZero> {
    type ImplOutput;
}

impl<T, N: Unsigned + IsEqual<U1> + Sub<U1>> RepeatImpl<N, B0> for T
where
    T: RepeatImpl<Diff<N, U1>, Eq<N, U1>>,
{
    type ImplOutput = Cons<T::ImplOutput, T>;
}

impl<T> RepeatImpl<U0, B1> for T {
    type ImplOutput = Nil;
}

pub type Appended<A, B> = <A as Append<B>>::Output;
pub trait Append<T> {
    type Output;
}

pub trait AppendTo<T> {
    type Output;
}

impl<L, T> Append<L> for T
where
    L: AppendTo<Self>,
{
    type Output = L::Output;
}

impl<L> AppendTo<L> for Nil {
    type Output = L;
}

impl<L, R, T> AppendTo<L> for Cons<R, T>
where
    R: AppendTo<L>,
{
    type Output = Cons<R::Output, T>;
}

pub type CycleL<L, N> = <L as Cycle<N>>::Output;
pub trait Cycle<N> {
    type Output;
}

impl<T, N> Cycle<N> for T
where
    N: IsEqual<U0>,
    T: ImplCycle<N, Eq<N, U0>>,
{
    type Output = T::ImplOutput;
}

pub trait ImplCycle<N, IsZero> {
    type ImplOutput;
}

impl<T> ImplCycle<U0, B1> for T {
    type ImplOutput = Nil;
}

impl<T, N> ImplCycle<N, B0> for T
where
    N: Sub<U1>,
    T: Cycle<N::Output>,
    CycleL<T, N::Output>: Append<T>,
{
    type ImplOutput = Appended<CycleL<T, N::Output>, T>;
}
