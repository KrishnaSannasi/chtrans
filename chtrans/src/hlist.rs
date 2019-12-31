
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
    Nil: Push<T, N>,
{
    type Output = PushTo<Nil, T, N>;
}

pub type Appended<A, B> = <A as Append<B>>::Output;
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

pub type PushTo<L, T, N> = <L as Push<T, N>>::Output;
pub trait Push<T, N> {
    type Output;
}

impl<T, N, L> Push<T, N> for L
where
    N: IsEqual<U0>,
    L: ImplPush<T, N, Eq<N, U0>>,
{
    type Output = L::ImplOutput;
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