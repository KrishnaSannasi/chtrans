use crate::{hlist::*, slots::*, Next};

mod non_zero {
    use super::*;

    pub trait HasNonZero<N> {}
    
    impl<P: HasNonZero<N>, N> HasNonZero<Next<N>> for Cons<P, Init> {}
    
    impl<N, M, P> HasNonZero<Next<N>> for Cons<P, Markers<M>>
    where
        M: HasNonZeroMarker,
    {}

    pub trait HasNonZeroMarker {}
    impl<P, N> HasNonZeroMarker for Cons<P, NonZero<N>> {}
    impl<P: HasNonZeroMarker> HasNonZeroMarker for Cons<P, bool> {}
    impl<P: HasNonZeroMarker, T> HasNonZeroMarker for Cons<P, &T> {}
    impl<P: HasNonZeroMarker, T> HasNonZeroMarker for Cons<P, &mut T> {}
}

mod marker_bool {
    use super::*;

    pub trait IsBool {}

    impl<P, M> IsBool for Cons<P, Markers<M>> where M: HasBoolMarker {}

    pub trait HasBoolMarker {}

    impl<P> HasBoolMarker for Cons<P, bool> {}

    impl<P: HasBoolMarker, N> HasBoolMarker for Cons<P, NonZero<N>> {}
    impl<P: HasBoolMarker, T> HasBoolMarker for Cons<P, &T> {}
    impl<P: HasBoolMarker, T> HasBoolMarker for Cons<P, &mut T> {}
}

mod marker_ref {
    use super::*;

    pub trait IsRef<R> {}
    
    impl<P, M, R> IsRef<R> for Cons<P, Markers<M>> where M: HasRefMarker<R> {}
    pub trait HasRefMarker<R> {}

    impl<'a: 'b, 'b, T, P> HasRefMarker<&'a T> for Cons<P, &'b T> {}
    impl<'a: 'b, 'b, T, P> HasRefMarker<&'a T> for Cons<P, &'b mut T> {}
    impl<'a: 'b, 'b, T, P> HasRefMarker<&'a mut T> for Cons<P, &'b mut T> {}
    
    impl<R, P: HasRefMarker<R>> HasRefMarker<R> for Cons<P, bool> {}
    impl<R, P: HasRefMarker<R>, N> HasRefMarker<R> for Cons<P, NonZero<N>> {}
}

pub unsafe trait CanConvertFrom<L> {}

unsafe impl CanConvertFrom<Nil> for Nil {}

unsafe impl<L, P, M> CanConvertFrom<L> for Cons<P, Markers<M>>
where
    M: VerifyMarkers<L>,
    P: CanConvertFrom<L>,
{}

unsafe impl<L, P, M> CanConvertFrom<Cons<L, Markers<M>>> for Cons<P, Init>
where
    Self: CanConvertFrom<L>,
{}

unsafe impl<L, P, M> CanConvertFrom<Cons<L, Markers<M>>> for Cons<P, Uninit>
where
    Self: CanConvertFrom<L>,
{}

unsafe impl<L, P> CanConvertFrom<Cons<L, Init>> for Cons<P, Init>
where
    P: CanConvertFrom<L>,
{}

unsafe impl<L, P> CanConvertFrom<Cons<L, Init>> for Cons<P, Uninit>
where
    P: CanConvertFrom<L>,
{}

unsafe impl<L, P> CanConvertFrom<Cons<L, Uninit>> for Cons<P, Uninit>
where
    P: CanConvertFrom<L>,
{}

pub trait VerifyMarkers<L> {}

impl<L> VerifyMarkers<L> for Nil {}
impl<L, P, N> VerifyMarkers<L> for Cons<P, NonZero<N>>
where
    L: non_zero::HasNonZero<N>,
    P: VerifyMarkers<L>,
{}

impl<L, P> VerifyMarkers<L> for Cons<P, bool>
where
    L: marker_bool::IsBool,
    P: VerifyMarkers<L>,
{}

impl<'a, L, P, T> VerifyMarkers<L> for Cons<P, &'a T>
where
    L: marker_ref::IsRef<&'a T>,
    P: VerifyMarkers<L>,
{}

impl<'a, L, P, T> VerifyMarkers<L> for Cons<P, &'a mut T>
where
    L: marker_ref::IsRef<&'a mut T>,
    P: VerifyMarkers<L>,
{}
