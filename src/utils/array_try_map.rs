use std::mem::MaybeUninit;

pub trait ArrayTryMap
where
    Self: Sized,
{
    type Item;
    type Out<R>;

    fn try_map<R, E>(self, f: impl FnMut(Self::Item) -> Result<R, E>) -> Result<Self::Out<R>, E>;
}

impl<const N: usize, T> ArrayTryMap for [T; N] {
    type Item = T;
    type Out<R> = [R; N];

    fn try_map<R, E>(self, mut f: impl FnMut(T) -> Result<R, E>) -> Result<[R; N], E> {
        let mut tmp: [MaybeUninit<R>; N] = [(); N].map(|()| MaybeUninit::uninit());

        for (item, out) in self.into_iter().zip(tmp.iter_mut()) {
            *out = MaybeUninit::new(f(item)?);
        }

        Ok(tmp.map(|maybe| unsafe { maybe.assume_init() }))
    }
}
