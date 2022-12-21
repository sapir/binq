#[derive(Clone, Debug)]
pub struct VecSet<T>(Vec<T>);

impl<T> VecSet<T>
where
    T: PartialEq,
{
    pub fn contains(&self, value: &T) -> bool {
        self.0.contains(value)
    }

    pub fn insert(&mut self, value: T) {
        if !self.contains(&value) {
            self.0.push(value);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter()
    }
}

impl<T> Default for VecSet<T> {
    fn default() -> Self {
        Self(vec![])
    }
}

impl<A> Extend<A> for VecSet<A>
where
    A: PartialEq,
{
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        for value in iter.into_iter() {
            self.insert(value);
        }
    }
}
