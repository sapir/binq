use smallvec::SmallVec;

/// Maps each `K` to a set of `V`s
#[derive(Clone, Debug)]
pub struct SmallMultiMap<K, V>(SmallVec<[(K, V); 3]>);

impl<K, V> SmallMultiMap<K, V>
where
    K: PartialEq,
    V: PartialEq,
{
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn contains_item(&self, key: &K, value: &V) -> bool {
        self.0.iter().any(|(k, v)| k == key && v == value)
    }

    pub fn insert(&mut self, key: K, value: V) {
        if !self.contains_item(&key, &value) {
            self.0.push((key, value));
        }
    }

    pub fn get<'a>(&'a self, key: &'a K) -> impl Iterator<Item = &V> + 'a {
        self.0
            .iter()
            .filter(move |(k, _v)| k == key)
            .map(|(_k, v)| v)
    }
}

impl<K, V> Default for SmallMultiMap<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}
