use std::collections::HashMap;

#[derive(Clone)]
pub struct AppendOnlyHashMap<K, V> {
    // This might all be a bit overkill
    map: HashMap<K, V>,
}
impl<K: std::hash::Hash + Eq + Clone, V: Clone> AppendOnlyHashMap<K, V> {
    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, K, V> {
        self.map.iter()
    }
}
impl<K: std::hash::Hash + Eq + Clone, V: Clone> IntoIterator for AppendOnlyHashMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::collections::hash_map::IntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}
impl<K: std::hash::Hash + Eq + Clone, V: Clone> AppendOnlyHashMap<K,V> {
    pub fn new() -> Self {
        AppendOnlyHashMap {
        map: HashMap::new()
    }
    }
    pub fn insert(&mut self, key: K, value: V) -> Result<Option<V>, ()> {
        if self.map.contains_key(&key) {
            Err(())
        } else {
            Ok(self.map.insert(key, value))
        }
    }
    pub fn contains_key(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }
    pub fn get(&self, key: &K) -> Option<&V> {
        self.map.get(key)
    }
    pub fn to_hash_map(self) -> HashMap<K, V> {
        self.map
    }
    pub fn to_buffered(&self) -> BufferedHashMap<K, V> {
        BufferedHashMap::new(self)
    }
    pub fn get_inner_map(&self) -> &HashMap<K, V> {
        &self.map
    }
}
impl <
    'a, K: std::hash::Hash + Eq + Clone, V: Clone
> DiffableHashMap<K, V> for AppendOnlyHashMap<K, V> {
    fn to_hash_map(&self) -> HashMap<K, V> {
        self.map.clone()
    }
    fn from_hash_map(map: HashMap<K, V>) -> Self where Self: Sized {
        AppendOnlyHashMap { map }
    }
    fn insert(&mut self, key: K, value: V) -> Result<Option<V>, ()> {
        self.insert(key, value)
    }
    fn get(&self, key: &K) -> Option<&V> {
        self.get(key)
    }
    fn contains_key(&self, key: &K) -> bool {
        self.contains_key(key)
    }
    fn build_changes(&self) -> AppendOnlyHashMap<K, V> {
        AppendOnlyHashMap::new()
    }
    fn apply_changes(&mut self, changes: HashMap<K, V>) -> Result<(), ()> {
        for (k, v) in changes.into_iter() {
            self.insert(k, v)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct BufferedHashMap<'a, K: Clone, V: Clone> {
    read_only: &'a dyn DiffableHashMap<K, V>,
    buffer: AppendOnlyHashMap<K, V>
}
impl <
    'a, K: std::hash::Hash + Eq + Clone, V: Clone
> BufferedHashMap<'a, K,V> {
    pub fn new(base: &'a dyn DiffableHashMap<K, V>) -> Self {
        BufferedHashMap {
            read_only: base,
            buffer: AppendOnlyHashMap::new()
        }
    }
    pub fn insert(&mut self, key: K, value: V) -> Result<Option<V>, ()> {
        if self.read_only.get(&key).is_some() || self.buffer.contains_key(&key) {
            Err(())
        } else {
            self.buffer.insert(key, value)
        }
    }
    pub fn get_source_ref(&self) -> &'a dyn DiffableHashMap<K, V> {
        self.read_only
    }
    pub fn get(&self, key: &K) -> Option<&V> {
        if let Some(value) = self.buffer.get(key) {
            Some(value)
        } else {
            self.read_only.get(key)
        }
    }
}
impl <
    'a, K: std::hash::Hash + Eq + Clone, V: Clone
> DiffableHashMap<K, V> for BufferedHashMap<'a, K, V> {
    fn to_hash_map(&self) -> HashMap<K, V> {
        let mut combined = self.read_only.to_hash_map();
        for (k, v) in self.buffer.iter() {
            combined.insert(k.clone(), v.clone());
        }
        combined
    }
    fn from_hash_map(_map: HashMap<K, V>) -> Self where Self: Sized {
        unimplemented!("Cannot create BufferedHashMap from HashMap directly");
    }
    fn insert(&mut self, key: K, value: V) -> Result<Option<V>, ()> {
        self.insert(key, value)
    }
    fn get(&self, key: &K) -> Option<&V> {
        self.get(key)
    }
    fn contains_key(&self, key: &K) -> bool {
        self.get(key).is_some()
    }

    fn build_changes(&self) -> AppendOnlyHashMap<K, V> { self.buffer.clone() }
    fn apply_changes(&mut self, changes: HashMap<K, V>) -> Result<(), ()> {
        for (k, v) in changes.into_iter() {
            self.insert(k, v)?;
        }
        Ok(())
    }
}

pub trait DiffableHashMap<K: Clone + Eq + std::hash::Hash, V: Clone> {
    fn to_hash_map(&self) -> HashMap<K, V>;
    fn from_hash_map(map: HashMap<K, V>) -> Self where Self: Sized;
    fn insert(&mut self, key: K, value: V) -> Result<Option<V>, ()>;
    fn get(&self, key: &K) -> Option<&V>;
    fn contains_key(&self, key: &K) -> bool;
    fn build_changes(&self) -> AppendOnlyHashMap<K, V>;
    fn apply_changes(&mut self, changes: HashMap<K, V>) -> Result<(), ()> {
        for (k, v) in changes.into_iter() {
            self.insert(k, v)?;
        }
        Ok(())
    }
    fn to_buffered(&self) -> BufferedHashMap<K, V> where Self: Sized {
        BufferedHashMap::new(self)
    }
}

pub struct StackAllocationResult {
    pub new_stack_value: u64,
    // new allocations of variable ids to their stack addresses
    pub new_stack_allocations: HashMap<u64, u64>
}
impl StackAllocationResult {
    pub fn new(new_stack_value: u64) -> Self {
        StackAllocationResult {
            new_stack_value,
            new_stack_allocations: HashMap::new()
        }
    }
    pub fn new_with_allocations(
        new_stack_value: u64,
        new_var_stack_allocations: HashMap<u64, u64>
    ) -> Self {
        // TODO: make this private (redirect calls to new_from_buffered)
        StackAllocationResult {
            new_stack_value,
            new_stack_allocations: new_var_stack_allocations
        }
    }
    pub fn new_from_buffered(
        new_stack_value: u64,
        buffered_allocations: BufferedHashMap<u64, u64>
    ) -> Self {
        StackAllocationResult {
            new_stack_value,
            new_stack_allocations: buffered_allocations.build_changes().to_hash_map()
        }
    }
}

pub trait ToStackAllocated {
    fn to_stack_allocated(
        &self, stack_value: u64,
        // pseudo-register ID to stack address offset
        allocations: &dyn DiffableHashMap<u64, u64>
        // returns a tuple of (Self, new stack_value)
    ) -> (Self, StackAllocationResult) where Self: Sized;
}
