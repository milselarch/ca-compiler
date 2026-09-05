use std::collections::BTreeMap;
use std::fmt;

use indexmap::IndexMap;
use indexmap::IndexSet;

use crate::automata::tape_overlaps::MultiTapeState;
use crate::automata::terms::CellState;
use crate::automata::terms_multitape::{
    AbstractMultiTapeExpression, MultiTapeProduct, TapeNo,
};

/// Writes performed by a single product: `tape_no -> output tape cell state`.
/// `BTreeMap` so iteration is deterministic (ascending `TapeNo`).
pub type TapeWrites = BTreeMap<TapeNo, CellState>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ProductWritesError {
    /// The same product wants to write two different states to the same tape.
    ConflictingOutput {
        product: String,
        tape_no: TapeNo,
        existing: CellState,
        incoming: CellState,
    },
    /// Mutating a frozen map.
    Frozen,
    /// Lookup of a product that is not present in the map.
    MissingProduct { product: String },
}

impl fmt::Display for ProductWritesError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProductWritesError::ConflictingOutput {
                product, tape_no, existing, incoming,
            } => write!(
                f,
                "Conflicting output states for product={} on tape {}: {} vs {}",
                product, tape_no, existing, incoming
            ),
            ProductWritesError::Frozen => {
                write!(f, "Cannot modify ProductWritesMap when frozen")
            }
            ProductWritesError::MissingProduct { product } => {
                write!(f, "Product {} is not in the ProductWritesMap", product)
            }
        }
    }
}
impl std::error::Error for ProductWritesError {}

/// map `product -> tape_no -> output tape cell state`
///
/// `IndexMap` preserves insertion order, mirroring the iteration order of the
/// Python `dict`-backed implementation.
#[derive(Debug, Clone, Default)]
pub struct ProductWritesMap {
    prod_to_state_map: IndexMap<MultiTapeProduct, TapeWrites>,
    frozen: bool,
}

impl ProductWritesMap {
    pub fn new() -> Self {
        Self { prod_to_state_map: IndexMap::new(), frozen: false }
    }

    pub fn freeze(&mut self) {
        self.frozen = true;
    }

    pub fn is_frozen(&self) -> bool {
        self.frozen
    }

    pub fn to_unfrozen(&self) -> Self {
        Self { prod_to_state_map: self.prod_to_state_map.clone(), frozen: false }
    }

    pub fn len(&self) -> usize {
        self.prod_to_state_map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.prod_to_state_map.is_empty()
    }

    pub fn contains_product(&self, product: &MultiTapeProduct) -> bool {
        self.prod_to_state_map.contains_key(product)
    }

    /// Python `__getitem__` (returns a borrow instead of a shallow copy).
    pub fn get(&self, product: &MultiTapeProduct) -> Option<&TapeWrites> {
        self.prod_to_state_map.get(product)
    }

    /// Python `__iter__` over products.
    pub fn products(&self) -> impl Iterator<Item = &MultiTapeProduct> {
        self.prod_to_state_map.keys()
    }

    /// Python `items()`.
    pub fn iter(&self) -> indexmap::map::Iter<'_, MultiTapeProduct, TapeWrites> {
        self.prod_to_state_map.iter()
    }

    /// Python `__delitem__`.
    pub fn remove(
        &mut self, product: &MultiTapeProduct,
    ) -> Result<Option<TapeWrites>, ProductWritesError> {
        if self.frozen {
            return Err(ProductWritesError::Frozen);
        }
        Ok(self.prod_to_state_map.shift_remove(product))
    }

    pub fn insert(
        &mut self, product: MultiTapeProduct, tape_output: MultiTapeState,
    ) -> Result<(), ProductWritesError> {
        self._insert(product, tape_output.tape_no, tape_output.tape_cell_state)
    }

    fn _insert(
        &mut self,
        product: MultiTapeProduct,
        write_tape_no: TapeNo,
        write_tape_cell_state: CellState,
    ) -> Result<(), ProductWritesError> {
        if self.frozen {
            return Err(ProductWritesError::Frozen);
        }

        let product_repr = product._to_string("D");
        let writes_map = self.prod_to_state_map.entry(product).or_default();
        let existing = writes_map
            .get(&write_tape_no)
            .copied()
            .unwrap_or(write_tape_cell_state);

        if existing != write_tape_cell_state {
            return Err(ProductWritesError::ConflictingOutput {
                product: product_repr,
                tape_no: write_tape_no,
                existing,
                incoming: write_tape_cell_state,
            });
        }

        writes_map.insert(write_tape_no, write_tape_cell_state);
        Ok(())
    }

    /// Insert a product whose outputs rewrite the input terms with offset 0
    /// so that they keep the same state.
    pub fn insert_neutral_product(
        &mut self, product: &MultiTapeProduct,
    ) -> Result<(), ProductWritesError> {
        for term in product.to_flat_terms() {
            if term.position != 0 {
                continue;
            }
            let (tape_no, tape_cell_state) = term.state;
            self._insert(product.copy(), tape_no, tape_cell_state)?;
        }
        Ok(())
    }

    pub fn merge(&mut self, other: &ProductWritesMap) -> Result<(), ProductWritesError> {
        for (product, writes) in other.iter() {
            for (tape_no, tape_cell_state) in writes.iter() {
                self._insert(product.copy(), *tape_no, *tape_cell_state)?;
            }
        }
        Ok(())
    }

    pub fn get_state_writes_for(
        &self, product: &MultiTapeProduct,
    ) -> Result<Vec<MultiTapeState>, ProductWritesError> {
        let writes_map = self.get(product).ok_or_else(|| {
            ProductWritesError::MissingProduct { product: product._to_string("D") }
        })?;

        Ok(writes_map
            .iter()
            .map(|(tape_no, cell_state)| MultiTapeState::new(*tape_no, *cell_state))
            .collect())
    }

    /// All (input) states referenced by the products in this map.
    pub fn get_states_set(&self) -> IndexSet<MultiTapeState> {
        let mut states_set: IndexSet<MultiTapeState> = IndexSet::new();

        for product in self.products() {
            for term in product.to_flat_terms() {
                let (tape_no, tape_cell_state) = term.state;
                states_set.insert(MultiTapeState::new(tape_no, tape_cell_state));
            }
        }
        states_set
    }

    /// maps state -> products that produce it in their output writes.
    pub fn build_state_to_products_map(
        &self,
    ) -> BTreeMap<MultiTapeState, Vec<MultiTapeProduct>> {
        let mut state_to_products: BTreeMap<MultiTapeState, Vec<MultiTapeProduct>> =
            BTreeMap::new();

        for (product, writes) in self.iter() {
            for (tape_no, tape_cell_state) in writes.iter() {
                let state = MultiTapeState::new(*tape_no, *tape_cell_state);
                state_to_products.entry(state).or_default().push(product.copy());
            }
        }
        state_to_products
    }

    /// maps state -> products that contain it in their input terms.
    pub fn build_input_state_to_prod_map(
        &self,
    ) -> BTreeMap<MultiTapeState, Vec<MultiTapeProduct>> {
        let mut input_state_to_prod: BTreeMap<MultiTapeState, Vec<MultiTapeProduct>> =
            BTreeMap::new();

        for product in self.products() {
            for term in product.to_flat_terms() {
                let (tape_no, tape_cell_state) = term.state;
                let state = MultiTapeState::new(tape_no, tape_cell_state);
                input_state_to_prod.entry(state).or_default().push(product.copy());
            }
        }
        input_state_to_prod
    }
}

impl<'a> IntoIterator for &'a ProductWritesMap {
    type Item = (&'a MultiTapeProduct, &'a TapeWrites);
    type IntoIter = indexmap::map::Iter<'a, MultiTapeProduct, TapeWrites>;

    fn into_iter(self) -> Self::IntoIter {
        self.prod_to_state_map.iter()
    }
}
