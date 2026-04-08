from __future__ import annotations

import dataclasses
import math
import typing
import numpy as np

from typing import Final, Literal, Type, TypeVar, Generic
from py_ca_compiler import A, PyExpression, PyProduct, D

T = TypeVar('T', bound=typing.Union[A, D])


@dataclasses.dataclass
class MultiTapeAutomataTransitionsGroup(object):
    """
    contains a set of transitions for a multi-tape cellular automaton
    defined as a mapping from input states to output state
    map D[] -> (output tape_no, output state)
    """
    transitions: list[
        tuple[
            tuple[D, ...],
            tuple[int, int]
        ]
    ]

    @classmethod
    def spawn_new(cls) -> MultiTapeAutomataTransitionsGroup:
        return cls(transitions=[])

    def add_transition(
        self, input_terms: tuple[D, ...],
        output_tape_no: int, output_cell_state: int
    ):
        self.transitions.append((
            input_terms, (output_tape_no, output_cell_state)
        ))


@dataclasses.dataclass
class AutomataTransitionsGroup(object):
    """
    contains a set of transitions for a cellular automaton
    defined as a mapping from input states to output state
    map A[] -> output state
    """
    num_states: int
    transitions: list[
        tuple[
            tuple[A, ...],
            int
        ]
    ]

    @classmethod
    def spawn_new(cls, num_states: int) -> 'AutomataTransitionsGroup':
        return cls(num_states=num_states, transitions=[])

    def add_transition(
        self, input_terms: tuple[A, ...], output_state: int
    ):
        assert 0 <= output_state < self.num_states
        for term in input_terms:
            assert isinstance(term, A)
            state = term.get_state()
            assert 0 <= state < self.num_states

        self.transitions.append((input_terms, output_state))


class TransitionMatrix(object):
    def __init__(self, data: np.ndarray):
        assert data.ndim == 2
        assert data.dtype == np.bool
        assert data.shape[0] == data.shape[1]
        assert isinstance(data, np.ndarray)
        self._data = data

    def __matmul__(self, other: TransitionMatrix) -> TransitionMatrix:
        assert isinstance(other, TransitionMatrix)
        assert self._data.shape == other._data.shape
        result_data = self._data @ other._data
        result_data = result_data.astype(np.bool)
        return TransitionMatrix(result_data)

    def binary_encode(self) -> str:
        encoded_data = ''
        for row in self._data:
            encoded_data += ''.join(['1' if cell else '0' for cell in row])

        return '0b' + encoded_data

    def hex_encode(self) -> str:
        binary_str = self.binary_encode()
        binary_digits = binary_str[2:]  # Remove '0b' prefix
        max_num_hex_digits = math.ceil(len(binary_digits) / 4)
        hex_str = hex(int(binary_str, 2))
        hex_digits = hex_str[2:]
        pad_length = max_num_hex_digits - len(hex_digits)
        padded_hex_str = '0x' + '0' * pad_length + hex_digits
        return padded_hex_str

    @classmethod
    def from_hex_encode(
        cls, encoded_str: str, size: int
    ) -> TransitionMatrix:
        assert encoded_str.startswith('0x')
        int_value = int(encoded_str, 16)
        binary_str = bin(int_value)[2:]  # Remove '0b' prefix
        total_bits = size ** 2
        pad_length = total_bits - len(binary_str)
        padded_binary_str = '0' * pad_length + binary_str

        data = np.zeros((size, size), dtype=np.bool)
        index = 0

        for i in range(size):
            for j in range(size):
                bit = padded_binary_str[index]
                data[i, j] = bit == '1'
                index += 1

        return TransitionMatrix(data)

    @classmethod
    def build_identity(cls, size: int) -> TransitionMatrix:
        identity_data = np.eye(size, dtype=np.bool)
        return TransitionMatrix(identity_data)

    @classmethod
    def from_state(cls, state: int, num_states: int) -> TransitionMatrix:
        assert 0 <= state < num_states
        data = np.zeros((1, num_states), dtype=np.bool)
        data[0, state] = True
        return TransitionMatrix(data)

    def is_idempotent_for_state(self, state: int) -> bool:
        assert 0 <= state < self._data.shape[0]
        value = self._data[state][state]
        # print("VALUE_EXTRACTED", value, type(value))
        assert isinstance(value, np.bool)
        return bool(value)

    def as_uint8_array(self) -> np.ndarray:
        int_data = self._data.astype(np.uint8)
        return int_data

    def get_temporal_start_state(
        self, temporal_end_state: int
    ) -> int:
        assert 0 <= temporal_end_state < self._data.shape[0]
        col = self._data[:, temporal_end_state]
        start_states = np.nonzero(col)[0]

        if len(start_states) == 0:
            raise ValueError(f'No end state for start state {start_states}')

        if len(start_states) > 1:
            raise ValueError(
                f'Multiple end states for start state {start_states}: '
                f'{start_states}'
            )

        return int(start_states[0])

    def get_temporal_end_state(self, temporal_start_state: int) -> int:
        assert 0 <= temporal_start_state < self._data.shape[0]
        row = self._data[temporal_start_state, :]
        end_states = np.nonzero(row)[0]

        if len(end_states) == 0:
            raise ValueError(
                f'No end state for start state {temporal_start_state}'
            )

        if len(end_states) > 1:
            raise ValueError(
                f'Multiple end states for start state {temporal_start_state}: '
                f'{end_states}'
            )

        return int(end_states[0])

    def has_transition(
        self, temporal_start_state: int, temporal_end_state: int
    ) -> bool:
        """
        Check if there is a transition from the start state
        (earlier in time) to the end state (later in time)
        :param temporal_start_state:
        :param temporal_end_state:
        :return:
        """
        assert 0 <= temporal_start_state < self._data.shape[0]
        assert 0 <= temporal_end_state < self._data.shape[0]
        return bool(self._data[temporal_start_state, temporal_end_state])

    def __hash__(self) -> int:
        return hash(self.hex_encode())

    def __eq__(self, other: 'TransitionMatrix') -> bool:
        return self.hex_encode() == other.hex_encode()

    def __repr__(self):
        return (
            f'{self.__class__.__name__}(\n'
            f'{self.as_uint8_array()}'
            f'\n)'
        )


@dataclasses.dataclass
class AutomataRuleSet(object):
    expansion_map: dict[int, PyExpression]
    flat_term_offsets: tuple[int, ...]
    base_num_products: int
    base_terms_per_product: int
    num_flat_terms: int
    num_states: int

    def get_expression_for_state(self, state: int) -> PyExpression:
        assert 0 <= state < self.num_states
        return self.expansion_map[state]

    def get_num_products(self, timesteps: int) -> int:
        return self.base_num_products ** (
            (-1 + self.base_terms_per_product ** timesteps) //
            (-1 + self.base_terms_per_product)
        )

    def get_num_terms(self, timesteps: int) -> int:
        return self.base_terms_per_product ** timesteps

    def get_state_transition_matrix(
        self, expansion_index: int
    ) -> TransitionMatrix:
        assert 0 <= expansion_index < self.num_flat_terms
        raw_transition_matrix = np.zeros(
            (self.num_states, self.num_states), dtype=np.bool
        )

        states = sorted(list(self.expansion_map.keys()))
        for next_state in states:
            expr = self.expansion_map[next_state]
            flat_terms: list[A] = expr.get_flat_terms()
            assert len(flat_terms) == self.num_flat_terms
            term = flat_terms[expansion_index]
            start_state = term.get_state()
            raw_transition_matrix[start_state, next_state] = True

        transition_matrix = TransitionMatrix(raw_transition_matrix)
        return transition_matrix

    def __call__(self, expansion_index: int) -> TransitionMatrix:
        return self.get_state_transition_matrix(expansion_index)

    def get_position_matrices(self, expansion_index: int) -> np.ndarray:
        assert 0 <= expansion_index < self.num_flat_terms
        position_matrix = np.zeros(
            (self.num_states, self.num_states), dtype=np.uint
        )

        states = sorted(list(self.expansion_map.keys()))
        for next_state in states:
            expr = self.expansion_map[next_state]
            flat_terms: list[A] = expr.get_flat_terms()
            assert len(flat_terms) == self.num_flat_terms
            term = flat_terms[expansion_index]
            start_state = term.get_state()
            start_position = term.get_position()
            position_matrix[start_state, next_state] = start_position

        return position_matrix

    def has_transition(
        self, start_state: int, end_state: int, expansion_index: int
    ) -> bool:
        """
        Check if the transition from start_state to end_state
        exists in the transition matrix for the given expansion_index

        Note that start_state is the state before the transition
        (i.e. in the previous timestep), and end_state is the state
        after the transition (i.e. in the current timestep).

        :param start_state:
        :param end_state:
        :param expansion_index:
        :return:
        """
        assert 0 <= expansion_index < self.num_flat_terms
        assert 0 <= start_state < self.num_states
        assert 0 <= end_state < self.num_states
        expr = self.expansion_map[start_state]
        flat_terms: list[A] = expr.get_flat_terms()
        assert len(flat_terms) == self.num_flat_terms
        term = flat_terms[expansion_index]
        term_end_state = term.get_state()
        return term_end_state == end_state

    def get_position_offset(self, expansion_index: int) -> int:
        return self.flat_term_offsets[expansion_index]

    def resolve_state_hist(
        self, initial_state: int, expansion_path: list[int]
    ) -> list[int]:
        state = initial_state
        state_hist: list[int] = [state]

        for expand_index in expansion_path:
            expand_equation = self.expansion_map[state]
            flat_expand_terms = expand_equation.get_flat_terms()
            target_term: A = flat_expand_terms[expand_index]
            state = target_term.get_state()
            state_hist.append(state)

        return state_hist

    def resolve_position_hist(
        self, initial_position: int, expansion_path: list[int]
    ) -> list[int]:
        position = initial_position
        position_hist: list[int] = [position]

        for expand_index in expansion_path:
            step_offset = self.get_position_offset(expand_index)
            position += step_offset
            position_hist.append(position)

        return position_hist

    def build_state_matrix(self, state: int) -> TransitionMatrix:
        raw_data = np.zeros((self.num_states, self.num_states), dtype=np.bool)
        raw_data[state][state] = np.bool(True)
        return TransitionMatrix(raw_data)


class RuleGenerator(object):
    @staticmethod
    def tuple_to_product(terms: tuple[A, ...]) -> PyProduct:
        product = terms[0].to_py_product()
        if len(terms) == 1:
            return product

        for term in terms[1:]:
            product = product.multiply_by_term(term)

        assert isinstance(product, PyProduct)
        return product

    @staticmethod
    def aggregate_bit_or(expr_list: list[
        typing.Union[PyExpression, PyProduct]
    ]) -> PyExpression:
        if not expr_list:
            return PyExpression()

        result = expr_list[0]
        for k in range(1, len(expr_list)):
            result = result | expr_list[k]

        return result.to_py_expression()

    @classmethod
    def to_ruleset(
        cls, transitions_group: AutomataTransitionsGroup,
        verbose: bool = False
    ) -> AutomataRuleSet:
        equations = cls.generate_equations(
            transitions_group, pad_product_length=True,
            pad_expr_length=True, verbose=verbose
        )
        max_flat_terms = 0
        base_num_products = 0
        base_terms_per_product = 0
        base_flat_term_offsets = []

        for state in sorted(equations.keys()):
            assert isinstance(equations[state], PyExpression)
            flat_terms = equations[state].get_flat_terms()
            flat_term_offsets = [term.get_position() for term in flat_terms]

            if not base_flat_term_offsets:
                base_flat_term_offsets = flat_term_offsets
            else:
                assert flat_term_offsets == base_flat_term_offsets, (
                    f'Inconsistent flat term offsets for state {state}: '
                    f'{flat_term_offsets} != {base_flat_term_offsets}'
                )

            assert isinstance(flat_terms, list)
            num_flat_terms = len(flat_terms)
            if max_flat_terms != 0:
                assert num_flat_terms == max_flat_terms

            base_num_products = len(equations[state])
            max_flat_terms = max(max_flat_terms, num_flat_terms)

            for product in equations[state]:
                assert isinstance(product, PyProduct)
                base_terms_per_product = product.get_num_terms()

                for term in product:
                    assert isinstance(term, A)

        assert max_flat_terms > 0
        return AutomataRuleSet(
            num_states=transitions_group.num_states,
            flat_term_offsets=tuple(base_flat_term_offsets),
            expansion_map=equations,
            num_flat_terms=max_flat_terms,
            base_num_products=base_num_products,
            base_terms_per_product=base_terms_per_product
        )

    @classmethod
    def generate_equations(
        cls, transitions_group: AutomataTransitionsGroup,
        pad_product_length: bool = True,
        pad_expr_length: bool = True,
        verbose: bool = False
    ) -> dict[int, PyExpression]:
        """
        generates a mapping from state to expression
        :param transitions_group:
        :param pad_product_length:
        whether to pad the products to the same length
        :param pad_expr_length:
        whether to pad the expressions to the same length
        :param verbose:
        :return:
        """
        def log(*args, **kwargs):
            if verbose:
                print(*args, **kwargs)

        state_eq_terms_map: dict[int, list[PyProduct]] = {}
        for transition in transitions_group.transitions:
            input_states, output_state = transition
            if output_state not in state_eq_terms_map:
                state_eq_terms_map[output_state] = []

            product = cls.tuple_to_product(input_states)
            state_eq_terms_map[output_state].append(product)

        if pad_product_length:
            # ensure that all products have the same length
            max_product_length = 0
            for next_state in state_eq_terms_map:
                for product in state_eq_terms_map[next_state]:
                    log(f'Product for state {next_state}: {product}')
                    log(type(product))

                max_product_length = max([
                    len(product) for product in state_eq_terms_map[next_state]
                ])

            log(f'Padding products to length {max_product_length}')
            assert max_product_length > 0

            for next_state in state_eq_terms_map:
                state_products = state_eq_terms_map[next_state]

                for prod_idx in range(len(state_products)):
                    product = state_products[prod_idx]
                    start_product_length = len(product)
                    pad_length = max_product_length - start_product_length
                    end_term = product[start_product_length - 1]
                    new_product = product

                    for _ in range(pad_length):
                        new_product = new_product.multiply_by_term(end_term)

                    assert len(new_product) == max_product_length
                    state_products[prod_idx] = new_product

        state_eq_map: dict[int, PyExpression] = {
            next_state: cls.aggregate_bit_or(state_eq_terms_map[next_state])
            for next_state in state_eq_terms_map
        }

        if pad_expr_length:
            # ensure that all expressions have the same length
            # i.e. same number of products
            max_expr_length = max([
                len(state_eq_map[next_state]) for next_state in state_eq_map
            ])
            for next_state in state_eq_map:
                start_expr_length = len(state_eq_map[next_state])
                pad_length = max_expr_length - start_expr_length
                end_product = state_eq_map[next_state][start_expr_length - 1]

                for _ in range(pad_length):
                    state_eq_map[next_state] |= end_product

        sorted_states = sorted(list(state_eq_map.keys()))
        for next_state in sorted_states:
            log(f'{next_state} -> {state_eq_map[next_state]}')

        for state in range(transitions_group.num_states):
            err = f'State {state} missing in ruleset'
            assert state in state_eq_terms_map, err

        return state_eq_map


if __name__ == "__main__":
    transitions = AutomataTransitionsGroup(
        transitions=[
            ((A(0, 1),), 1),
            ((A(1, 1), A(1, 0)), 0)
        ],
        num_states=2
    )

    ruleset = RuleGenerator.to_ruleset(
        transitions, verbose=True
    )

    print('RULESET', ruleset)
    print('T0:\n', ruleset(0))
    print('T1:\n', ruleset(1))
    print('T0 @ T1:\n', ruleset(0) @ ruleset(1))
    print('T1 @ T0:\n', ruleset(1) @ ruleset(0))
