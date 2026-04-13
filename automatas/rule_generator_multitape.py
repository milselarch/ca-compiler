from __future__ import annotations

import dataclasses

from py_ca_compiler import (
    A, PyExpression, PyProduct,
    D, PyMultiTapeProduct, PyMultiTapeExpression
)


@dataclasses.dataclass
class MultiTapeOutput:
    tape_no: int
    tape_cell_state: int

    def __hash__(self):
        return hash((self.tape_no, self.tape_cell_state))


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
            MultiTapeOutput
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
            input_terms, MultiTapeOutput(
                tape_no=output_tape_no,
                tape_cell_state=output_cell_state
            )
        ))


class MultiTapeRuleGenerator(object):
    @staticmethod
    def tuple_to_product(terms: tuple[D, ...]) -> PyMultiTapeProduct:
        product = terms[0].to_py_product()
        if len(terms) == 1:
            return product

        for term in terms[1:]:
            product = product.multiply_by_term(term)

        assert isinstance(product, PyMultiTapeProduct)
        return product

    @staticmethod
    def aggregate_bit_or(expr_list: list[
        PyMultiTapeExpression | PyMultiTapeProduct
    ]) -> PyMultiTapeExpression:
        if not expr_list:
            return PyMultiTapeExpression()

        result = expr_list[0]
        for k in range(1, len(expr_list)):
            result = result | expr_list[k]

        return result.to_py_expression()

    @classmethod
    def generate_equations(
        cls, transitions_group: MultiTapeAutomataTransitionsGroup,
    ):
        state_eq_terms_map: dict[
            MultiTapeOutput, list[PyMultiTapeProduct]
        ] = {}

        for transition in transitions_group.transitions:
            input_states, output_state = transition
            if output_state not in state_eq_terms_map:
                state_eq_terms_map[output_state] = []

            product = cls.tuple_to_product(input_states)
            state_eq_terms_map[output_state].append(product)

        state_eq_map: dict[MultiTapeOutput, PyMultiTapeExpression] = {
            next_state: cls.aggregate_bit_or(state_eq_terms_map[next_state])
            for next_state in state_eq_terms_map
        }
        return state_eq_map


class BidirectionalTape(object):
    def __init__(self):
        self.data = []
        self.rev_data = []

    def read(self, position: int) -> int:
        if position >= 0:
            if position >= len(self.data):
                return 0

            return self.data[position]
        else:
            rev_position = -position - 1
            if rev_position >= len(self.rev_data):
                return 0

            return self.rev_data[rev_position]

    def write(self, position: int, value: int):
        if position >= 0:
            while position >= len(self.data):
                self.data.append(0)

            self.data[position] = value
        else:
            rev_position = -position - 1
            while rev_position >= len(self.rev_data):
                self.rev_data.append(0)

            self.rev_data[rev_position] = value


class MultiTape(object):
    def __init__(
        self, state_eq_map: dict[MultiTapeOutput, PyMultiTapeExpression]
    ):
        self.tapes: dict[int, BidirectionalTape] = {}
        self.state_eq_map: dict[
            MultiTapeOutput, PyMultiTapeExpression
        ] = state_eq_map

    @classmethod
    def reverse_state_eq_map(
        cls, state_eq_map: dict[MultiTapeOutput, PyMultiTapeExpression]
    ):
        product_to_state_no_map: dict[PyMultiTapeProduct, MultiTapeOutput] = {}
        raise NotImplementedError
