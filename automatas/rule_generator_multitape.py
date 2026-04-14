from __future__ import annotations

import copy
import dataclasses

from collections import defaultdict
from py_ca_compiler import (
    A, PyExpression, PyProduct,
    D, PyMultiTapeProduct, PyMultiTapeExpression
)


class TapeNo(int):
    pass


class TapeCellState(int):
    pass


@dataclasses.dataclass
class MultiTapeOutput:
    tape_no: TapeNo
    tape_cell_state: TapeCellState

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
                tape_no=TapeNo(output_tape_no),
                tape_cell_state=TapeCellState(output_cell_state)
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

    def get_range(self) -> tuple[int, int]:
        min_pos = -len(self.rev_data)
        max_pos = len(self.data) - 1
        return min_pos, max_pos

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

    def __getitem__(self, position: int) -> int:
        return self.read(position)

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


class BiDirectionalMultiTape(object):
    def __init__(self, tapes: dict[int, BidirectionalTape] | None = None):
        if tapes is None:
            tapes = {}

        self.tapes: dict[int, BidirectionalTape] = tapes

    def get_or_make_tape(self, tape_no: int) -> BidirectionalTape:
        if tape_no not in self.tapes:
            self.tapes[tape_no] = BidirectionalTape()

        return self.tapes[tape_no]

    def get_range(self):
        min_pos, max_pos = 0, 0

        for tape in self.tapes.values():
            tape_min, tape_max = tape.get_range()
            min_pos = min(min_pos, tape_min)
            max_pos = max(max_pos, tape_max)

        return min_pos, max_pos


class MultiTapeAutomata(object):
    def __init__(
        self, state_eq_map: dict[MultiTapeOutput, PyMultiTapeExpression]
    ):
        self.multi_tape = BiDirectionalMultiTape()
        self.prod_to_state_map = self.reverse_state_eq_map(state_eq_map)
        self.state_eq_map = state_eq_map

    @classmethod
    def reverse_state_eq_map(
        cls, state_eq_map: dict[MultiTapeOutput, PyMultiTapeExpression]
    ) -> defaultdict[PyMultiTapeProduct, dict[TapeNo, TapeCellState]]:
        # map product -> tape_no -> output tape cell state
        prod_to_state_map: defaultdict[
            PyMultiTapeProduct, defaultdict[TapeNo, TapeCellState]
        ] = defaultdict(dict)

        for multi_tape_output, expr in state_eq_map.items():
            products = expr.get_flat_products()
            write_tape_no = multi_tape_output.tape_no
            write_tape_cell_state = multi_tape_output.tape_cell_state

            for product in products:
                writes_map = prod_to_state_map[product]
                existing_tape_write_state = writes_map.get(
                    write_tape_no, write_tape_cell_state
                )
                if existing_tape_write_state != write_tape_cell_state:
                    raise ValueError(
                        f"Conflicting output states for {product=} "
                        f"on tape {write_tape_no}: "
                        f"{existing_tape_write_state} vs "
                        f"{write_tape_cell_state}"
                    )

                writes_map[write_tape_no] = write_tape_cell_state

        return prod_to_state_map

    def product_satisfies(
        self, product: PyMultiTapeProduct, position: int
    ) -> bool:
        """
        check if the given product is satisfied at the given
        position on the tapes
        :param product:
        :param position:
        :return:
        """
        for term in product.get_flat_terms():
            tape_no, tape_cell_state = term.get_state()
            tape = self.multi_tape.get_or_make_tape(tape_no)
            if tape.read(position) != tape_cell_state:
                return False

        return True

    def step(self):
        # TODO: option for ensuring rules have 0 ambiguity
        # i.e. no void states filled in by default
        min_pos, max_pos = self.multi_tape.get_range()
        new_multi_tape = copy.deepcopy(self.multi_tape)

        for position in range(min_pos, max_pos + 1):
            # TODO: copy over existing values
            for matching_product in self.prod_to_state_map:
                if not self.product_satisfies(matching_product, position):
                    continue

                writes_map = self.prod_to_state_map[matching_product]
                for tape_no, tape_cell_state in writes_map.items():
                    output_tape = new_multi_tape.get_or_make_tape(tape_no)
                    output_tape.write(position, tape_cell_state)


