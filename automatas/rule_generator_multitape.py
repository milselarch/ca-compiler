from __future__ import annotations

import copy
import dataclasses

from collections import defaultdict
from typing import Final

from py_ca_compiler import (
    D, PyMultiTapeProduct, PyMultiTapeExpression
)

from automatas.counter_automata import VOID_STATE
from renderer import RenderFrame


class TapeNo(int):
    pass


class TapeCellState(int):
    pass


VOID_STATE: Final[TapeCellState] = 0
BLANK_INT: Final[int] = -1


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
    ) -> dict[MultiTapeOutput, PyMultiTapeExpression]:
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


class TapeRenderFrame(RenderFrame):
    def __init__(self, line: str, num_cells: int, cell_width: int):
        super().__init__([line])
        self.num_cells = num_cells
        self.cell_width = cell_width

    def get_space_consumed(self) -> int:
        return self.num_cells * (self.cell_width + 1)


class BidirectionalTape(object):
    def __init__(self):
        self.data: list[TapeCellState] = []
        self.rev_data: list[TapeCellState] = []

    def get_range(self) -> tuple[int, int]:
        min_pos = -len(self.rev_data)
        max_pos = len(self.data) - 1
        return min_pos, max_pos

    def get_all_states(self) -> set[TapeCellState]:
        # TODO: consider tracking unique states instead of recomputing
        return set(self.data) | set(self.rev_data)

    def prune(self) -> tuple[int, int]:
        forward_popped, reverse_popped = 0, 0
        # prune leading zeros in both directions
        while self.data and self.data[-1] == VOID_STATE:
            forward_popped += 1
            self.data.pop()
        while self.rev_data and self.rev_data[-1] == VOID_STATE:
            reverse_popped += 1
            self.rev_data.pop()

        return forward_popped, reverse_popped

    def get_minimal_data_region(self) -> list[TapeCellState]:
        """
        Get the minimal contiguous region of tape data
        that contains all non-void states.
        :return:
        """
        self.prune()
        return self.data + self.rev_data

    def render_line(
        self, start_position: int, length: int,
        cell_width: int = BLANK_INT
    ) -> TapeRenderFrame:
        all_states = self.get_all_states()
        max_state = VOID_STATE if not all_states else max(all_states)

        if cell_width == BLANK_INT:
            cell_width = len(str(max_state))
        elif cell_width < len(str(max_state)):
            raise ValueError(
                f"Cell width {cell_width} is too small to fit "
                f"the largest state {max_state}"
            )

        cells_to_render = length // (cell_width + 1)
        line: str = ""

        for k in range(cells_to_render):
            position = start_position + k
            state = self.read(position)
            line += str(state).rjust(cell_width, '0') + "|"

        line += " " * (length - len(line))
        return TapeRenderFrame(
            line=line, num_cells=cells_to_render,
            cell_width=cell_width
        )

    def read(self, position: int) -> TapeCellState:
        if position >= 0:
            if position >= len(self.data):
                return VOID_STATE

            return self.data[position]
        else:
            rev_position = -position - 1
            if rev_position >= len(self.rev_data):
                return VOID_STATE

            return self.rev_data[rev_position]

    def __getitem__(self, position: int) -> TapeCellState:
        return self.read(position)

    def write(self, position: int, value: TapeCellState):
        if position >= 0:
            while position >= len(self.data):
                self.data.append(VOID_STATE)

            self.data[position] = value
        else:
            rev_position = -position - 1
            while rev_position >= len(self.rev_data):
                self.rev_data.append(VOID_STATE)

            self.rev_data[rev_position] = value

    def write_region(
        self, position: int, end_position: int,
        values: list[TapeCellState]
    ):
        for new_position in range(position, end_position+1):
            offset = new_position - position
            value = values[offset % len(values)]
            self.write(new_position, value)


class BiDirectionalMultiTape(object):
    def __init__(self, tapes: dict[TapeNo, BidirectionalTape] | None = None):
        if tapes is not None:
            self.tapes: dict[TapeNo, BidirectionalTape] = tapes
        else:
            self.tapes: dict[TapeNo, BidirectionalTape] = {}

    def get_or_make_tape(self, tape_no: TapeNo) -> BidirectionalTape:
        if tape_no not in self.tapes:
            self.tapes[tape_no] = BidirectionalTape()

        return self.tapes[tape_no]

    def init_tapes(self, tape_nos: list[TapeNo]):
        for tape_no in tape_nos:
            self.get_or_make_tape(tape_no)

    def write_region(
        self, position: int, end_position: int,
        data: list[MultiTapeOutput]
    ):
        for new_position in range(position, end_position+1):
            offset = new_position - position
            value = data[offset % len(data)]
            self.write(new_position, value)

    def write(self, position: int, value: MultiTapeOutput):
        tape = self.get_or_make_tape(value.tape_no)
        tape.write(position, value.tape_cell_state)

    def get_all_states(self) -> set[TapeCellState]:
        all_states = set()
        for tape in self.tapes.values():
            all_states |= tape.get_all_states()

        return all_states

    def prune(self):
        tape_nos = sorted(set(self.tapes.keys()))

        for tape_no in tape_nos:
            tape = self.tapes[tape_no]
            tape.prune()

    def render_tapes(
        self, start_position: int, length: int,
        cell_width: int = BLANK_INT
    ) -> RenderFrame:
        all_states = self.get_all_states()
        max_state = VOID_STATE if not all_states else max(all_states)

        if cell_width == BLANK_INT:
            cell_width = len(str(max_state))
        elif cell_width < len(str(max_state)):
            raise ValueError(
                f"Cell width {cell_width} is too small to fit "
                f"the largest state {max_state}"
            )

        tape_nos = sorted(set(self.tapes.keys()) | {TapeNo(0)})
        left_tabs = []

        for tape_no in tape_nos:
            left_tab = f"Tape {tape_no}: "
            left_tabs.append(left_tab)

        if left_tabs:
            max_left_tab_width = max([len(tab) for tab in left_tabs])
        else:
            max_left_tab_width = 0

        left_sidebar = RenderFrame(left_tabs)
        content_width = length - max_left_tab_width
        tape_view_lines: list[TapeRenderFrame] = []

        for tape_no in tape_nos:
            tape = self.tapes[tape_no]
            tape_line = tape.render_line(
                start_position=start_position,
                length=content_width,
                cell_width=cell_width
            )
            tape_view_lines.append(tape_line)

        # TODO: align by actual space consumed by tape
        num_cells = tape_view_lines[0].num_cells if tape_view_lines else 0
        # width of text actually consumed by tape cells, excluding padding
        tape_content_width = tape_view_lines[0].get_space_consumed()
        start_pos_str = str(start_position) + '<'
        end_pos_str = '>' + str(start_position + num_cells - 1)

        buffer_len = tape_content_width - len(start_pos_str) - len(end_pos_str)
        position_str = (
            ' ' * left_sidebar.get_width() +
            start_pos_str +
            ' ' * buffer_len +
            end_pos_str +
            ' ' * (content_width - tape_content_width)
        )

        tapes_frame = RenderFrame.join_vertically(tape_view_lines)
        return RenderFrame.join_vertically([
            RenderFrame.from_line(position_str),
            RenderFrame.join_horizontally([
                left_sidebar, tapes_frame
            ])
        ])

    def get_tape_nos(self) -> list[TapeNo]:
        return list(self.tapes.keys())

    def get_range(self):
        min_pos, max_pos = 0, 0

        for tape in self.tapes.values():
            tape_min, tape_max = tape.get_range()
            min_pos = min(min_pos, tape_min)
            max_pos = max(max_pos, tape_max)

        return min_pos, max_pos


@dataclasses.dataclass
class WriteRecord(object):
    origin_product: PyMultiTapeProduct
    write_target: tuple[TapeNo, int]  # (tape_no, position)
    tape_cell_state: TapeCellState

    def log(self):
        print(
            f'{self.origin_product} | {self.write_target} '
            f'-> {self.tape_cell_state}'
        )


@dataclasses.dataclass
class ProcessStepResult(object):
    prev_multi_tape: BiDirectionalMultiTape
    new_multi_tape: BiDirectionalMultiTape
    active_writes: list[WriteRecord]


class MultiTapeAutomata(object):
    def __init__(
        self, state_eq_map: dict[MultiTapeOutput, PyMultiTapeExpression]
    ):
        self._multi_tape: BiDirectionalMultiTape = BiDirectionalMultiTape()
        self._prod_to_state_map = self.reverse_state_eq_map(state_eq_map)
        self._max_radius = self.get_max_radius()
        self._state_eq_map = state_eq_map

    def get_max_radius(self) -> int:
        max_radius: int = 0

        for product in self._prod_to_state_map:
            terms = product.get_flat_terms()

            for term in terms:
                offset = abs(term.get_position())
                max_radius = max(max_radius, offset)

        return max_radius

    def init_tapes(self, tape_nos: list[TapeNo]):
        self._multi_tape.init_tapes(tape_nos)

    def write_region(
        self, position: int, end_position: int,
        data: list[MultiTapeOutput]
    ):
        self._multi_tape.write_region(
            position=position, end_position=end_position,
            data=data
        )

    def render_tapes(
        self, start_position: int, length: int,
        cell_width: int = BLANK_INT
    ) -> RenderFrame:
        return self._multi_tape.render_tapes(
            start_position=start_position, length=length,
            cell_width=cell_width
        )

    @classmethod
    def reverse_state_eq_map(
        cls, state_eq_map: dict[MultiTapeOutput, PyMultiTapeExpression]
    ) -> defaultdict[PyMultiTapeProduct, dict[TapeNo, TapeCellState]]:
        # map product -> tape_no -> output tape cell state
        prod_to_state_map: defaultdict[
            PyMultiTapeProduct, dict[TapeNo, TapeCellState]
        ] = defaultdict(lambda: dict())

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
            term_offset = term.get_position()
            tape_no, tape_cell_state = term.get_state()
            tape_no = TapeNo(tape_no)
            tape_cell_state = TapeCellState(tape_cell_state)

            tape = self._multi_tape.get_or_make_tape(tape_no)
            term_position = position + term_offset
            if tape.read(term_position) != tape_cell_state:
                return False

        return True

    def process_step(
        self, log_active_writes: bool = True
    ) -> ProcessStepResult:
        # i.e. no void states filled in by default
        existing_tape_nos = self._multi_tape.get_tape_nos()
        min_pos, max_pos = self._multi_tape.get_range()
        new_multi_tape = copy.deepcopy(self._multi_tape)
        scan_start = min_pos - self._max_radius
        scan_end = max_pos + self._max_radius + 1
        # record all (tape_no, position) -> tape_cell_state writes
        writes_map: dict[tuple[TapeNo, int], TapeCellState] = {}
        active_writes: list[WriteRecord] = []

        for position in range(scan_start, scan_end):
            written_tape_nos = set()

            # apply all matching rules at this position to get new tape states
            for matching_product in self._prod_to_state_map:
                if not self.product_satisfies(matching_product, position):
                    continue

                product_writes_map = self._prod_to_state_map[matching_product]

                for tape_no in product_writes_map:
                    tape_cell_state = product_writes_map[tape_no]
                    write_target: tuple[TapeNo, int] = (tape_no, position)
                    prev_write_state = writes_map.get(
                        write_target, tape_cell_state
                    )
                    if prev_write_state != tape_cell_state:
                        raise ValueError(
                            f"Conflicting writes to tape {tape_no} "
                            f"from {matching_product} at "
                            f"position {position}: {prev_write_state} vs "
                            f"{tape_cell_state}"
                        )

                    write_record = WriteRecord(
                        origin_product=matching_product,
                        write_target=(tape_no, position),
                        tape_cell_state=tape_cell_state
                    )
                    active_writes.append(write_record)
                    if log_active_writes:
                        write_record.log()

                    writes_map[write_target] = tape_cell_state
                    output_tape = new_multi_tape.get_or_make_tape(tape_no)
                    output_tape.write(position, tape_cell_state)
                    assert output_tape.read(position) == tape_cell_state
                    written_tape_nos.add(tape_no)

            # copy over unchanged tape cells for tapes that
            # were not written to at this position
            for tape_no in existing_tape_nos:
                if tape_no in written_tape_nos:
                    continue

                current_tape = self._multi_tape.get_or_make_tape(tape_no)
                new_tape = new_multi_tape.get_or_make_tape(tape_no)
                previous_tape_val: TapeCellState = current_tape.read(position)
                new_tape.write(position, previous_tape_val)

        return ProcessStepResult(
            prev_multi_tape=self._multi_tape,
            new_multi_tape=new_multi_tape,
            active_writes=active_writes
        )

    def step(self) -> ProcessStepResult:
        """
        Set the new state of the multi-tape after going forward
        a single step.
        :return:
        The previous multi-tape state before the step
        """
        process_result = self.process_step()
        self._multi_tape = process_result.new_multi_tape
        return process_result


class MultiTapeBuilder(object):
    def __init__(self, multi_tape_automata: MultiTapeAutomata):
        self._automata = multi_tape_automata
        # tape state -> (relative) position -> overlapping tape state
        # (tape_no, state) -> int -> (tape_no, state)
        # and by overlaps I mean (tape_no, state)
        self._initial_overlaps: defaultdict[
            MultiTapeOutput, defaultdict[int, set[MultiTapeOutput]]
        ] = defaultdict(lambda: defaultdict(set))

    def get_max_radius(self) -> int:
        return self._automata.get_max_radius()

    def declare_group_overlaps(
        self, overlap_states: set[MultiTapeOutput]
    ):
        """
        Declare that every state in the set of states passed in
        can overlap with any other state at any relative offset
        in the initial automata tape
        :param overlap_states:
        :return:
        """
        radius = self.get_max_radius()
        for state, other_state in zip(overlap_states, overlap_states):
            for offset in range(radius+1):
                self._initial_overlaps[state][offset].add(other_state)

    def compose(self):
        # TODO: infer existing overlaps from the automata as well
        pass
