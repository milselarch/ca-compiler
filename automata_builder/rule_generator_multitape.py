from __future__ import annotations

import copy
import dataclasses

from result import Result, Ok, Err

import utils

from collections import defaultdict
from typing import Final, Iterator, Sequence

from automata_builder.rule_generator import AutomataTransitionsGroup
from automata_builder.renderer import RenderFrame
from py_ca_compiler import (
    D, PyMultiTapeProduct, PyMultiTapeExpression,
    A, PyProduct
)


class TapeNo(int):
    def __eq__(self, other: int):
        return int(self) == int(other)

    def __hash__(self):
        return hash(int(self))


class TapeCellState(int):
    def __eq__(self, other: int):
        return int(self) == int(other)

    def __hash__(self):
        return hash(int(self))


BLANK_INT: Final[int] = -1
VOID_STATE: Final[TapeCellState] = TapeCellState(0b0)
HALT_STATE: Final[TapeCellState] = TapeCellState(0b1)


@dataclasses.dataclass
class MultiTapeState(object):
    """
    This represents the state of a cell in a specific tape of
    a multi-tape automaton
    Also this is technically the same as D, but without term position
    """
    tape_no: TapeNo
    tape_cell_state: TapeCellState

    def __hash__(self):
        return hash((self.tape_no, self.tape_cell_state))

    def __eq__(self, other: MultiTapeState) -> bool:
        return (
            self.tape_no == other.tape_no and
            self.tape_cell_state == other.tape_cell_state
        )

    def __gt__(self, other: MultiTapeState):
        if self.tape_no != other.tape_no:
            return self.tape_no > other.tape_no

        return self.tape_cell_state > other.tape_cell_state

    def to_tuple(self) -> tuple[int, int]:
        return int(self.tape_no), int(self.tape_cell_state)

    def to_str(self) -> str:
        tape_no = int(self.tape_no)
        tape_cell_state = int(self.tape_cell_state)
        str_state = f'T{tape_no}:{tape_cell_state}'
        return str_state

    def to_term(self, offset: int = 0) -> D:
        return D(
            position=offset,
            tape_no=self.tape_no, state=self.tape_cell_state
        )

    def has_tape_mutual_exclusion(self, other: MultiTapeState) -> bool:
        """
        Check if this state has tape mutual exclusion with the other state
        i.e. they are on the same tape but have different cell states
        :param other:
        :return:
        """
        return (
            self.tape_no == other.tape_no and
            self.tape_cell_state != other.tape_cell_state
        )

    @classmethod
    def from_term(cls, term: D):
        tape_no, tape_cell_state = term.get_state()
        return cls(
            tape_no=TapeNo(tape_no),
            tape_cell_state=TapeCellState(tape_cell_state)
        )


@dataclasses.dataclass(frozen=True)
class MultiTapeTransition(object):
    input_terms: tuple[D, ...]
    output_state: MultiTapeState
    annotation: str = ''


@dataclasses.dataclass
class MultiTapeTransitionsGroup(object):
    """
    contains a set of transitions for a multi-tape cellular automaton
    defined as a mapping from input states to output state
    map D[] -> (output tape_no, output state)
    """
    transitions: list[MultiTapeTransition] = dataclasses.field(
        default_factory=list
    )
    require_annotation: bool = False

    def add_transition(
        self, input_terms: tuple[D, ...],
        output_tape_no: int, output_cell_state: int,
        validate_void: bool = True,
        validate_halt: bool = True,
        annotation: str = ''
    ):
        """
        :param input_terms:
        :param output_tape_no:
        :param output_cell_state:
        :param validate_void:
        If true, check that the input terms do not all have void state
        :param validate_halt:
        If true, check that the halt state is not within input terms
        :param annotation:
        :return:
        """
        if self.require_annotation and not annotation:
            raise ValueError(f'Annotation expected')

        if validate_void:
            is_all_void = True

            for term in input_terms:
                if term.get_state() != VOID_STATE:
                    is_all_void = False

            if is_all_void:
                raise ValueError(
                    f"Input terms are all void, which is not "
                    f"allowed since it would make the simulation range "
                    f"infinite"
                )
        if validate_halt:
            for term in input_terms:
                if term.get_state() != HALT_STATE:
                    continue

                raise ValueError(
                    f"Input term {term} has halt state, which is not "
                    f"allowed since it has predefined behavior"
                )

        output_state = MultiTapeState(
            tape_no=TapeNo(output_tape_no),
            tape_cell_state=TapeCellState(output_cell_state)
        )
        transition = MultiTapeTransition(
            input_terms=input_terms,
            output_state=output_state,
            annotation=annotation
        )
        self.transitions.append(transition)

    def __or__(self):
        # TODO: go implement this
        raise NotImplementedError


class MultiTapeRuleGenerator(object):
    @staticmethod
    def terms_to_product(
        terms: tuple[D, ...], annotation: str
    ) -> PyMultiTapeProduct:
        return PyMultiTapeProduct(
            terms=terms, annotation=annotation
        )

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
        cls, transitions_group: MultiTapeTransitionsGroup,
        require_annotations: bool = False
    ) -> dict[MultiTapeState, PyMultiTapeExpression]:
        state_eq_terms_map: dict[
            MultiTapeState, list[PyMultiTapeProduct]
        ] = {}

        for transition in transitions_group.transitions:
            input_states = transition.input_terms
            output_state = transition.output_state
            annotation = transition.annotation

            if output_state not in state_eq_terms_map:
                state_eq_terms_map[output_state] = []

            product = cls.terms_to_product(input_states, annotation)
            state_eq_terms_map[output_state].append(product)
            if require_annotations:
                assert product.get_annotation()

        state_eq_map: dict[MultiTapeState, PyMultiTapeExpression] = {
            next_state: cls.aggregate_bit_or(state_eq_terms_map[next_state])
            for next_state in state_eq_terms_map
        }
        for next_state in state_eq_map:
            expr = state_eq_map[next_state]
            flat_products = expr.get_flat_products()

            if not flat_products:
                raise ValueError(
                    f"Output state {next_state} has no products in "
                    f"its expression {expr}"
                )

            for product in flat_products:
                if require_annotations:
                    assert product.get_annotation()

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
        # automata cell states from position 0 and higher
        # note that position increases for cells as we go rightwards in data
        self.data: list[TapeCellState] = []
        # automata cell states from position -1 and lower
        # note that position decreases for cells as we go rightwards in data
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
        # make a shallow copy to avoid mutable reference
        data_region = self.rev_data[::-1] + self.data
        minimal_data_region: list[TapeCellState] = []
        data_region_started: bool = False

        for tape_cell_state in data_region:
            if tape_cell_state != VOID_STATE:
                data_region_started = True

            if not data_region_started:
                continue

            minimal_data_region.append(tape_cell_state)

        # remove trailing void state cells
        # this can happen if all data cells are from the rev_data region
        while minimal_data_region and minimal_data_region[-1] == VOID_STATE:
            minimal_data_region.pop()

        return minimal_data_region

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
        self._freeze_tapes: bool = False

        if tapes is not None:
            self._tapes: dict[TapeNo, BidirectionalTape] = tapes
        else:
            self._tapes: dict[TapeNo, BidirectionalTape] = {}

    def get_or_make_tape(self, tape_no: TapeNo) -> BidirectionalTape:
        if tape_no not in self._tapes:
            if self._freeze_tapes:
                raise ValueError(
                    "Cannot write to new tape when tapes are frozen"
                )

            self._tapes[tape_no] = BidirectionalTape()

        return self._tapes[tape_no]

    def __getitem__(self, tape_no: TapeNo) -> BidirectionalTape:
        return self.get_or_make_tape(tape_no=tape_no)

    def init_tapes(self, tape_nos: list[TapeNo], freeze: bool = True):
        for tape_no in tape_nos:
            self.get_or_make_tape(tape_no)

        if freeze:
            self._freeze_tapes = True

    def get_tape_nos(self) -> list[TapeNo]:
        return sorted(list(self._tapes.keys()))

    def write_region(
        self, position: int, end_position: int,
        data: list[MultiTapeState]
    ):
        """
        Populate the automata cells from :position: to :end_position:
        (inclusive) using :data: as a full pattern
        :param position:
        :param end_position:
        :param data:
        :return:
        """
        for new_position in range(position, end_position+1):
            offset = new_position - position
            value = data[offset % len(data)]
            self.write(new_position, value)

    def write(self, position: int, value: MultiTapeState):
        tape = self.get_or_make_tape(value.tape_no)
        tape.write(position, value.tape_cell_state)

    def get_all_states(self) -> set[TapeCellState]:
        all_states = set()
        for tape in self._tapes.values():
            all_states |= tape.get_all_states()

        return all_states

    def prune(self):
        tape_nos = sorted(set(self._tapes.keys()))

        for tape_no in tape_nos:
            tape = self._tapes[tape_no]
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

        tape_nos = sorted(set(self._tapes.keys()) | {TapeNo(0)})
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
            tape = self._tapes[tape_no]
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

    def get_range(self):
        """
        Get the range for which tape cell data is currently encoded
        in the tape
        :return:
        """
        min_pos, max_pos = 0, 0

        for tape in self._tapes.values():
            tape_min, tape_max = tape.get_range()
            min_pos = min(min_pos, tape_min)
            max_pos = max(max_pos, tape_max)

        return min_pos, max_pos


@dataclasses.dataclass
class WriteRecord(object):
    origin_product: PyMultiTapeProduct
    write_target: tuple[TapeNo, int]  # (tape_no, position)
    tape_cell_state: TapeCellState
    # for debugging purposes (to trace originating product)
    annotation: str = ''

    def log(self):
        print(
            f'{self.origin_product} | {self.write_target} '
            f'-> {self.tape_cell_state} ({self.annotation})'
        )


@dataclasses.dataclass
class ProcessStepResult(object):
    prev_multi_tape: BiDirectionalMultiTape
    new_multi_tape: BiDirectionalMultiTape
    active_writes: list[WriteRecord]


class MultiTapeAutomata(object):
    def __init__(
        self, state_eq_map: dict[MultiTapeState, PyMultiTapeExpression]
    ):
        self._multi_tape: BiDirectionalMultiTape = BiDirectionalMultiTape()
        self._prod_to_state_map = self.reverse_state_eq_map(
            state_eq_map, require_annotations=True
        )

        leftmost_extent, rightmost_extent = self.get_rule_range()
        self._leftmost_extent: int = leftmost_extent
        self._rightmost_extent: int = rightmost_extent
        self._state_eq_map = state_eq_map

    def __getitem__(self, tape_no: TapeNo) -> BidirectionalTape:
        return self._multi_tape[tape_no]

    def get_tape_nos(self) -> list[TapeNo]:
        return self._multi_tape.get_tape_nos()

    def get_prod_to_state_map(self) -> ProductWritesMap:
        return copy.deepcopy(self._prod_to_state_map)

    def get_state_eq_map(self) -> dict[
        MultiTapeState, PyMultiTapeExpression
    ]:
        return copy.deepcopy(self._state_eq_map)

    @property
    def leftmost_extent(self) -> int:
        return self._leftmost_extent

    @property
    def rightmost_extent(self) -> int:
        return self._rightmost_extent

    def get_rule_range(self) -> tuple[int, int]:
        leftmost_extent, rightmost_extent = 0, 0

        for product in self._prod_to_state_map:
            terms = product.get_flat_terms()

            for term in terms:
                offset = term.get_position()
                leftmost_extent = min(leftmost_extent, offset)
                rightmost_extent = max(rightmost_extent, offset)

        assert leftmost_extent <= 0
        assert rightmost_extent >= 0
        return leftmost_extent, rightmost_extent

    def init_tapes(self, tape_nos: list[TapeNo]):
        self._multi_tape.init_tapes(tape_nos)

    def write_region(
        self, position: int, end_position: int,
        data: list[MultiTapeState]
    ):
        """
        Populate the automata cells from :position: to :end_position:
        (inclusive) using :data: as a full pattern
        :param position:
        :param end_position:
        :param data:
        :return:
        """
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
        cls, state_eq_map: dict[MultiTapeState, PyMultiTapeExpression],
        require_annotations: bool = False
    ) -> ProductWritesMap:
        """
        given a mapping from output tape states to expressions
        over input tape states, create a mapping of tape state products to the
        tape_no and tape cell state they write to

        map product -> tape_no -> output tape cell state

        The reason we don't make it return product -> MultiTapeOutput
        is because we also want to check for write collisions
        (so given the same tape we should expect the product to only
        write a unique cell state, if at all)

        :param require_annotations:
        :param state_eq_map:
        :return:
        """
        # TODO: extract out annotations as well for debugging
        prod_to_state_map = ProductWritesMap()

        for multi_tape_output, expr in state_eq_map.items():
            products = expr.get_flat_products()

            for product in products:
                product_terms = product.get_flat_terms()
                """
                Whether a product transitions a contiguous region
                of void states into a non-void state
                This can't be allowed because it would make the 
                simulation range infinite.
                """
                product_is_void: bool = True

                for term in product_terms:
                    if term.get_state() != VOID_STATE:
                        product_is_void = False
                        break

                if product_is_void:
                    raise ValueError(
                        f"Product {product} transitions void states to "
                        f"non-void state {multi_tape_output}, which is not "
                        f"allowed since it would make the simulation range "
                        f"infinite"
                    )

                annotation = product.get_annotation()
                if require_annotations and not annotation:
                    raise ValueError(f'EMPTY ANNOTATION {product=}')

                prod_to_state_map.insert(
                    product=product, tape_output=multi_tape_output
                )

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
        scan_start = min_pos + self._leftmost_extent
        scan_end = max_pos + self._rightmost_extent + 1

        # record all (tape_no, position) -> (tape_cell_state) writes
        writes_map: dict[tuple[TapeNo, int], TapeCellState] = {}
        annotations_map: dict[tuple[TapeNo, int], set[str]] = defaultdict(set)
        active_writes: list[WriteRecord] = []

        for position in range(scan_start, scan_end):
            written_tape_nos = set()

            # apply all matching rules at this position to get new tape states
            for matching_product in self._prod_to_state_map:
                if not self.product_satisfies(matching_product, position):
                    continue

                product_writes_map = self._prod_to_state_map[matching_product]
                annotation = matching_product.get_annotation()

                for tape_no in product_writes_map:
                    tape_cell_state = product_writes_map[tape_no]
                    write_target: tuple[TapeNo, int] = (tape_no, position)
                    # previously recorded write to this tape cell, if any
                    prev_write = writes_map.get(write_target, tape_cell_state)
                    prev_annotations = annotations_map[write_target]

                    if prev_write != tape_cell_state:
                        raise ValueError(
                            f"Conflicting writes to tape {tape_no} "
                            f"from {matching_product=} {annotation=} at "
                            f"position {position}: {prev_write} vs "
                            f"{tape_cell_state} ({prev_annotations=})"
                        )

                    write_record = WriteRecord(
                        origin_product=matching_product,
                        write_target=(tape_no, position),
                        tape_cell_state=tape_cell_state,
                        annotation=annotation
                    )
                    active_writes.append(write_record)
                    if log_active_writes:
                        write_record.log()

                    writes_map[write_target] = tape_cell_state
                    annotations_map[write_target].add(annotation)
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

    def step(self, verbose: bool = False) -> ProcessStepResult:
        """
        Set the new state of the multi-tape after going forward
        a single step.
        :return:
        The previous multi-tape state before the step
        """
        process_result = self.process_step(log_active_writes=verbose)
        self._multi_tape = process_result.new_multi_tape
        return process_result


class TapeOverlaps(object):
    """
    we say that a tape state A can overlap with tape state B at offset k if
    in the history of the automata it is possible that:
    1. A is present at some position p on the tape and
    2. B to be present at position p + k.

    This structure stores all possible state overlaps
    """
    def __init__(self):
        """
        A: MultiTapeState -> B: int -> C: set[MultiTapeState]

        For a given source state A,
        the mapping gives all target states B that can
        be present at offset k from A, for all possible offsets k.
        """
        self._overlaps: defaultdict[
            MultiTapeState, defaultdict[int, set[MultiTapeState]]
        ] = defaultdict(lambda: defaultdict(set))

    def visualize_for_states(
        self, source_states: set[MultiTapeState]
    ) -> list[str]:
        max_prefix_length = 0

        for source_state in source_states:
            tree_prefix = source_state.to_str()
            max_prefix_length = max(max_prefix_length, len(tree_prefix))

        lines = []
        source_states_seq = sorted(list(source_states))

        for source_state in source_states_seq:
            source_state_lines = self.visualize_for_state(
                source_state, prefix_length=max_prefix_length
            )
            lines.extend(source_state_lines)
            lines.append('')

        return lines

    def print_for_states(
        self, source_states: set[MultiTapeState] | None = None
    ) -> None:
        if source_states is None:
            _source_states = set(self._overlaps.keys())
        else:
            _source_states = set(source_states)

        lines = self.visualize_for_states(_source_states)
        print("\n".join(lines))

    @staticmethod
    def list_for_offset(
        target_states_set: set[MultiTapeState]
    ) -> str:
        # print(f"{target_states_set=}")
        sorted_target_states = sorted(list(target_states_set))
        tape_states_map: defaultdict[
            TapeNo, list[TapeCellState]
        ] = defaultdict(list)

        for sorted_target_state in sorted_target_states:
            tape_no = sorted_target_state.tape_no
            tape_cell_state = sorted_target_state.tape_cell_state
            tape_states_map[tape_no].append(tape_cell_state)

        tape_nos = sorted(list(tape_states_map.keys()))
        tape_chunks = []

        for tape_no in tape_nos:
            tape_cell_states = tape_states_map[tape_no]
            tape_cell_states_str = '|'.join([str(x) for x in tape_cell_states])
            tape_chunk = f'T{tape_no}:' + tape_cell_states_str
            tape_chunks.append(tape_chunk)

        target_states_str = ' '.join(tape_chunks)
        # print(f'{target_states_str=}')
        # print('')
        return target_states_str

    def visualize_for_state(
        self, source_state: MultiTapeState, prefix_length: int = 0
    ) -> list[str]:
        # print(source_state)
        tree_prefix = source_state.to_str()
        if prefix_length > len(tree_prefix):
            tree_prefix += ' ' * (prefix_length - len(tree_prefix))

        overlap_map = self._overlaps[source_state]
        overlap_offsets = set(overlap_map.keys())
        min_offset, max_offset = 0, 0

        if overlap_offsets:
            min_offset = min(min(overlap_offsets), 0)
            max_offset = max(max(overlap_offsets), 0)

        offset_pad_length = max(len(str(min_offset)), len(str(max_offset)))
        mid_pre_line = tree_prefix + ' —'
        lines = []

        for offset in range(min_offset, max_offset + 1):
            if offset == 0:
                pre_line = mid_pre_line
            else:
                pre_line = " " * len(mid_pre_line)

            target_states_set: set[MultiTapeState] = overlap_map[offset]
            if (offset != 0) and not target_states_set:
                # we want to not skip if offset=0 because
                # the offset=0 is also the one showing the tape state
                # lines.append(pre_line + f'|—')
                continue

            target_states_str = self.list_for_offset(target_states_set)

            if offset < 0:
                offset_str = str(offset)
            else:
                offset_str = f'+{offset}'

            if offset == min_offset:
                branch_char = '┌'
            elif offset == max_offset:
                branch_char = '└'
            else:
                branch_char = '|'

            offset_pad_chars = (offset_pad_length - len(offset_str)) * ' '
            offset_padded_str = offset_pad_chars + offset_str
            lines.append(
                pre_line +
                f'{branch_char}— {offset_padded_str}'
                f' —> {target_states_str}'
            )

        return lines

    def __repr__(self):
        return f'{self.__class__.__name__}(overlaps={self._overlaps})'

    def get_all_states(self) -> set[MultiTapeState]:
        return set(self._overlaps.keys())

    def get_overlaps_for_offset(
        self, source_state: MultiTapeState, offset: int
    ) -> set[MultiTapeState]:
        return copy.copy(self._overlaps[source_state][offset])

    def get_overlaps(
        self, source_state: MultiTapeState
    ) -> defaultdict[int, set[MultiTapeState]]:
        return copy.deepcopy(self._overlaps[source_state])

    def propagate_overlap(
        self, source_state: MultiTapeState, target_state: MultiTapeState,
        offset: int, min_offset: int, max_offset: int
    ) -> bool:
        source_updated = self.insert_overlaps_for(
            source_state=source_state, target_state=target_state,
            offset=offset, min_offset=min_offset, max_offset=max_offset
        )
        self.validate_mutual_overlaps_for(source_state=source_state)
        self.validate_symmetric_overlaps_for(source_state=source_state)

        target_updated = self.insert_overlaps_for(
            source_state=target_state, target_state=source_state,
            offset=-offset, min_offset=min_offset, max_offset=max_offset
        )
        self.validate_mutual_overlaps_for(source_state=source_state)
        self.validate_symmetric_overlaps_for(source_state=source_state)
        return source_updated or target_updated
        # return source_updated

    def validate_mutual_overlaps_for(self, source_state: MultiTapeState) -> None:
        """
        Check that there are no overlaps with tape mutual exclusion
        i.e. if two states are on the same tape and same position
        but have different cell states, then they can't overlap
        :param source_state:
        :return:
        """
        source_overlaps_map = self._overlaps[source_state]
        direct_overlap_states = source_overlaps_map[0]  # direct overlap

        for overlap_state in direct_overlap_states:
            has_tape_mutual_exclusion = (
                source_state.tape_no == overlap_state.tape_no and
                source_state.tape_cell_state != overlap_state.tape_cell_state
            )
            if has_tape_mutual_exclusion:
                raise ValueError(
                    f"Invalid overlap between {source_state} and "
                    f"{overlap_state} at offset 0 due to tape mutual "
                    f"exclusion"
                )

    def validate_symmetric_overlaps_for(
        self, source_state: MultiTapeState
    ) -> None:
        """
        Every overlap should be symmetric:
        so if A overlaps with B at offset k,
        then B should also overlap with A at offset -k
        """
        source_overlaps_map = self._overlaps[source_state]

        for offset in source_overlaps_map:
            target_states = source_overlaps_map[offset]

            for target_state in target_states:
                target_overlaps_map = self._overlaps[target_state]
                symmetric_overlap_states = target_overlaps_map[-offset]

                if source_state not in symmetric_overlap_states:
                    raise ValueError(
                        f"Invalid asymmetric overlap: {source_state} overlaps "
                        f"with {target_state} at offset {offset}, but "
                        f"{target_state} does not overlap with "
                        f"{source_state} at offset {-offset}"
                    )

    def insert_overlaps_for(
        self, source_state: MultiTapeState, target_state: MultiTapeState,
        offset: int, min_offset: int, max_offset: int
    ) -> bool:
        """
        :param source_state:
        :param target_state:
        :param offset:
        offset of target_state FROM source_state
        :param min_offset:
        :param max_offset:
        :return:
        """
        source_cell_state = source_state.tape_cell_state
        # target_cell_state = target_state.tape_cell_state
        source_overlaps_map = self._overlaps[source_state]
        target_overlaps_map = self._overlaps[target_state]
        target_overlap_offsets = list(target_overlaps_map.keys())
        overlaps_inserted = False

        for target_state_offset in target_overlap_offsets:
            """
            If state A overlaps with state B at offset k,
            and state B overlaps with state C at offset m,
            then state A also overlaps with state C at offset k + m,
            
            So what we are doing here is to add all relevant states C 
            to the overlaps of state A at relevant (shifted) offsets 
            accordingly
            """
            source_state_offset = offset + target_state_offset
            # target_state_offset_overlap_inserted = False
            if source_state_offset < min_offset:
                continue
            if source_state_offset > max_offset:
                continue

            target_overlap_states = target_overlaps_map[target_state_offset]
            source_overlap_states = source_overlaps_map[source_state_offset]
            prev_target_overlap_states = copy.copy(target_overlap_states)
            # prev_source_overlap_states = copy.copy(source_overlap_states)

            for target_overlap_state in prev_target_overlap_states:
                # overlaps_snapshot = copy.deepcopy(self)\
                if target_overlap_state in source_overlap_states:
                    # print("SKIP_TARGET", source_state_offset, target_overlap_state)
                    continue

                target_overlap_cell_state = (
                    target_overlap_state.tape_cell_state
                )
                has_tape_mutual_exclusion = (
                    source_state_offset == 0 and
                    target_overlap_cell_state != source_cell_state and
                    target_overlap_state.tape_no == source_state.tape_no
                )
                if has_tape_mutual_exclusion:
                    # if the states have same-tape mutual exclusion,
                    # then they can't overlap
                    continue

                # new_source_overlap_states.add(target_overlap_state)
                # source_overlap_states.add(target_overlap_state)
                overlaps_inserted |= self.insert_direct_overlap(
                    source_state=source_state,
                    target_state=target_overlap_state,
                    offset=source_state_offset
                )

                assert target_overlap_state in source_overlaps_map[source_state_offset]
                # target_state_offset_overlap_inserted = True

                """
                The target_overlap_state also would now overlap with 
                source_state at an offset of -offset
                """
                overlaps_inserted |= self.insert_direct_overlap(
                    source_state=target_overlap_state,
                    target_state=source_state,
                    offset=-offset
                )

                # TODO: insert source_state = source_overlap_state at target_offset?
                # TODO: loop source_overlap_state also
                """
                overlaps_inserted |= self.insert_direct_overlap(
                    source_state=source_state,
                    target_state=target_overlap_state,
                    offset=source_state_offset
                )
                """
                self.validate_mutual_overlaps_for(source_state)
                self.validate_symmetric_overlaps_for(source_state)
                # target_overlaps_map[target_state_offset] = new_target_overlap_states
                # source_overlaps_map[source_state_offset] = new_source_overlap_states

        has_tape_mutual_exclusion = (
            source_state.tape_no == target_state.tape_no and
            source_state.tape_cell_state != target_state.tape_cell_state and
            offset == 0
        )

        if not has_tape_mutual_exclusion:
            """
            if two multi tape states are on the same tape and 
            have different tape cell states then they must 
            necessarily never be able to overlap with one another directly 
            (i.e. with an overlap offset=0)
            """
            overlaps_inserted |= self.insert_direct_overlap(
                source_state=source_state, target_state=target_state,
                offset=offset
            )
            # self._overlaps[source_state][offset].add(target_state)
            # self._overlaps[target_state][-offset].add(source_state)
            self.validate_mutual_overlaps_for(source_state)
            # return False

        self.validate_mutual_overlaps_for(source_state)
        self.validate_symmetric_overlaps_for(source_state)
        return overlaps_inserted

    def insert_direct_overlap(
        self, source_state: MultiTapeState, target_state: MultiTapeState,
        offset: int
    ) -> bool:
        """
        If state A overlaps with state B at offset k,
        then state B also overlaps with state A at offset -k

        :param source_state:
        :param target_state:
        :param offset:
        offset of term with state_b FROM state_a
        one has to conscientious about direction
        :return:
        """
        has_tape_mutual_exclusion = (
            source_state.tape_no == target_state.tape_no and
            source_state.tape_cell_state != target_state.tape_cell_state and
            offset == 0
        )
        if has_tape_mutual_exclusion:
            return False

        source_overlaps = self._overlaps[source_state][offset]
        target_overlaps = self._overlaps[target_state][-offset]

        if target_state in source_overlaps:
            # overlap already exists
            return False

        source_overlaps.add(target_state)
        # assert source_state not in target_overlaps
        target_overlaps.add(source_state)
        self.validate_mutual_overlaps_for(source_state=source_state)
        self.validate_symmetric_overlaps_for(source_state=source_state)
        return True

    def can_overlap_exist(
        self, source_state: MultiTapeState,
        target_state: MultiTapeState, offset: int
    ) -> bool:
        """
        :param source_state:
        :param target_state:
        :param offset:
        offset of term with target_state FROM term with source_state
        :return:
        """
        return target_state in self._overlaps[source_state][offset]

    def create_whitelist_for_offset(
        self, offset: int = 0
    ) -> MultiTapeStatesMap:
        """
        For a given offset, create a whitelist of what tape states can
        exist at each tape for that offset

        If we leave offset at zero, this conveniently returns all the possible
        input tape states that can exist grouped by tape no.

        :param offset:
        :return:
        """
        whitelist = MultiTapeStatesMap()

        for source_state in self._overlaps:
            source_tape_no = source_state.tape_no
            # source_tape_cell_state = source_state.tape_cell_state
            state_overlaps = self._overlaps[source_state]
            offset_state_overlaps = state_overlaps[offset]

            for offset_state_overlap in offset_state_overlaps:
                whitelist.insert(
                    tape_no=offset_state_overlap.tape_no,
                    state=offset_state_overlap
                )

        return whitelist


class MultiTapeStatesMap(object):
    """
    Intended to represent what tape states can occur at each
    tape from any possible source state at a given offset
    """
    def __init__(
        self, whitelist: defaultdict[TapeNo, set[TapeCellState]] | None = None
    ):
        if whitelist is None:
            _whitelist: defaultdict[
                TapeNo, set[TapeCellState]
            ] = defaultdict(set)
        else:
            _whitelist = whitelist

        self._whitelist: defaultdict[TapeNo, set[TapeCellState]] = _whitelist

    def __iter__(self) -> Iterator[TapeNo]:
        return iter(self._whitelist)

    def get_tape_nos(self) -> list[TapeNo]:
        return sorted(list(self._whitelist.keys()))

    def insert(self, tape_no: TapeNo, state: TapeCellState | MultiTapeState):
        if isinstance(state, MultiTapeState):
            assert tape_no == state.tape_no
            tape_cell_state: TapeCellState = state.tape_cell_state
        else:
            assert isinstance(state, TapeCellState)
            tape_cell_state: TapeCellState = state

        self._whitelist[tape_no].add(tape_cell_state)

    def __getitem__(self, item: TapeNo) -> set[TapeCellState]:
        return copy.copy(self._whitelist[item])

    def __setitem__(self, key: TapeNo, value: set[TapeCellState]):
        self._whitelist[key] = copy.copy(value)


@dataclasses.dataclass
class ProductTrie(object):
    """
    A trie of product terms nested from smallest to largest term offset
    """
    # whether the path of all terms till here constitute ann inserted product
    is_end_product: bool = False
    # map offset from current term to next trie
    next_terms: defaultdict[D, ProductTrie] = dataclasses.field(
        default_factory=lambda: defaultdict(ProductTrie)
    )

    def next(self, term: D) -> ProductTrie:
        return self.next_terms[term]

    def _insert_term_path(self, term_path: list[D]):
        if not term_path:
            return

        current_term, next_terms = term_path[0], term_path[1:]
        self.next_terms[current_term]._insert_term_path(next_terms)

    def insert_term_path(self, term_path: list[D]):
        term_path = sorted(term_path, key=lambda term: term.get_position())
        self._insert_term_path(term_path)

    def insert_product(self, product: PyMultiTapeProduct):
        terms = product.get_flat_terms()
        self.insert_term_path(terms)

    def _has_term_path(self, term_path: list[D]) -> bool:
        if not term_path:
            return self.is_end_product

        current_term, next_terms = term_path[0], term_path[1:]
        if current_term not in self.next_terms:
            return False

        return self.next_terms[current_term]._has_term_path(next_terms)

    def has_term_path(self, term_path: list[D]) -> bool:
        term_path = sorted(term_path, key=lambda term: term.get_position())
        return self._has_term_path(term_path)

    def has_product(self, product: PyMultiTapeProduct) -> bool:
        terms = product.get_flat_terms()
        return self.has_term_path(terms)


@dataclasses.dataclass
class ProductWritesMap(object):
    """
    map product -> tape_no -> output tape cell state
    """
    prod_to_state_map: defaultdict[
        PyMultiTapeProduct, dict[TapeNo, TapeCellState]
    ] = dataclasses.field(
        default_factory=lambda: defaultdict(lambda: dict())
    )

    def __iter__(self) -> Iterator[PyMultiTapeProduct]:
        return iter(self.prod_to_state_map.keys())

    def items(self):
        return self.prod_to_state_map.items()

    def get_state_writes_for(
        self, product: PyMultiTapeProduct
    ) -> list[MultiTapeState]:
        writes_map = self.prod_to_state_map[product]
        tape_state_writes: list[MultiTapeState] = []

        for tape_no in writes_map:
            tape_cell_state = writes_map[tape_no]
            tape_state = MultiTapeState(
                tape_no=tape_no, tape_cell_state=tape_cell_state
            )
            tape_state_writes.append(tape_state)

        return tape_state_writes

    def build_state_to_products_map(
        self, verbose: bool = False
    ) -> defaultdict[MultiTapeState, set[PyMultiTapeProduct]]:
        """
        maps states -> products that produce them in their output terms
        :param verbose:
        :return:
        """
        state_to_products_map: defaultdict[
            MultiTapeState, set[PyMultiTapeProduct]
        ] = defaultdict(set)

        for product in self.prod_to_state_map:
            writes_map = self.prod_to_state_map[product]

            for tape_no in writes_map:
                tape_cell_state = writes_map[tape_no]
                tape_state = MultiTapeState(
                    tape_no=tape_no, tape_cell_state=tape_cell_state
                )
                state_to_products_map[tape_state].add(product)

        if verbose:
            states = sorted(state_to_products_map.keys())

            for state in states:
                print(f'Products that produce {state=}')

                production_products = state_to_products_map[state]
                for product in production_products:
                    print(f'- {product}')

        return state_to_products_map

    def build_input_state_to_prod_map(
        self, verbose: bool = False
    ) -> defaultdict[
        MultiTapeState, set[PyMultiTapeProduct]
    ]:
        """
        maps state -> products that contain it in their input terms
        :param verbose:
        :return:
        """
        # map state -> products that contain it in their input terms
        input_state_to_prod_map: defaultdict[
            MultiTapeState, set[PyMultiTapeProduct]
        ] = defaultdict(set)

        for product in self.prod_to_state_map:
            input_terms = product.get_flat_terms()

            for input_term in input_terms:
                input_state = MultiTapeState.from_term(input_term)
                input_state_to_prod_map[input_state].add(product)

        if verbose:
            for input_state in input_state_to_prod_map:
                print(f'Input products for {input_state}')

                products = input_state_to_prod_map[input_state]
                for product in products:
                    print(f'- {product}')

            print('')

        return input_state_to_prod_map

    def get_states_set(self) -> set[MultiTapeState]:
        states_set: set[MultiTapeState] = set()

        for tape_product in self.prod_to_state_map:
            product_terms = tape_product.get_flat_terms()

            for term in product_terms:
                tape_no, tape_cell_state = term.get_state()
                states_set.add(MultiTapeState(
                    tape_no=TapeNo(tape_no),
                    tape_cell_state=TapeCellState(tape_cell_state)
                ))

        return states_set

    def keys(self):
        return self.prod_to_state_map.keys()

    def values(self):
        return self.prod_to_state_map.values()

    def __getitem__(self, item: PyMultiTapeProduct):
        return copy.copy(self.prod_to_state_map[item])

    @classmethod
    def get_zero_terms_from_path(cls, product_path: list[D]) -> list[D]:
        zero_terms = []

        for term in product_path:
            if term.get_position() == 0:
                zero_terms.append(term)

        return zero_terms

    @classmethod
    def get_zero_terms_from_product(
        cls, product: PyMultiTapeProduct
    ) -> list[D]:
        return cls.get_zero_terms_from_path(
            product_path=product.get_flat_terms()
        )

    def insert_neutral_product(self, product: PyMultiTapeProduct):
        """
        Insert a product whose outputs rewrite the input terms
        that have an offset = 0 to have the same state
        :param product:
        :return:
        """
        zero_terms = self.get_zero_terms_from_product(product)

        for zero_term in zero_terms:
            zero_state = MultiTapeState.from_term(zero_term)
            self.insert(product=product, tape_output=zero_state)

    def merge(self, other_writes_map: ProductWritesMap):
        for other_product in other_writes_map:
            other_product_writes = other_writes_map[other_product]

            for tape_no in other_product_writes:
                tape_cell_state = other_product_writes[tape_no]
                tape_output = MultiTapeState(tape_no, tape_cell_state)
                self.insert(product=other_product, tape_output=tape_output)

    def insert(
        self, product: PyMultiTapeProduct, tape_output: MultiTapeState
    ):
        write_tape_no = tape_output.tape_no
        write_tape_cell_state = tape_output.tape_cell_state
        self._insert(
            product=product, write_tape_no=write_tape_no,
            write_tape_cell_state=write_tape_cell_state
        )

    def _insert(
        self, product: PyMultiTapeProduct, write_tape_no: TapeNo,
        write_tape_cell_state: TapeCellState
    ):
        writes_map = self.prod_to_state_map[product]
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


@dataclasses.dataclass
class MultiTapeStateRemap(object):
    _global_tape_state_remap: dict[MultiTapeState, TapeCellState]
    _rev_global_tape_state_remap: dict[TapeCellState, MultiTapeState]

    @classmethod
    def create_from(
        cls, global_tape_state_remap: dict[MultiTapeState, TapeCellState]
    ) -> MultiTapeStateRemap:
        rev_global_tape_state_remap: dict[TapeCellState, MultiTapeState] = {}
        _global_tape_state_remap = copy.deepcopy(global_tape_state_remap)

        for multi_tape_output in global_tape_state_remap:
            remapped_state = global_tape_state_remap[multi_tape_output]
            rev_global_tape_state_remap[remapped_state] = multi_tape_output

        return MultiTapeStateRemap(
            _global_tape_state_remap=_global_tape_state_remap,
            _rev_global_tape_state_remap=rev_global_tape_state_remap
        )

    def __len__(self) -> int:
        return len(self._global_tape_state_remap)

    def __getitem__(self, item: MultiTapeState) -> TapeCellState:
        return self.to_composed_state(item)

    def to_composed_state(
        self, multi_tape_state: MultiTapeState
    ) -> TapeCellState:
        assert isinstance(multi_tape_state, MultiTapeState), multi_tape_state
        return self._global_tape_state_remap[multi_tape_state]

    def from_composed_state(
        self, composed_state: TapeCellState
    ) -> MultiTapeState:
        return self._rev_global_tape_state_remap[composed_state]


@dataclasses.dataclass
class MultiTapeStatePathRemap(object):
    """
    Contains a remap of
    possible tape cell state combinations
    along the exact same position across all tapes
    to global tape cell state values
    """
    remap_counter_start: TapeCellState

    # TODO: technically only 1 state path can exist here
    void_state_paths: set[tuple[MultiTapeState, ...]] = dataclasses.field(
        default_factory=set
    )
    halt_state_paths: set[tuple[MultiTapeState, ...]] = dataclasses.field(
        default_factory=set
    )
    tape_state_path_remap: dict[
        tuple[MultiTapeState, ...], TapeCellState
    ] = dataclasses.field(
        default_factory=dict
    )
    rev_state_path_remap: dict[
        TapeCellState, tuple[MultiTapeState, ...]
    ] = dataclasses.field(
        default_factory=dict
    )

    def __len__(self):
        return self.num_normal_remaps

    def rev_lookup(
        self, tape_cell_state: TapeCellState
    ) -> Result[tuple[MultiTapeState, ...], TapeCellState]:
        if tape_cell_state == HALT_STATE:
            return Err(tape_cell_state)
        elif tape_cell_state == VOID_STATE:
            return Ok(list(self.void_state_paths)[0])

        return Ok(self.rev_state_path_remap[tape_cell_state])

    def get_all_state_paths(self) -> set[tuple[MultiTapeState, ...]]:
        return (
            set(self.tape_state_path_remap.keys()) |
            self.void_state_paths | self.halt_state_paths
        )

    @property
    def remap_counter_end(self) -> TapeCellState:
        return TapeCellState(
            self.remap_counter_start + self.num_normal_remaps
        )

    @property
    def next_free_counter(self) -> TapeCellState:
        return TapeCellState(
            self.remap_counter_end + TapeCellState(1)
        )

    def remap(self, state_path: tuple[MultiTapeState, ...]) -> TapeCellState:
        if self.is_halt_path(state_path):
            return HALT_STATE
        if self.is_void_path(state_path):
            return VOID_STATE

        return self.tape_state_path_remap[state_path]

    def get_all_remap_states(self) -> set[TapeCellState]:
        """
        Get all remapped tape cell state values that
        have been allocated in this remap
        :return:
        """
        remap_states = set(self.tape_state_path_remap.values())

        if self.void_state_paths:
            remap_states.add(VOID_STATE)
        if self.halt_state_paths:
            remap_states.add(HALT_STATE)

        return remap_states

    def __getitem__(self, item: tuple[MultiTapeState, ...]) -> TapeCellState:
        return self.remap(item)

    @property
    def num_normal_remaps(self) -> int:
        return len(self.tape_state_path_remap)

    @property
    def num_void_remaps(self) -> int:
        return len(self.void_state_paths)

    @property
    def num_halt_remaps(self) -> int:
        return len(self.halt_state_paths)

    @staticmethod
    def is_void_path(path: tuple[MultiTapeState, ...]):
        """
        The remapped state is VOID if all the multi-tape cell states
        passed in are void also
        :param path:
        :return:
        """
        for multi_tape_state in path:
            if multi_tape_state.tape_cell_state != VOID_STATE:
                return False

        return True

    @staticmethod
    def is_halt_path(path: tuple[MultiTapeState, ...]):
        """
        The remapped state is HALT if any of the multi-tape cell states
        passed in are HALT also
        :param path:
        :return:
        """
        for multi_tape_state in path:
            if multi_tape_state.tape_cell_state == HALT_STATE:
                return True

        return False

    def insert_overlap_path(
        self, path: tuple[MultiTapeState, ...],
    ) -> Result[TapeCellState, None]:
        if path in self.tape_state_path_remap:
            return Err(None)

        if self.is_halt_path(path):
            self.halt_state_paths.add(path)
            return Ok(HALT_STATE)
        elif self.is_void_path(path):
            self.void_state_paths.add(path)
            return Ok(VOID_STATE)
        else:
            new_tape_state = self.get_next_tape_state()
            self.tape_state_path_remap[path] = new_tape_state
            self.rev_state_path_remap[new_tape_state] = path
            return Ok(new_tape_state)

    def get_next_tape_state(self) -> TapeCellState:
        """
        :return:
        The next available tape cell state that isn't being
        used in a mapping from an existing path
        """
        return TapeCellState(
            self.remap_counter_start + self.num_normal_remaps
        )

    def merge(self, other_remap: MultiTapeStatePathRemap):
        """
        We could probably just merge the dict / sets
        directly with a little more work but whatever
        :param other_remap:
        :return:
        """
        for path in other_remap.tape_state_path_remap:
            self.insert_overlap_path(path)

        for void_path in other_remap.void_state_paths:
            self.insert_overlap_path(void_path)

        for halt_path in other_remap.halt_state_paths:
            self.insert_overlap_path(halt_path)

    @classmethod
    def from_path(
        cls, path: tuple[MultiTapeState, ...],
        remap_counter_start: TapeCellState
    ) -> MultiTapeStatePathRemap:
        remap_states = cls(remap_counter_start=remap_counter_start)
        remap_states.insert_overlap_path(path)
        return remap_states


@dataclasses.dataclass
class ComposeTapesResult(object):
    transitions_group: AutomataTransitionsGroup
    state_remap: MultiTapeStatePathRemap

    def get_transition_at(self, index: int) -> tuple[PyProduct, int]:
        return self.transitions_group[index]

    def remap_prod_to_multi_tape(
        self, input_product: PyProduct
    ) -> Result[PyMultiTapeProduct, TapeCellState]:
        """
        Remaps the input product to a multi-tape product based
        on the state remap
        :param input_product:
        :return:
        """
        input_product_terms = input_product.to_flat_terms()
        collected_global_terms: list[D] = []

        for term in input_product_terms:
            sub_product_res = self.remap_term_to_multi_tape(input_term=term)
            if sub_product_res.is_err():
                return sub_product_res

            sub_product = sub_product_res.unwrap()
            sub_product_terms = sub_product.get_flat_terms()
            collected_global_terms.extend(sub_product_terms)

        multi_tape_product = PyMultiTapeProduct(collected_global_terms)
        return Ok(multi_tape_product)

    def remap_term_to_multi_tape(
        self, input_term: A
    ) -> Result[PyMultiTapeProduct, TapeCellState]:
        collected_global_terms: list[D] = []
        position = input_term.get_position()
        global_tape_state = TapeCellState(input_term.get_state())
        multi_tape_states_res = self.state_remap.rev_lookup(
            tape_cell_state=global_tape_state
        )
        if multi_tape_states_res.is_err():
            halt_state = multi_tape_states_res.unwrap_err()
            return Err(halt_state)

        multi_tape_states = multi_tape_states_res.unwrap()
        for multi_tape_state in multi_tape_states:
            tape_no = multi_tape_state.tape_no
            tape_cell_state = multi_tape_state.tape_cell_state
            individual_term = D(
                position=position,
                tape_no=tape_no,
                state=tape_cell_state
            )
            collected_global_terms.append(individual_term)

        multi_tape_product = PyMultiTapeProduct(collected_global_terms)
        return Ok(multi_tape_product)


class MultiTapeBuilder(object):
    def __init__(self, multi_tape_automata: MultiTapeAutomata):
        self._automata = multi_tape_automata
        # tape state -> (relative) position -> overlapping tape state
        # (tape_no, state) -> int -> (tape_no, state)
        # and by overlaps I mean (tape_no, state)
        self._initial_overlaps: TapeOverlaps = TapeOverlaps()

        tape_nos = self.get_tape_nos()
        void_overlap_states = set([
            MultiTapeState(tape_no=tape_no, tape_cell_state=VOID_STATE)
            for tape_no in tape_nos
        ])
        # declare that void states can overlap with one another
        self.declare_initial_group_overlaps(void_overlap_states)

    @property
    def leftmost_extent(self) -> int:
        return self._automata.leftmost_extent

    @property
    def rightmost_extent(self) -> int:
        return self._automata.rightmost_extent

    def get_tape_nos(self) -> list[TapeNo]:
        return self._automata.get_tape_nos()

    def _get_prod_to_state_map(self) -> ProductWritesMap:
        return self._automata.get_prod_to_state_map()

    def declare_initial_group_overlaps(
        self, overlap_states: set[MultiTapeState]
    ):
        """
        Declare that every state in overlap_states
        can overlap with any other state at any relative offset
        in the initial automata tape

        To clarify,
        when I say that a tape state A can overlap with tape state B
        at offset k, I mean that:

        if A is present at some position p on the tape,
        then B can also plausibly be present at position p + k
        at some point in the history of the tape
        :param overlap_states:
        :return:
        """
        tape_nos = self.get_tape_nos()

        for offset in range(self.leftmost_extent, self.rightmost_extent+1):
            for state in overlap_states:
                state_tape_no = state.tape_no

                for other_state in overlap_states:
                    """
                    every tape state could overlap with any other tape state
                    at any offset within the range of possible offsets
                    covered across all the automata's rules
                    """
                    self._initial_overlaps.insert_direct_overlap(
                        source_state=state, target_state=other_state,
                        offset=offset,
                        # min_offset=self.leftmost_extent,
                        # max_offset=self.rightmost_extent
                    )
                for tape_no in tape_nos:
                    if (tape_no == state_tape_no) and (offset == 0):
                        """
                        a tape state can't overlap directly
                        on the same position with void on the same tape
                        """
                        continue

                    # every tape state can overlap with void at any offset
                    tape_void = MultiTapeState(tape_no, VOID_STATE)
                    self._initial_overlaps.insert_direct_overlap(
                        source_state=state, target_state=tape_void,
                        offset=offset,
                        # min_offset=self.leftmost_extent,
                        # max_offset=self.rightmost_extent
                    )

    @staticmethod
    def is_product_satisfiable(
        product: PyMultiTapeProduct, overlaps: TapeOverlaps
    ) -> bool:
        """
        Check if the given product is satisfiable based on the
        overlaps that exist in the automata
        :param overlaps:
        :param product:
        :return:
        """
        terms = product.get_flat_terms()

        for k in range(len(terms)-1):
            term_a, term_b = terms[k], terms[k+1]

            tape_no_a, tape_state_a = term_a.get_state()
            offset_a = term_a.get_position()
            output_state_a = MultiTapeState(
                tape_no=TapeNo(tape_no_a),
                tape_cell_state=TapeCellState(tape_state_a)
            )

            tape_no_b, tape_state_b = term_b.get_state()
            offset_b = term_b.get_position()
            output_state_b = MultiTapeState(
                tape_no=TapeNo(tape_no_b),
                tape_cell_state=TapeCellState(tape_state_b)
            )

            relative_offset = offset_b - offset_a
            overlap_exists = overlaps.can_overlap_exist(
                source_state=output_state_a, target_state=output_state_b,
                offset=relative_offset
            )
            if not overlap_exists:
                """
                output_state_b cannot possibly be found at a position offset 
                of relative_offset from output_state_a
                """
                return False

        return True

    def build_overlaps(self) -> TapeOverlaps:
        """
        Builds a mapping of which tape states can overlap with
        which other tape states at what relative offsets
        :return:
        """
        # TODO: infer existing overlaps from the automata as well
        global_overlaps = copy.deepcopy(self._initial_overlaps)
        # map input products to output tape writes
        prod_to_state_map = self._get_prod_to_state_map()
        # map state -> products that contain it in their input terms
        input_state_to_prod_map = (
            prod_to_state_map.build_input_state_to_prod_map()
        )
        prod_to_state_map.build_state_to_products_map(verbose=True)
        # input products that can effect a new state overlap
        relevant_input_products = list(prod_to_state_map.keys())
        overlaps_updated = True
        round_no: int = 0

        while overlaps_updated:
            overlaps_updated = False
            new_relevant_input_products: set[PyMultiTapeProduct] = set()
            # print(f'{relevant_input_products=}')
            print(f'NEXT_ROUND: {round_no}\n')
            round_no += 1

            tape_overlap_states = global_overlaps.get_all_states()
            lines = global_overlaps.visualize_for_states(tape_overlap_states)
            print('\n'.join(lines))

            for product in relevant_input_products:
                if not self.is_product_satisfiable(product, global_overlaps):
                    continue

                product_writes = prod_to_state_map[product]
                print('SATISFIABLE PRODUCT PRE:', product, product_writes)
                input_terms = product.get_flat_terms()

                for write_tape_no in product_writes:
                    output_tape_cell_state = product_writes[write_tape_no]

                    output_state = MultiTapeState(
                        tape_no=write_tape_no,
                        tape_cell_state=output_tape_cell_state
                    )

                    for input_term in input_terms:
                        # Insert overlaps between the products' constituent
                        # input states and the output state it writes to
                        input_state = MultiTapeState.from_term(input_term)
                        term_offset_from_output = input_term.get_position()
                        term_offset_from_input = -term_offset_from_output

                        overlaps_updated |= global_overlaps.propagate_overlap(
                            source_state=input_state,
                            target_state=output_state,
                            offset=term_offset_from_input,
                            min_offset=self.leftmost_extent,
                            max_offset=self.rightmost_extent
                        )
                        """
                        overlaps_updated |= global_overlaps.insert_direct_overlap(
                            source_state=input_state,
                            target_state=output_state,
                            offset=term_offset_from_input
                        )
                        """

                    if not overlaps_updated:
                        print("SKIP_WRITE", (write_tape_no, output_tape_cell_state))
                        continue

                    print("DO_WRITE", (write_tape_no, output_tape_cell_state))
                    # Get the other products that use the current products'
                    # output state as one of their input states, and add it
                    # to list of products to check for satisfiability later
                    # """
                    affected_products = input_state_to_prod_map[output_state]
                    for affected_product in affected_products:
                        new_relevant_input_products.add(affected_product)
                    # """

                # print('SATISFIABLE PRODUCT:', product, product_writes)
                print('>>>')

            relevant_input_products = new_relevant_input_products

        return global_overlaps

    @classmethod
    def build_product_same_writes_map(
        cls, overlaps: TapeOverlaps, current_product_path: list[D],
        start_offset: int, end_offset: int,
        product_exclusions: ProductTrie
    ) -> ProductWritesMap:
        """
        Generate a mapping of all possible product combinations
        to an output state that is the same as previous input state,
        from an offset of start_offset up until a maximum offset of
        end_offset, given information about all the possible
        overlaps that exist in the automata

        :param product_exclusions:
        if a built product is in product_exclusions, we will
        exclude it from being added to the returned ProductWritesMap
        :param overlaps:
        information about what tape states can overlap with what
        other tape states over all relevant position offsets
        :param current_product_path:
        The current partially built product
        :param start_offset:
        position offset to start / continue product construction from
        :param end_offset:
        position offset to terminate product construction at
        :return:
        A product writes map where the products generated
        will transition every combination of term states along
        the write position offset to itself,
        (so no change from input to output)
        """
        product_writes_map = ProductWritesMap()

        if start_offset == end_offset:
            if product_exclusions.is_end_product:
                return product_writes_map

            current_product = PyMultiTapeProduct(current_product_path)
            product_writes_map.insert_neutral_product(current_product)
            return product_writes_map

        if not current_product_path:
            # if the path is empty, then we construct
            # paths starting with every possible state in the automata
            states = overlaps.get_all_states()
        else:
            last_term = current_product_path[-1]
            last_state = MultiTapeState.from_term(last_term)
            states = overlaps.get_overlaps_for_offset(
                source_state=last_state, offset=start_offset
            )

        for state in states:
            term = state.to_term(offset=start_offset)
            next_product_exclusions = product_exclusions.next(term)

            current_product_path.append(term)
            sub_products = cls.build_product_same_writes_map(
                overlaps=overlaps,
                start_offset=start_offset + 1,
                current_product_path=current_product_path,
                end_offset=end_offset,
                product_exclusions=next_product_exclusions
            )
            product_writes_map.merge(sub_products)
            current_product_path.pop()

        return product_writes_map

    @classmethod
    def build_remap_states(
        cls, tape_nos: list[TapeNo],
        multi_tape_states_map: MultiTapeStatesMap,
        tape_overlaps: TapeOverlaps,
        overlap_state_path: Sequence[MultiTapeState] = (),
        tape_no_index: int = 0,
        remap_counter_start: TapeCellState = TapeCellState(2),
    ) -> MultiTapeStatePathRemap:
        """
        We want to remap all combinations of individual tape states
        that can overlap over each other directly along the same position
        at offset=0 across all tapes to global tape state numbers

        TODO: not sure if its the best to set a default counter start
            and have MultiTapeStatePathRemap merge shift conflicting remaps

        :param tape_no_index:
        index of the current tape we are building the remap
        for in the tape_nos list
        :param tape_nos:
        list of tapes to iterate over for tape state combination generation
        :param tape_overlaps:
        :param multi_tape_states_map:
        TapeNo -> set[TapeCellState]
        for each individual tape with tape no TapeNo,
        contains what tape cell states exist for that particular tape
        :param overlap_state_path:
        The currently built combination of tape states
        :param remap_counter_start:
        :return:
        MultiTapeStatePathRemap instance,
        which is a wrapper for combinations of individual tape states
        to global tape state numbers.
        """
        # counter state cannot collide with void (0) and halt (1) states
        assert remap_counter_start >= 2

        if tape_no_index >= len(tape_nos):
            # TODO: handle void / halt edge cases
            # print("INSERT_PATH", overlap_state_path)
            return MultiTapeStatePathRemap.from_path(
                path=tuple(overlap_state_path),
                remap_counter_start=remap_counter_start
            )

        collated_tape_state_remap = MultiTapeStatePathRemap(
            remap_counter_start=remap_counter_start
        )
        tape_no = tape_nos[tape_no_index]
        # states we are building combinations for in current tape
        next_tape_cell_states_set = multi_tape_states_map[tape_no]
        next_tape_cell_states = list(sorted(next_tape_cell_states_set))

        # what other states can overlap directly on top of
        # the last state in the overlap_state_path
        _overlap_state_path: list[MultiTapeState] = []
        if not isinstance(overlap_state_path, list):
            _overlap_state_path = list(overlap_state_path)
        else:
            _overlap_state_path = overlap_state_path

        if _overlap_state_path:
            prev_tape_state = _overlap_state_path[-1]
            next_state_overlaps: set[MultiTapeState] = (
                tape_overlaps.get_overlaps(prev_tape_state)[0]
            )
        else:
            next_state_overlaps: set[MultiTapeState] = set([
                MultiTapeState(tape_no=tape_no, tape_cell_state=cell_state)
                for cell_state in next_tape_cell_states_set
            ])

        for next_tape_cell_state in next_tape_cell_states:
            next_tape_state = MultiTapeState(
                tape_no=tape_no, tape_cell_state=next_tape_cell_state
            )
            if next_tape_state not in next_state_overlaps:
                # print("SKIP_STATE_1", _overlap_state_path, next_tape_state)
                continue

            # print("PUSH", _overlap_state_path, next_tape_state)
            _overlap_state_path.append(next_tape_state)
            sub_tape_state_path_remap = cls.build_remap_states(
                tape_no_index=tape_no_index+1,
                tape_nos=tape_nos,
                overlap_state_path=_overlap_state_path,
                multi_tape_states_map=multi_tape_states_map,
                tape_overlaps=tape_overlaps,
                remap_counter_start=remap_counter_start,
            )
            collated_tape_state_remap.merge(sub_tape_state_path_remap)
            # print("POP", _overlap_state_path)
            _overlap_state_path.pop()

        return collated_tape_state_remap

    @classmethod
    def build_global_state_path_remap(
        cls, product_writes_map: ProductWritesMap,
        overlaps: TapeOverlaps
    ) -> MultiTapeStatePathRemap:
        """
        remap individual tape states to a global combined tape state
        :param overlaps:
        mapping for which tape states can overlap with which other
        tape states over all relevant relative offsets
        :param product_writes_map:
        mapping containing what output writes are emitted by the
        input products in product_writes_map
        :return:
        """
        # contains which tape cell states can exist in each tape
        multi_tape_states_map: defaultdict[
            TapeNo, set[TapeCellState]
        ] = defaultdict(set)

        for product in product_writes_map:
            product_writes = product_writes_map[product]

            # insert product output terms into multi_tape_states_map
            for tape_no in product_writes:
                tape_cell_state = product_writes[tape_no]
                multi_tape_states_map[tape_no].add(tape_cell_state)

            product_terms = product.get_flat_terms()
            # insert product input terms into multi_tape_states_map
            for product_term in product_terms:
                term_state = MultiTapeState.from_term(product_term)
                tape_no = term_state.tape_no
                tape_cell_state = term_state.tape_cell_state
                multi_tape_states_map[tape_no].add(tape_cell_state)

        tape_nos = sorted(multi_tape_states_map.keys())
        global_tape_state_remap = cls.build_remap_states(
            tape_no_index=0, tape_nos=tape_nos,
            multi_tape_states_map=multi_tape_states_map,
            tape_overlaps=overlaps
        )
        """
        # remap individual tape states to a global combined tape state
        global_tape_state_remap: dict[
            tuple[MultiTapeState], TapeCellState
        ] = dict()

        tape_nos = sorted(multi_tape_states_map.keys())
        global_state_counter: TapeCellState = TapeCellState(2)

        for tape_no in tape_nos:
            tape_cell_states_set = multi_tape_states_map[tape_no]
            tape_cell_states = list(sorted(tape_cell_states_set))

            for tape_cell_state in tape_cell_states:
                tape_output = MultiTapeState(tape_no, tape_cell_state)

                if tape_cell_state == VOID_STATE:
                    # remap all void states to global void state
                    global_tape_state_remap[tape_output] = VOID_STATE
                    continue
                elif tape_cell_state == HALT_STATE:
                    # remap all halt states to global halt state
                    global_tape_state_remap[tape_output] = HALT_STATE
                    continue

                global_tape_state_remap[tape_output] = global_state_counter
                global_state_counter += 1

        return MultiTapeStateRemap.create_from(
            global_tape_state_remap=global_tape_state_remap
        )
        """
        return global_tape_state_remap

    def compose_tapes(self) -> ComposeTapesResult:
        """
        Combine a multi-tape automata into a single tape automata
        TODO: reorder existing products for comparison with generated ones
        :return:
        """
        overlaps = self.build_overlaps()
        # TODO assert that void state can overlap with itself at any offset
        # get all tape states that can exist in each tape
        all_tape_states_per_tape = overlaps.create_whitelist_for_offset()
        preexisting_products = ProductTrie()
        preexisting_writes_map = self._get_prod_to_state_map()
        all_tape_nos = sorted(self.get_tape_nos())

        for multi_tape_product in preexisting_writes_map:
            preexisting_products.insert_product(multi_tape_product)

        """
        Generate rules for all possible term combinations
        that could exist given the state overlaps passed in,
        excluding pre-existing products as they already have explicit 
        output write rule(s).
        
        The products generated here will transition every combination 
        of term states along the write position offset to itself, 
        (so no change from input to output) 
        """
        product_same_writes_map = self.build_product_same_writes_map(
            overlaps=overlaps, current_product_path=[],
            start_offset=self.leftmost_extent,
            end_offset=self.rightmost_extent,
            product_exclusions=preexisting_products
        )
        product_writes_map = ProductWritesMap()
        product_writes_map.merge(preexisting_writes_map)
        product_writes_map.merge(product_same_writes_map)

        # remap individual tape states to a global combined tape state
        global_state_path_remap = self.build_global_state_path_remap(
            product_writes_map=product_same_writes_map, overlaps=overlaps
        )
        # input-output pairs for the final combined automata
        global_transitions_group = AutomataTransitionsGroup(
            num_states=None, transitions=[]
        )

        for multi_tape_product in product_writes_map:
            """
            For every position that is covered by the current product, 
            we want to know which states could be present in the product terms 
            at that position across all individual tapes, 
            and then determine all fully formed term combinations that 
            could satisfy the multi_tape_product
            """
            product_terms = multi_tape_product.get_flat_terms()
            # tape writes that the multi_tape_product produces as output
            product_outputs = product_writes_map[multi_tape_product]
            product_term_positions_set: set[int] = set()
            """
            map position_offset -> tape_no -> choice of possible tape states 
            that are required to be present at the aforementioned 
            (offset, tape_no) in order for the current product input 
            terms to be satisfied
            
            if for a given (position_offset, tape_no) there is no 
            entry in product_state_whitelists, then that means that any 
            tape state of tape tape_no can be assigned at 
            output position offset position_offset while still satisfying 
            the product's input terms
            """
            # TODO: ^ wow this is a mouthful
            product_state_whitelists: defaultdict[
                int, MultiTapeStatesMap
            ] = defaultdict(MultiTapeStatesMap)

            for product_term in product_terms:
                term_offset = product_term.get_position()
                product_term_positions_set.add(term_offset)
                term_state = MultiTapeState.from_term(product_term)
                # tape_no -> set of possible tape cell states at current pos
                product_state_whitelists[term_offset].insert(
                    tape_no=term_state.tape_no, state=term_state
                )

            """
            maps offset (from output) to possible tape states 
            that can exist at said offset such that the product 
            inputs are satisfied.
            
            This (offset_tape_states_map) is different from 
            product_state_whitelists in that:
            
            product_state_whitelists only contains tape states that are 
            explicitly present in the product terms, whereas
            offset_tape_states_map contains all tape states that can
            exist at the given offset so long that the product's
            input terms still remain satisfied
            """
            offset_tape_states_map: defaultdict[
                int, MultiTapeStatesMap
            ] = defaultdict(MultiTapeStatesMap)

            for term_offset in product_state_whitelists:
                # what product term states can occur at each tape
                # for terms at the current term_offset (from product write)
                offset_states_whitelist: MultiTapeStatesMap = (
                    product_state_whitelists[term_offset]
                )
                for tape_no in all_tape_states_per_tape:
                    if tape_no in offset_states_whitelist:
                        continue

                    """
                    If there aren't any constraints on the tape cell states 
                    that can exist on a particular tape_no imposed by the 
                    product terms at the current term_offset, then the set 
                    of states that can exist on that tape_no at the 
                    current term_offset is just the set of all tape cell 
                    states that can exist on that tape in general
                    """
                    assert tape_no not in offset_states_whitelist
                    offset_states_whitelist[tape_no] = (
                        all_tape_states_per_tape[tape_no]
                    )

                offset_tape_states_map[term_offset] = (
                    offset_states_whitelist
                )

            # get input state combinations at offset 0
            # (relative to output position)
            input_zero_whitelist = product_state_whitelists[0]
            # remapped_global_state_set: set[TapeCellState] = set()
            """
            possible tape states that can exist for each tape 
            that exists, along the output write position for the 
            current product, right *after* output has been written 
            """
            post_output_whitelist = copy.deepcopy(input_zero_whitelist)

            for output_tape_no in product_outputs:
                """
                When we spit out output tape_cell_states, we have to 
                consider the possible tape cell state values for tapes 
                that weren't explicitly written to, and remap all 
                possible combinations of unwritten tape states and 
                output tape states to a global tape state 
                """
                output_tape_cell_state = product_outputs[output_tape_no]
                """
                Immediately after writing, the current tape state
                would only have the output tape state
                """
                post_output_whitelist[output_tape_no] = {
                    output_tape_cell_state
                }

            remap_counter_start = TapeCellState(2)
            offset_combos_map: dict[int, MultiTapeStatePathRemap] = {}
            global_combos_remap = MultiTapeStatePathRemap(
                remap_counter_start=remap_counter_start
            )

            for term_offset in product_state_whitelists:
                offset_states_whitelist = offset_tape_states_map[term_offset]
                offset_input_combos = self.build_remap_states(
                    tape_nos=all_tape_nos,
                    multi_tape_states_map=offset_states_whitelist,
                    tape_overlaps=overlaps,
                    remap_counter_start=remap_counter_start
                )
                remap_counter_start = offset_input_combos.next_free_counter
                global_combos_remap.merge(offset_input_combos)
                offset_combos_map[term_offset] = offset_input_combos

            global_combos_remap.merge(
                output_combos := self.build_remap_states(
                    tape_nos=all_tape_nos,
                    multi_tape_states_map=post_output_whitelist,
                    tape_overlaps=overlaps
                )
            )

            product_term_positions = sorted(list(product_term_positions_set))
            """
            Each list item contains the set of possible remapped terms 
            that the corresponding term offset could contain
            """
            product_pos_combos: list[tuple[A, ...]] = []

            for product_term_position in product_term_positions:
                offset_input_combos = offset_combos_map[product_term_position]
                position_combos = offset_input_combos.get_all_state_paths()
                position_remapped_terms: list[A] = []

                for state_path in position_combos:
                    remapped_cell_state = global_state_path_remap[state_path]
                    remapped_term = A(
                        position=product_term_position,
                        state=remapped_cell_state
                    )
                    position_remapped_terms.append(remapped_term)

                product_pos_combos.append(tuple(set(position_remapped_terms)))

            specific_combos = utils.cartesian_product(product_pos_combos)

            for remapped_product_input_terms in specific_combos:
                all_remap_outputs = output_combos.get_all_remap_states()

                for remapped_output_state in all_remap_outputs:
                    global_transitions_group.add_transition(
                        input_terms=tuple(remapped_product_input_terms),
                        output_state=remapped_output_state
                    )

        return ComposeTapesResult(
            transitions_group=global_transitions_group,
            state_remap=global_state_path_remap
        )
