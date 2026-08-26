from __future__ import annotations

import copy
import dataclasses
import utils

from result import Result, Ok, Err
from collections import defaultdict
from typing import Sequence

from utils import FreezableSet, FrozenSet
from automata_builder.rule_generator import (
    AutomataTransitionsGroup, TapeCellState, TapeNo,
    VOID_STATE, HALT_STATE, BLANK_INT
)
from automata_builder.renderer import RenderFrame
from automata_builder.tape_overlaps import (
    MultiTapeState, TapeOverlaps, MultiTapeStatesMap,
    ProductWritesMap, TapeOverlapsFSM, TapeOverlapsFSMState, FrozenTapeOverlaps, FrozenProductWritesMap
)
from py_ca_compiler import (
    D, PyMultiTapeProduct, PyMultiTapeExpression,
    A, PyProduct
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

    def __len__(self):
        return len(self.transitions)

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

    def __or__(
        self, other: MultiTapeTransitionsGroup
    ) -> MultiTapeTransitionsGroup:
        if not isinstance(other, MultiTapeTransitionsGroup):
            raise TypeError(f'unexpected type {type(other)}')

        require_annotation = (
            self.require_annotation or other.require_annotation
        )
        if require_annotation:
            if not self.require_annotation:
                raise ValueError(
                    "Cannot combine transitions while other group "
                    "does not require annotation"
                )
            elif not other.require_annotation:
                raise ValueError(
                    "Cannot combine transitions while own group "
                    "requires annotation"
                )

        combined = self.__class__(require_annotation=require_annotation)
        combined.transitions.extend(copy.deepcopy(self.transitions))
        combined.transitions.extend(copy.deepcopy(other.transitions))
        return combined


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
            assert len(self.void_state_paths) == 1
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
class TransitionOptimizations(object):
    disappeared_states: set[MultiTapeState]
    new_prod_to_state_map: ProductWritesMap
    whitelist_overlaps: FrozenTapeOverlaps


@dataclasses.dataclass
class ComposeTapesResult(object):
    transitions_group: AutomataTransitionsGroup
    state_remap: MultiTapeStatePathRemap

    def get_transition_at(self, index: int) -> tuple[PyProduct, int]:
        return self.transitions_group[index]

    def count_unique_states(self):
        return len(self.state_remap.get_all_remap_states())

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

        if len(terms) == 1:
            # if overlaps contain the one term, then product is satisfiable
            tape_state = MultiTapeState.from_term(terms[0])
            return tape_state in overlaps

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

    @classmethod
    def _build_whitelist_overlaps(
        cls, overlaps_fsm_state: TapeOverlapsFSMState,
        states_written: dict[MultiTapeState, set[PyMultiTapeProduct]],
        verbose: bool = False
    ) -> FrozenTapeOverlaps:
        """
        If for some newly spawned state_written
        that did not exist in the previous overlaps,
        if for some offset e,

        every contributing product to the state_written
        has a translated variant that also writes to the
        same tape as state_written at said offset e,

        then we know that state_written can only overlap
        with the states produced by the translated variant
        products
        """
        def log(*args, **kwargs):
            if verbose:
                print(*args, **kwargs)

        prev_overlaps = overlaps_fsm_state.tape_overlaps
        prod_to_state_map = overlaps_fsm_state.product_writes_map
        # all states that could exist at the start of current time step
        all_prev_overlap_states = prev_overlaps.get_all_states()
        whitelist_overlaps = TapeOverlaps()

        for state_written in states_written:
            if state_written in all_prev_overlap_states:
                continue

            state_written_tape_no = state_written.tape_no
            log(f"NEW SPAWNED STATE {state_written}")
            writing_products = states_written[state_written]
            covering_products: defaultdict[
                int, set[PyMultiTapeProduct]
            ] = defaultdict(set)

            for contributing_product in writing_products:
                translated_prods = prod_to_state_map.get_translated_variants(
                    target_product=contributing_product
                )
                for translated_prod, offset in translated_prods:
                    prod_writes = prod_to_state_map.get_state_writes_for(
                        translated_prod
                    )
                    if state_written_tape_no not in prod_writes:
                        continue

                    covering_products[offset].add(translated_prod)

            for offset, translated_prods in covering_products.items():
                if offset == 0:
                    continue
                if len(translated_prods) != len(writing_products):
                    # translated products don't cover
                    # all products contributing to the state_written
                    continue

                for translated_prod in translated_prods:
                    writes = prod_to_state_map[translated_prod]
                    written_tape_cell_state = writes[state_written_tape_no]
                    whitelist_overlaps.insert_overlaps_for(
                        source_state=state_written,
                        target_state=written_tape_cell_state,
                        offset=offset, min_offset=None, max_offset=None
                    )

        return whitelist_overlaps.to_frozen()

    @classmethod
    def _create_optimizations(
        cls, overlaps_fsm_state: TapeOverlapsFSMState,
        states_written: dict[MultiTapeState, set[PyMultiTapeProduct]],
        verbose: bool = False
    ) -> TransitionOptimizations:
        """
        :param overlaps_fsm_state:
        overlaps FSM state at start of time step
        (i.e. previous overlaps FSM state)

        :param states_written:
        Mapping of states -> contributing products
        that were spawned in the current timestep* from said products

        Note that this does not make any claims
        on whether the states written here did not already exist
        in the automata / overlaps at the start of timestep

        :param verbose:
        :return:
        """
        def log(*args, **kwargs):
            if verbose:
                print(*args, **kwargs)

        prev_overlaps = overlaps_fsm_state.tape_overlaps
        prev_prod_to_state_map = overlaps_fsm_state.product_writes_map
        # all states that could exist at the start of current time step
        all_prev_overlap_states = prev_overlaps.get_all_states()
        extinct_states: set[MultiTapeState] = set()
        # states that cease to exist in tapes after current time step
        disappeared_states: set[MultiTapeState] = set()
        prod_to_state_map = prev_prod_to_state_map.to_unfrozen()

        for prev_overlap_state in all_prev_overlap_states:
            current_state_attrs = prev_prod_to_state_map.get_state_attributes(
                prev_overlap_state, extant_states=all_prev_overlap_states
            )
            # whether state has no occurrences after current time step
            no_state_occurrences_post_transition = (
                current_state_attrs.instant_delete and
                prev_overlap_state not in states_written
            )
            if no_state_occurrences_post_transition:
                log(f"DISAPPEARED STATE {prev_overlap_state}")
                disappeared_states.add(prev_overlap_state)

                if not current_state_attrs.writable:
                    # a state is extinct if it will never show up again
                    # in any future time step
                    extinct_states.add(prev_overlap_state)
                    prod_to_state_map.extinct_input_state(prev_overlap_state)

        # TODO: apply overlap_states_at_offsets to tape_overlaps
        # TODO: refactor automata builder to its own repo?
        whitelist_overlaps = cls._build_whitelist_overlaps(
            overlaps_fsm_state=overlaps_fsm_state,
            states_written=states_written
        )

        # remove products that will never be satisfiable after
        # current time step
        state_attrs_map = prod_to_state_map.build_all_state_attrs_map(
            extant_states=None
        )
        prod_to_state_map.purge_unsatisfiable_products(
            state_attributes_map=state_attrs_map
        )
        return TransitionOptimizations(
            new_prod_to_state_map=prod_to_state_map,
            disappeared_states=disappeared_states
        )

    def transition_overlaps(
        self, overlaps_fsm_state: TapeOverlapsFSMState,
        overlaps_fsm: TapeOverlapsFSM, verbose: bool = False
    ) -> TapeOverlapsFSMState:
        """
        :param overlaps_fsm_state:
        :param overlaps_fsm:
        :param verbose:
        :return:
        new tape overlaps, and set of input products that could
        be affected by the new overlaps
        """
        def log(*args, **kwargs):
            if verbose:
                print(*args, **kwargs)

        prev_overlaps = overlaps_fsm_state.tape_overlaps
        relevant_input_products = overlaps_fsm_state.relevant_input_products
        prev_prod_to_state_map = overlaps_fsm_state.product_writes_map

        prev_overlaps.print_for_states()
        new_relevant_input_products: set[PyMultiTapeProduct] = set()
        """
        Collection of states that were spawned in the 
        current timestep* - note that this does not make any claims 
        on whether the states written here did not already exist 
        in the automata / overlaps at the start of timestep
        """
        states_written: defaultdict[
            MultiTapeState, set[PyMultiTapeProduct]
        ] = defaultdict(set)

        overlaps = prev_overlaps.to_unfrozen()
        # print(f'{relevant_input_products=}')
        input_state_to_prod_map = (
            prev_prod_to_state_map.build_input_state_to_prod_map()
        )

        for product in relevant_input_products:
            if not self.is_product_satisfiable(product, prev_overlaps):
                log('NO_SAT <<<', product, product.get_annotation())
                continue

            log('IS_SAT >>>', product, product.get_annotation())
            product_writes = prev_prod_to_state_map[product]
            # print('SATISFIABLE PRODUCT PRE:', product, product_writes)
            input_terms = product.get_flat_terms()

            for write_tape_no in product_writes:
                output_tape_cell_state = product_writes[write_tape_no]
                output_state = MultiTapeState(
                    tape_no=write_tape_no,
                    tape_cell_state=output_tape_cell_state
                )
                states_written[output_state].add(product)
                overlaps_updated = False

                for input_term in input_terms:
                    # Insert overlaps between the products' constituent
                    # input states and the output state it writes to
                    input_state = MultiTapeState.from_term(input_term)
                    term_offset_from_output = input_term.get_position()
                    term_offset_from_input = -term_offset_from_output

                    overlaps_updated |= overlaps.propagate_overlap(
                        source_state=input_state,
                        target_state=output_state,
                        offset=term_offset_from_input,
                        min_offset=self.leftmost_extent,
                        max_offset=self.rightmost_extent
                    )
                    assert overlaps_fsm_state in overlaps_fsm

                write_pair = (write_tape_no, output_tape_cell_state)
                if not overlaps_updated:
                    # print("SKIP_WRITE", write_pair)
                    continue

                log("DO_WRITE", write_pair)
                # Get the other products that use the current products'
                # output state as one of their input states, and add it
                # to list of products to check for satisfiability later
                # """
                affected_products = input_state_to_prod_map[output_state]
                for affected_product in affected_products:
                    new_relevant_input_products.add(affected_product)
                # """

            # print('SATISFIABLE PRODUCT:', product, product_writes)
            # print('>>>')

        optimizations = self._create_optimizations(
            overlaps_fsm_state=overlaps_fsm_state,
            states_written=states_written,
            verbose=verbose
        )
        # TODO: refactor to optimizations.apply_to(fsm_state) -> new_fsm_state
        disappeared_states = optimizations.disappeared_states
        for disappeared_state in disappeared_states:
            overlaps.delete_state(disappeared_state)

        log(f'{states_written=}')
        log(f'{disappeared_states=}')

        whitelist_overlaps = optimizations.whitelist_overlaps
        for source_state in overlaps:
            # TODO: can we just remove direct overlaps?

        prod_to_state_map = optimizations.new_prod_to_state_map.to_frozen()
        return TapeOverlapsFSMState.create(
            tape_overlaps=overlaps.to_frozen(),
            relevant_input_products=FrozenSet(new_relevant_input_products),
            product_writes_map=prod_to_state_map
        )

    def build_overlaps(self, verbose: bool = True) -> TapeOverlaps:
        """
        Builds a mapping of which tape states can overlap with
        which other tape states at what relative offsets
        :return:
        """
        def log(*args, **kwargs):
            if verbose:
                print(*args, **kwargs)

        # map input products to output tape writes
        prod_to_state_map = self._get_prod_to_state_map()
        relevant_input_products = prod_to_state_map.build_input_products()

        # TODO: infer existing overlaps from the automata as well
        initial_fsm_state = TapeOverlapsFSMState.create(
            tape_overlaps=self._initial_overlaps.to_frozen(),
            relevant_input_products=relevant_input_products,
            product_writes_map=prod_to_state_map
        )
        overlaps_fsm = TapeOverlapsFSM(initial_fsm_state=initial_fsm_state)
        prev_fsm_state: TapeOverlapsFSMState = initial_fsm_state

        assert prev_fsm_state in overlaps_fsm
        # prod_to_state_map.build_state_to_products_map(verbose=True)
        overlaps_fsm_updated = True
        round_no: int = 0

        while overlaps_fsm_updated:
            log(f'NEXT_ROUND: {round_no}\n')
            round_no += 1

            next_fsm_state = self.transition_overlaps(
                overlaps_fsm_state=prev_fsm_state,
                overlaps_fsm=overlaps_fsm,
                verbose=verbose
            )
            # print(len(overlaps_fsm._existing_overlaps))
            _, overlaps_fsm_updated = overlaps_fsm.insert(
                state=prev_fsm_state, next_state=next_fsm_state
            )
            log(f'{overlaps_fsm_updated=}')
            prev_fsm_state = next_fsm_state
            assert prev_fsm_state in overlaps_fsm

        if verbose:
            log(f'overlaps FSM has {len(overlaps_fsm)} states')

        merged_overlaps = overlaps_fsm.merge()
        return merged_overlaps

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
        TODO: if we have all the vcriant states of a tape, skip the tape

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
        The currently built combination of tape states, or None
        None is used as a stand-in for every possible state for the
        particular tape at tape_no_index
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

        if not _overlap_state_path:
            # Use all available states fur the current tape
            # as the overlap path is empty / just started
            next_state_overlaps = tape_overlaps.get_states_for_tape(tape_no)
        else:
            # get the overlapping states for prev_tape_state
            prev_tape_state = _overlap_state_path[-1]
            next_state_overlaps: FreezableSet[MultiTapeState] = (
                tape_overlaps.get_overlaps(prev_tape_state)[0]
            )

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
        multi_tape_states_map = MultiTapeStatesMap()

        for product in product_writes_map:
            product_writes = product_writes_map[product]
            # insert product output terms into multi_tape_states_map
            for tape_no in product_writes:
                tape_cell_state = product_writes[tape_no]
                multi_tape_states_map.insert(tape_no, tape_cell_state)

            product_terms = product.get_flat_terms()
            # insert product input terms into multi_tape_states_map
            for product_term in product_terms:
                term_state = MultiTapeState.from_term(product_term)
                tape_no = term_state.tape_no
                tape_cell_state = term_state.tape_cell_state
                multi_tape_states_map.insert(tape_no, tape_cell_state)

        tape_nos = multi_tape_states_map.get_tape_nos()
        global_tape_state_remap = cls.build_remap_states(
            tape_no_index=0, tape_nos=tape_nos,
            multi_tape_states_map=multi_tape_states_map,
            tape_overlaps=overlaps
        )
        return global_tape_state_remap

    @classmethod
    def get_terms_at_output_pos(
        cls, terms: Sequence[A]
    ) -> Sequence[A]:
        terms_at_output_pos: list[A] = []

        for term in terms:
            if term.get_position() == 0:
                terms_at_output_pos.append(term)

        return terms_at_output_pos

    def build_transitions_for_product(
        self, multi_tape_product: PyMultiTapeProduct,
        product_writes_map: ProductWritesMap,
        all_tape_states_per_tape: MultiTapeStatesMap,
        global_overlaps: TapeOverlaps,
        global_state_path_remap: MultiTapeStatePathRemap
    ) -> AutomataTransitionsGroup:
        """
        For every position that is covered by the current product,
        we want to know which states could be present in the product
        terms at that position across all individual tapes,
        and then determine all fully formed term combinations that
        could satisfy the multi_tape_product
        """
        transitions_group = AutomataTransitionsGroup.spawn_new(None)
        all_tape_nos = sorted(self.get_tape_nos())
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
        for term_offset in product_state_whitelists:
            offset_states_whitelist = offset_tape_states_map[term_offset]
            offset_input_combos = self.build_remap_states(
                tape_nos=all_tape_nos,
                multi_tape_states_map=offset_states_whitelist,
                tape_overlaps=global_overlaps,
                remap_counter_start=remap_counter_start
            )
            remap_counter_start = offset_input_combos.next_free_counter
            offset_combos_map[term_offset] = offset_input_combos

        product_term_positions = sorted(list(product_term_positions_set))
        assert 0 in product_term_positions
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
                if remapped_term in position_remapped_terms:
                    continue

                position_remapped_terms.append(remapped_term)

            product_pos_combos.append(tuple(position_remapped_terms))

        """
        Get every specific combination of term states that could satisfy 
        the current product's input terms
        """
        specific_combos = utils.cartesian_product(product_pos_combos)
        for remapped_product_input_terms in specific_combos:
            input_terms_at_output_pos = self.get_terms_at_output_pos(
                remapped_product_input_terms
            )
            if len(input_terms_at_output_pos) != 1:
                raise ValueError(
                    f'There should only be one term at output position '
                    f'within {remapped_product_input_terms}'
                )

            input_term_at_output_pos = input_terms_at_output_pos[0]
            input_tape_cell_state_at_output_pos = TapeCellState(
                input_term_at_output_pos.get_state()
            )
            input_path_at_output_pos_res = global_state_path_remap.rev_lookup(
                tape_cell_state=input_tape_cell_state_at_output_pos
            )

            remapped_output_state: TapeCellState = HALT_STATE
            if input_path_at_output_pos_res.is_ok():
                output_state_path = input_path_at_output_pos_res.unwrap()
                remapped_output_state = global_state_path_remap[
                    output_state_path
                ]

            transitions_group.add_transition(
                input_terms=tuple(remapped_product_input_terms),
                output_state=remapped_output_state,
                ban_halt_state=True
            )

        return transitions_group

    def compose_tapes(self) -> ComposeTapesResult:
        """
        Combine a multi-tape automata into a single tape automata
        TODO: reorder existing products for comparison with generated ones
        :return:
        """
        global_overlaps = self.build_overlaps()
        # TODO assert that void state can overlap with itself at any offset
        # get all tape states that can exist in each tape
        all_tape_states_per_tape: MultiTapeStatesMap = (
            global_overlaps.create_whitelist_for_offset()
        )
        preexisting_products = ProductTrie()
        preexisting_writes_map = self._get_prod_to_state_map()
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
            overlaps=global_overlaps, current_product_path=[],
            start_offset=self.leftmost_extent,
            end_offset=self.rightmost_extent,
            product_exclusions=preexisting_products
        )
        product_writes_map = ProductWritesMap()
        product_writes_map.merge(preexisting_writes_map)
        product_writes_map.merge(product_same_writes_map)

        # remap individual tape states to a global combined tape state
        global_state_path_remap = self.build_global_state_path_remap(
            product_writes_map=product_same_writes_map,
            overlaps=global_overlaps
        )
        # input-output pairs for the final combined automata
        global_transitions_group = AutomataTransitionsGroup(
            num_states=None, transitions=[]
        )

        for multi_tape_product in product_writes_map:
            product_transitions_group = self.build_transitions_for_product(
                multi_tape_product=multi_tape_product,
                product_writes_map=product_writes_map,
                all_tape_states_per_tape=all_tape_states_per_tape,
                global_overlaps=global_overlaps,
                global_state_path_remap=global_state_path_remap
            )
            global_transitions_group.merge(product_transitions_group)

        return ComposeTapesResult(
            transitions_group=global_transitions_group,
            state_remap=global_state_path_remap
        )
