from __future__ import annotations

import copy
import dataclasses

from collections import defaultdict
from typing import Final, List, TypeVar, Iterator, Tuple

from py_ca_compiler import (
    D, PyMultiTapeProduct, PyMultiTapeExpression
)

from renderer import RenderFrame


class TapeNo(int):
    pass


class TapeCellState(int):
    pass


BLANK_INT: Final[int] = -1
VOID_STATE: Final[TapeCellState] = TapeCellState(0b0)
HALT_STATE: Final[TapeCellState] = TapeCellState(0b1)

T = TypeVar('T')
U = TypeVar('U')


def zip_preserve_types(a: List[T], b: List[U]) -> Iterator[Tuple[T, U]]:
    return zip(a, b)


@dataclasses.dataclass
class MultiTapeOutput:
    tape_no: TapeNo
    tape_cell_state: TapeCellState

    def __hash__(self):
        return hash((self.tape_no, self.tape_cell_state))

    def to_term(self, offset: int = 0) -> D:
        return D(
            position=offset,
            tape_no=self.tape_no, state=self.tape_cell_state
        )

    @classmethod
    def from_term(cls, term: D):
        tape_no, tape_cell_state = term.get_state()
        return cls(
            tape_no=TapeNo(tape_no),
            tape_cell_state=TapeCellState(tape_cell_state)
        )


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

    def init_tapes(self, tape_nos: list[TapeNo], freeze: bool = True):
        for tape_no in tape_nos:
            self.get_or_make_tape(tape_no)

        if freeze:
            self._freeze_tapes = True

    def get_tape_nos(self) -> list[TapeNo]:
        return sorted(list(self._tapes.keys()))

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

        leftmost_extent, rightmost_extent = self.get_rule_range()
        self._leftmost_extent: int = leftmost_extent
        self._rightmost_extent: int = rightmost_extent
        self._state_eq_map = state_eq_map

    def get_tape_nos(self) -> list[TapeNo]:
        return self._multi_tape.get_tape_nos()

    def get_prod_to_state_map(self) -> ProductWritesMap:
        return copy.deepcopy(self._prod_to_state_map)

    def get_state_eq_map(self) -> dict[
        MultiTapeOutput, PyMultiTapeExpression
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

        :param state_eq_map:
        :return:
        """
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


class TapeOverlaps(object):
    """
    we say that a tape state A can overlap with tape state B at offset k if
    in the history of the automata it is possible that:
    1. A is present at some position p on the tape and
    2. B to be present at position p + k.

    This structure stores all possible state overlaps
    """
    def __init__(self):
        self._overlaps: defaultdict[
            MultiTapeOutput, defaultdict[int, set[MultiTapeOutput]]
        ] = defaultdict(lambda: defaultdict(set))

    def get_all_states(self) -> set[MultiTapeOutput]:
        return set(self._overlaps.keys())

    def get_overlaps_for_offset(
        self, source_state: MultiTapeOutput, offset: int
    ) -> set[MultiTapeOutput]:
        return copy.copy(self._overlaps[source_state][offset])

    def get_overlaps(
        self, source_state: MultiTapeOutput
    ) -> defaultdict[int, set[MultiTapeOutput]]:
        return copy.deepcopy(self._overlaps[source_state])

    def insert_overlap(
        self, source_state: MultiTapeOutput, target_state: MultiTapeOutput,
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
        source_overlaps = self._overlaps[source_state][offset]
        target_overlaps = self._overlaps[target_state][-offset]

        if target_state in source_overlaps:
            # overlap already exists
            return False

        source_overlaps.add(target_state)
        assert source_state not in target_overlaps
        target_overlaps.add(source_state)
        return True

    def can_overlap_exist(
        self, source_state: MultiTapeOutput,
        target_state: MultiTapeOutput, offset: int
    ) -> bool:
        """
        :param source_state:
        :param target_state:
        :param offset:
        offset of term with target_state FROM term with source_state
        :return:
        """
        return target_state in self._overlaps[source_state][offset]


@dataclasses.dataclass
class ProductTrie(object):
    """
    A trie of product terms nested from smallest to largest term offset
    """
    is_end: bool = False
    # map offset from current term to next trie
    next_term: defaultdict[D, ProductTrie] = dataclasses.field(
        default_factory=lambda: defaultdict(ProductTrie)
    )

    def _insert_term_path(self, term_path: list[D]):
        if not term_path:
            return

        current_term, next_terms = term_path[0], term_path[1:]
        self.next_term[current_term]._insert_term_path(next_terms)

    def insert_term_path(self, term_path: list[D]):
        term_path = sorted(term_path, key=lambda term: term.get_position())
        self._insert_term_path(term_path)

    def insert_product(self, product: PyMultiTapeProduct):
        terms = product.get_flat_terms()
        self.insert_term_path(terms)

    def _has_term_path(self, term_path: list[D]) -> bool:
        if not term_path:
            return self.is_end

        current_term, next_terms = term_path[0], term_path[1:]
        if current_term not in self.next_term:
            return False

        return self.next_term[current_term]._has_term_path(next_terms)

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

    def __iter__(self):
        return iter(self.prod_to_state_map.items())

    def items(self):
        return self.prod_to_state_map.items()

    def keys(self):
        return self.prod_to_state_map.keys()

    def values(self):
        return self.prod_to_state_map.values()

    def __getitem__(self, item):
        return copy.copy(self.prod_to_state_map[item])

    def insert(
        self, product: PyMultiTapeProduct, tape_output: MultiTapeOutput
    ):
        write_tape_no = tape_output.tape_no
        write_tape_cell_state = tape_output.tape_cell_state

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


class MultiTapeBuilder(object):
    def __init__(self, multi_tape_automata: MultiTapeAutomata):
        self._automata = multi_tape_automata
        # tape state -> (relative) position -> overlapping tape state
        # (tape_no, state) -> int -> (tape_no, state)
        # and by overlaps I mean (tape_no, state)
        self._initial_overlaps: TapeOverlaps = TapeOverlaps()

        tape_nos = self.get_tape_nos()
        void_overlap_states = set([
            MultiTapeOutput(tape_no=tape_no, tape_cell_state=VOID_STATE)
            for tape_no in tape_nos
        ])
        # declare that void states can overlap with one another
        self.declare_group_overlaps(void_overlap_states)

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

    def declare_group_overlaps(
        self, overlap_states: set[MultiTapeOutput]
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
                for other_state in overlap_states:
                    # every tape state could overlap with any other tape state
                    # at any offset within the range of possible offsets
                    # covered across all the automata's rules
                    self._initial_overlaps.insert_overlap(
                        source_state=state, target_state=other_state,
                        offset=offset
                    )
                for tape_no in tape_nos:
                    # every tape state can overlap with void at any offset
                    tape_void = MultiTapeOutput(tape_no, VOID_STATE)
                    self._initial_overlaps.insert_overlap(
                        source_state=state, target_state=tape_void,
                        offset=offset
                    )

    @staticmethod
    def is_prodict_satisfiable(
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
            output_state_a = MultiTapeOutput(
                tape_no=TapeNo(tape_no_a),
                tape_cell_state=TapeCellState(tape_state_a)
            )

            tape_no_b, tape_state_b = term_b.get_state()
            offset_b = term_b.get_position()
            output_state_b = MultiTapeOutput(
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

    def build_input_state_to_prod_map(self) -> defaultdict[
        MultiTapeOutput, set[PyMultiTapeProduct]
    ]:
        prod_to_state_map = self._get_prod_to_state_map()
        # map state -> products that contain it in their input terms
        input_state_to_prod_map: defaultdict[
            MultiTapeOutput, set[PyMultiTapeProduct]
        ] = defaultdict(set)

        for product in prod_to_state_map:
            input_terms = product.get_flat_terms()

            for input_term in input_terms:
                input_state = MultiTapeOutput.from_term(input_term)
                input_state_to_prod_map[input_state].add(product)

        return input_state_to_prod_map

    def build_overlaps(self):
        # TODO: infer existing overlaps from the automata as well
        global_overlaps = copy.deepcopy(self._initial_overlaps)
        # map input products to output tape writes
        prod_to_state_map = self._get_prod_to_state_map()
        # map state -> products that contain it in their input terms
        input_state_to_prod_map = self.build_input_state_to_prod_map()
        # input products that can effect a new state overlap
        relevant_input_products = list(prod_to_state_map.keys())

        while relevant_input_products:
            new_relevant_input_products: set[PyMultiTapeProduct] = set()

            for product in relevant_input_products:
                if not self.is_prodict_satisfiable(product, global_overlaps):
                    continue

                new_relevant_input_products.add(product)
                product_writes = prod_to_state_map[product]
                input_terms = product.get_flat_terms()

                for write_tape_no in product_writes:
                    output_tape_cell_state = product_writes[write_tape_no]
                    output_state = MultiTapeOutput(
                        tape_no=write_tape_no,
                        tape_cell_state=output_tape_cell_state
                    )

                    for input_term in input_terms:
                        # Insert overlaps between the products' constituent
                        # input states and the output state it writes to
                        input_state = MultiTapeOutput.from_term(input_term)
                        term_offset_from_output = input_term.get_position()
                        term_offset_from_input = -term_offset_from_output

                        global_overlaps.insert_overlap(
                            source_state=input_state,
                            target_state=output_state,
                            offset=term_offset_from_input
                        )

                    # Get the other products that use the current products'
                    # output state as one of their input states, and add it
                    # to list of products to check for satisfiability later
                    affected_products = input_state_to_prod_map[output_state]
                    for affected_product in affected_products:
                        new_relevant_input_products.add(affected_product)

            relevant_input_products = new_relevant_input_products

        return global_overlaps

    @staticmethod
    def _get_zero_terms(product: PyMultiTapeProduct) -> list[D]:
        terms = product.get_flat_terms()
        zero_terms = []

        for term in terms:
            if term.get_position() == 0:
                zero_terms.append(term)

        return zero_terms

    def compose(self):
        """
        TODO: reorder existing products for comparison with generated ones
        :return:
        """
        overlaps = self.build_overlaps()

        def build_all_products(
            current_product: list[D], offset: int, rightmost_extent: int
        ) -> defaultdict[PyMultiTapeProduct, dict[TapeNo, TapeCellState]]:
            if offset == rightmost_extent:
                # TODO: check against existing products as well
                return {PyMultiTapeProduct(current_product)}

            if not current_product:
                states = overlaps.get_all_states()
            else:
                last_term = current_product[-1]
                last_state = MultiTapeOutput.from_term(last_term)
                states = overlaps.get_overlaps_for_offset(
                    source_state=last_state, offset=offset
                )

            all_products: set[PyMultiTapeProduct] = set()

            for state in states:
                term = state.to_term(offset=offset)
                current_product.append(term)
                sub_products = build_all_products(
                    offset=offset+1,
                    current_product=current_product,
                    rightmost_extent=rightmost_extent
                )
                all_products = all_products | sub_products
                current_product.pop()

            return all_products

        all_products = build_all_products(
            current_product=[], offset=self.leftmost_extent,
            rightmost_extent=self.rightmost_extent
        )
        return all_products
