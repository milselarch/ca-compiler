from __future__ import annotations

import copy
import dataclasses
import typing

from collections import defaultdict
from typing import Iterator, Sequence
from py_ca_compiler import D, PyMultiTapeProduct

from automata_builder.rule_generator import (
    TapeCellState, TapeNo, VOID_STATE
)
from utils import (
    FreezableDefaultDict, FreezableSet, Freezable, FrozenSet,
    FrozenDict, FreezableDict
)


@dataclasses.dataclass(frozen=True)
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

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, MultiTapeState):
            return False

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


InnerOverlaps = FreezableDefaultDict[
    MultiTapeState,
    FreezableDefaultDict[int, FreezableSet[MultiTapeState]]
]


class TapeOverlaps(Freezable):
    """
    We say that a tape state A can overlap with tape state B at offset k if
    in the history of the automata it is possible that:
    1. A is present at some position p on the tape and
    2. B to be present at position p + k.

    This structure stores all possible state overlaps

    Note that all overlaps are symmetrical, since if tape state A
    can overlap with tape state B at offset k,
    then tape state B can overlap with tape state A at offset -k
    """
    def __init__(self, overlaps: InnerOverlaps | None = None):
        """
        A: MultiTapeState -> B: int -> C: set[MultiTapeState]

        For a given source state A,
        the mapping gives all target states B that can
        be present at offset k from A, for all possible offsets k.
        """
        super().__init__()
        if overlaps is None:
            self._overlaps: InnerOverlaps = FreezableDefaultDict(
                lambda: FreezableDefaultDict(FreezableSet)
            )
        else:
            self._overlaps = overlaps

    def _freeze(self) -> None:
        self._overlaps.freeze()

    def _encode(self) -> tuple:
        return self._overlaps.encode()

    def __iter__(self) -> Iterator[MultiTapeState]:
        return iter(self._overlaps)

    @classmethod
    def _decode(cls, data: tuple) -> typing.Self:
        return cls(FreezableDefaultDict.decode(data))

    def __contains__(self, item: object):
        if not isinstance(item, MultiTapeState):
            raise TypeError(f'not {MultiTapeState.__name__}')

        return item in self._overlaps

    def delete_state(self, state: MultiTapeState):
        if self._frozen:
            raise ValueError("Can't delete from frozen tape overlaps")

        if state in self._overlaps:
            del self._overlaps[state]

        for _, source_overlaps in self._overlaps.items():
            for _, offset_overlaps in source_overlaps.items():
                if state in offset_overlaps:
                    offset_overlaps.remove(state)

    def freeze(self) -> bool:
        self._frozen = True
        return self._overlaps.freeze()

    def freeze_copy(self) -> FrozenTapeOverlaps:
        return self.to_frozen()

    def encode(self):
        return self._overlaps.encode()

    def items(self):
        return self._overlaps.items()

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, TapeOverlaps):
            return False

        if self._overlaps.keys() != other._overlaps.keys():
            return False

        return self.encode() == other.encode()

    def __or__(self, other: object):
        if not isinstance(other, self.__class__):
            raise TypeError(
                f"Unsupported operand type(s) for |: "
                f"{other.__class__.__name__}"
            )

    @classmethod
    def merge(
        cls, input_tape_overlaps: Sequence[TapeOverlaps]
    ) -> TapeOverlaps:
        new_overlaps = cls()

        for tape_overlaps in input_tape_overlaps:
            for source_state, source_overlaps in tape_overlaps.items():
                for offset, offset_overlaps in source_overlaps.items():
                    for target_state in offset_overlaps:
                        new_overlaps.insert_direct_overlap(
                            source_state=source_state,
                            offset=offset, target_state=target_state
                        )

        return new_overlaps

    def get_cell_states_for_tape(self, tape_no: TapeNo) -> set[TapeCellState]:
        return set([
            state.tape_cell_state for state in
            self.get_states_for_tape(tape_no=tape_no)
        ])

    def get_states_for_tape(
        self, tape_no: TapeNo
    ) -> FreezableSet[MultiTapeState]:
        tape_states: set[MultiTapeState] = set()

        for source_state in self._overlaps:
            if source_state.tape_no == tape_no:
                tape_states.add(source_state)

        return FreezableSet(tape_states)

    def get_all_states(self) -> set[MultiTapeState]:
        return set(self._overlaps.keys())

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
        min_offset: int = 0
        max_offset: int = 0

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

            target_states_set: set[MultiTapeState] = (
                overlap_map[offset].clone_data()
            )
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

    def get_overlaps_for_offset(
        self, source_state: MultiTapeState, offset: int
    ) -> FreezableSet[MultiTapeState]:
        return self._overlaps[source_state][offset]

    def get_overlaps(
        self, source_state: MultiTapeState
    ) -> FreezableDefaultDict[int, FreezableSet[MultiTapeState]]:
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

    def validate_mutual_overlaps_for(
        self, source_state: MultiTapeState
    ) -> None:
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
        offset: int, min_offset: int | None, max_offset: int | None
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
            if (min_offset is not None) and source_state_offset < min_offset:
                continue
            if (max_offset is not None) and source_state_offset > max_offset:
                continue

            target_overlap_states = target_overlaps_map[target_state_offset]
            source_overlap_states = source_overlaps_map[source_state_offset]
            prev_target_overlap_states = copy.copy(target_overlap_states)
            # prev_source_overlap_states = copy.copy(source_overlap_states)

            for target_overlap_state in prev_target_overlap_states:
                # overlaps_snapshot = copy.deepcopy(self)\
                if target_overlap_state in source_overlap_states:
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

                assert target_overlap_state in source_overlaps_map[
                    source_state_offset
                ]
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

                # TODO: insert source_state = source_overlap_state
                #  at target_offset?
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
        if self._frozen:
            raise ValueError("Can't insert direct overlap when frozen")

        has_tape_mutual_exclusion = (
            source_state.tape_no == target_state.tape_no and
            source_state.tape_cell_state != target_state.tape_cell_state and
            offset == 0
        )
        if has_tape_mutual_exclusion:
            # states have same-tape mutual exclusion, so can't overlap
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

    def remove_direct_overlap(
        self, source_state: MultiTapeState, target_state: MultiTapeState,
        offset: int
    ):
        if self._frozen:
            raise ValueError("Can't remove direct overlap when frozen")

        source_overlaps = self._overlaps[source_state][offset]
        target_overlaps = self._overlaps[target_state][-offset]

        if target_state in source_overlaps:
            source_overlaps.remove(target_state)
        if source_state not in target_overlaps:
            target_overlaps.remove(source_state)

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
        if source_state not in self._overlaps:
            return False

        source_overlaps = self._overlaps[source_state]
        if offset not in source_overlaps:
            return False

        offset_overlaps = source_overlaps[offset]
        return target_state in offset_overlaps

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
            # source_tape_no = source_state.tape_no
            # source_tape_cell_state = source_state.tape_cell_state
            state_overlaps = self._overlaps[source_state]
            offset_state_overlaps = state_overlaps[offset]

            for offset_state_overlap in offset_state_overlaps:
                whitelist.insert(
                    tape_no=offset_state_overlap.tape_no,
                    state=offset_state_overlap
                )

        return whitelist

    def to_unfrozen(self):
        return self.__class__(
            overlaps=self._overlaps.to_unfrozen()
        )

    def to_frozen(self) -> FrozenTapeOverlaps:
        return FrozenTapeOverlaps(
            overlaps=self._overlaps.to_frozen()
        )


class FrozenTapeOverlaps(TapeOverlaps):
    def __init__(self, overlaps: InnerOverlaps):
        if not overlaps.is_frozen:
            raise ValueError("Tape overlaps need to be frozen")

        super().__init__(overlaps)
        self.freeze()

    def to_unfrozen(self) -> TapeOverlaps:
        return TapeOverlaps(
            overlaps=self._overlaps.to_unfrozen()
        )

    def __hash__(self) -> int:
        return hash(self._overlaps)


@dataclasses.dataclass(frozen=True)
class TapeOverlapsFSMState(object):
    _tape_overlaps: FrozenTapeOverlaps
    _relevant_input_products: FrozenSet[PyMultiTapeProduct]
    _product_writes_map: FrozenProductWritesMap

    @property
    def tape_overlaps(self) -> FrozenTapeOverlaps:
        return self._tape_overlaps

    @property
    def relevant_input_products(self) -> FrozenSet[PyMultiTapeProduct]:
        return self._relevant_input_products

    @property
    def product_writes_map(self) -> FrozenProductWritesMap:
        return self._product_writes_map

    def __post_init__(self) -> None:
        if not self._tape_overlaps.is_frozen:
            raise ValueError("Tape overlaps needs to be frozen")
        if not self._relevant_input_products.is_frozen:
            raise ValueError("Input products need to be frozen")

    @classmethod
    def create(
        cls, tape_overlaps: TapeOverlaps,
        relevant_input_products: FreezableSet[PyMultiTapeProduct],
        product_writes_map: ProductWritesMap
    ):
        return cls(
            _tape_overlaps=tape_overlaps.to_frozen(),
            _relevant_input_products=relevant_input_products.to_frozen(),
            _product_writes_map=product_writes_map.to_frozen()
        )


@dataclasses.dataclass
class MultiTapeStateAttributes(object):
    """
    :param writable:
    Whether there are products that can produce the target state
    :param deletable:
    Whether there are products that can transition away from
    the target state
    :param instant_delete:
    whether all instances of target_state are immediately deleted
    this happens if there is a rule target_state -> other_state
    """
    writable: bool
    deletable: bool
    instant_delete: bool


class ProductWritesMap(Freezable):
    """
    map product -> tape_no -> output tape cell state
    TODO: rewrite all of this in rust
    """
    def __init__(self, prod_to_state_map: FreezableDefaultDict[
        PyMultiTapeProduct, FreezableDict[TapeNo, TapeCellState]
    ] | None = None):
        super().__init__()
        self._prod_to_state_map: FreezableDefaultDict[
            PyMultiTapeProduct, FreezableDict[TapeNo, TapeCellState]
        ] = FreezableDefaultDict(
            default_factory=FreezableDict
        )

        if prod_to_state_map is not None:
            self._prod_to_state_map = prod_to_state_map

    def _freeze(self) -> None:
        self._prod_to_state_map.freeze()

    def _encode(self) -> tuple:
        return self._prod_to_state_map.encode()

    @classmethod
    def _decode(cls, data: tuple) -> typing.Self:
        return cls(FreezableDefaultDict.decode(data))

    def get_translated_variants(
        self, target_product: PyMultiTapeProduct
    ) -> set[tuple[PyMultiTapeProduct, int]]:
        """
        :param target_product:
        :return:
        All products which have the same form as the input product
        but all the offsets are shifted by a constant amount

        e.g.
        D(0,1,1)*D(1,2,1) and D(1,1,1)*D(2,2,1) are translational variants
        (second product is shifted by +1 offset)
        """
        if target_product not in self._prod_to_state_map:
            raise KeyError(
                f"Product {target_product} is not in {self._prod_to_state_map}"
            )

        translational_variants: set[tuple[PyMultiTapeProduct, int]] = set()
        input_terms = sorted(target_product.get_flat_terms())
        all_products = self._prod_to_state_map.keys()

        for target_product in all_products:
            target_terms = sorted(target_product.get_flat_terms())
            if len(input_terms) != len(target_terms):
                continue

            offset_diffs = set()
            for input_term, target_term in zip(input_terms, target_terms):
                input_offset = input_term.get_position()
                target_offset = target_term.get_position()
                offset_diff = target_offset - input_offset
                offset_diffs.add(offset_diff)

            if len(offset_diffs) == 1:
                translational_variants.add(
                    (target_product, offset_diffs.pop())
                )

        return translational_variants

    def extinct_input_state(self, state: MultiTapeState):
        """
        Remove all input products that contain the input state
        :param state:
        :return:
        """
        if self._frozen:
            raise ValueError("Cannot modify when frozen")

        for product in self._prod_to_state_map.keys():
            input_terms = product.get_flat_terms()

            for input_term in input_terms:
                input_multi_tape_state = MultiTapeState.from_term(input_term)
                if input_multi_tape_state == state:
                    del self._prod_to_state_map[product]

    def purge_unsatisfiable_products(
        self, state_attributes_map: FrozenDict[
            MultiTapeState, MultiTapeStateAttributes
        ]
    ):
        """
        Remove all input products that are unsatisfiable
        :return:
        """
        if self._frozen:
            raise ValueError("Cannot modify when frozen")

        for product in list(self._prod_to_state_map.keys()):
            becomes_unsatisfiable = self.does_product_becomes_unsatisfiable(
                product=product, state_attributes_map=state_attributes_map
            )
            if becomes_unsatisfiable:
                print("UNSATISFIABLE", product, product.get_annotation())
                del self._prod_to_state_map[product]

    def to_unfrozen(self):
        return self.__class__(
            prod_to_state_map=self._prod_to_state_map.to_unfrozen()
        )

    def to_frozen(self) -> FrozenProductWritesMap:
        return FrozenProductWritesMap(
            prod_to_state_map=self._prod_to_state_map.to_frozen()
        )

    def __iter__(self) -> Iterator[PyMultiTapeProduct]:
        return iter(self._prod_to_state_map.keys())

    def __delitem__(self, product: PyMultiTapeProduct) -> None:
        if self._frozen:
            raise ValueError("Cannot remove product when frozen")

        del self._prod_to_state_map[product]

    def items(self):
        return self._prod_to_state_map.items()

    def does_product_becomes_unsatisfiable(
        self, product: PyMultiTapeProduct,
        state_attributes_map: FrozenDict[
            MultiTapeState, MultiTapeStateAttributes
        ]
    ) -> bool:
        if product not in self._prod_to_state_map:
            raise KeyError(
                f"Product {product} is not in {self._prod_to_state_map}"
            )

        if product.get_annotation() == 'EXP_REDUCE_START':
            print("TOMATO")

        has_transition_to_unsatisfiability = False
        has_input_terms_along_output_offset = False
        product_writes = self._prod_to_state_map[product]

        input_terms = product.get_flat_terms()
        for input_term in input_terms:
            input_multi_tape_state = MultiTapeState.from_term(input_term)
            input_term_attrs = state_attributes_map[input_multi_tape_state]
            input_term_offset = input_term.get_position()

            if input_term_offset != 0:
                if input_term_attrs.writable:
                    """
                    If any of the input product's terms can be spawned
                    from somewhere else, and said term does not lie along
                    the output position, then at some point its possible that
                    the state will be spawned and make the product
                    satisfiable
                    """
                    return False

                continue

            assert input_term_offset == 0
            has_input_terms_along_output_offset = True
            input_term_tape_no = TapeNo(input_term.get_tape_no())
            # input_term_tape_cell_state = input_term.get_cell_state()
            if input_term_tape_no not in product_writes:
                # output does not write to the same tape as input term
                # -> we don't care to check for unsatisfiability in that case
                continue

            output_tape_cell_state = product_writes[input_term_tape_no]
            output_multi_tape_state = MultiTapeState(
                tape_no=input_term_tape_no,
                tape_cell_state=output_tape_cell_state
            )
            output_term_attrs = state_attributes_map[output_multi_tape_state]

            input_is_unwritable = not input_term_attrs.writable
            output_term_is_undeletable = not output_term_attrs.deletable
            in_out_neq = input_multi_tape_state != output_multi_tape_state
            """
            if:
            1. all input terms outside write position are unwritable,
               (if input_term_offset != 0: if input_term_attrs.writable:)
            2. some input along offset 0 must be unwritable, 
            3. and that input must be replaced with an undeletable state, 
            4. and the replaced state is different from the input state,
            -> then we can be sure that the product will no longer be used
               moving forward
            
            Property #1 & #2 would mean that new instances of such products  
            will not form from elsewhere (i.e. other positions). 
            
            Property #3 & #4 would mean that all current combinations of 
            terms that satisfy the product transition away from 
            satisfying it, and remain so permanently  
            """
            has_transition_to_unsatisfiability |= (
                input_is_unwritable and
                output_term_is_undeletable and
                in_out_neq
            )

        if (1, 8) in product_writes.items():
            print("POTATO")

        if not has_input_terms_along_output_offset:
            """
            all input terms are offset away from output, and
            none of them can be spawned moving forward
            """
            return True

        return has_transition_to_unsatisfiability

    def build_input_products(self) -> FreezableSet[PyMultiTapeProduct]:
        relevant_input_products = FreezableSet()
        for product in self._prod_to_state_map:
            relevant_input_products.add(product)

        return relevant_input_products

    def get_state_writes_for(
        self, product: PyMultiTapeProduct
    ) -> list[MultiTapeState]:
        writes_map = self._prod_to_state_map[product]
        tape_state_writes: list[MultiTapeState] = []

        for tape_no in writes_map:
            tape_cell_state = writes_map[tape_no]
            tape_state = MultiTapeState(
                tape_no=tape_no, tape_cell_state=tape_cell_state
            )
            tape_state_writes.append(tape_state)

        return tape_state_writes

    def build_all_state_attrs_map(
        self, extant_states: set[MultiTapeState] | None
    ) -> FrozenDict[
        MultiTapeState, MultiTapeStateAttributes
    ]:
        all_states = self.get_states_set()
        state_attributes_map: dict[
            MultiTapeState, MultiTapeStateAttributes
        ] = {}

        for state in all_states:
            state_attributes = self.get_state_attributes(
                state, extant_states=extant_states
            )
            state_attributes_map[state] = state_attributes

        return FrozenDict(state_attributes_map)

    def get_state_attributes(
        self, target_state: MultiTapeState,
        extant_states: set[MultiTapeState] | None
    ) -> MultiTapeStateAttributes:
        """
        :param target_state:
        State to get attributes for
        :param extant_states:
        Tape states that we declare exist in the cellular automata
        at the time step we are getting attributes for

        If None, then the generated state attributes will be applicable
        throughout all future timesteps of the automata, otherwise
        it will only be applicable for the time step we are getting
        attributes for
        :return:
        """
        target_tape_no = target_state.tape_no
        target_tape_cell_state = target_state.tape_cell_state
        """
        :writable:
        Whether there are products that can produce the target_state
        :deletable:
        Whether there are products that can transition away from 
        the target_state
        """
        writable, deletable = False, False
        """
        whether all instances of target_state are immediately deleted
        this happens if there is a rule target_state -> other_state
        """
        instant_delete = False

        for product in self._prod_to_state_map:
            if extant_states is not None:
                """
                Check if the input states required for the product 
                all exist in extant_states, if not the product 
                is unsatisfiable
                """
                is_product_unsatisfiable = False
                for input_term in product.get_flat_terms():
                    multi_tape_state = MultiTapeState.from_term(input_term)
                    if multi_tape_state not in extant_states:
                        is_product_unsatisfiable = True
                        break

                if is_product_unsatisfiable:
                    continue

            writes_map = self._prod_to_state_map[product]
            target_state_written = False

            for tape_no in writes_map:
                tape_cell_state = writes_map[tape_no]
                write_state = MultiTapeState(
                    tape_no=tape_no, tape_cell_state=tape_cell_state
                )
                if write_state == target_state:
                    """
                    output writes to the same tape and same state as
                    target_state, so it is being "created" per-se
                    """
                    target_state_written = True

            # whether product writes to same tape as target_state
            # and the written TapeCellState is different from target_state
            writes_away_from_target_state = False
            # whether the product transitions cells
            # with input target_state to output target_state
            # (idempotency is relative to target_state only)
            is_idempotent_transition = False

            for input_term in product.get_flat_terms():
                if input_term.get_position() != 0:
                    continue
                if input_term.get_tape_no() != target_state.tape_no:
                    continue

                tape_cell_state = input_term.get_cell_state()
                if tape_cell_state != target_state.tape_cell_state:
                    continue

                output_tape_cell_state = writes_map.get(
                    target_state.tape_no, target_state.tape_cell_state
                )
                if target_state.tape_cell_state != output_tape_cell_state:
                    """
                    Our input product contains target_state
                    along that output position (offset 0) and the
                    output writes to the same tape, but to a different state,
                    so the original target_state is deleted
                    """
                    writes_away_from_target_state = True
                else:
                    is_idempotent_transition = True

            if len(product) == 1:
                input_term = product.get_flat_terms()[0]
                assert input_term.get_cell_state() != VOID_STATE, (
                    f"VOID STATE CANNOT AUTO TRANSITION AWAY - {product}"
                )
                target_tape_write_state = writes_map.get(target_tape_no, None)
                instant_delete |= (
                    # input term has same tape as target term
                    input_term.get_tape_no() == target_tape_no and
                    # and input term has same tape state as target term
                    input_term.get_cell_state() == target_tape_cell_state and
                    # and product writes back to same tape but with a
                    # different state than the original target_tape_cell_state
                    target_tape_write_state != target_tape_cell_state
                )

            writable |= target_state_written and not is_idempotent_transition
            deletable |= writes_away_from_target_state

        return MultiTapeStateAttributes(
            writable=writable, deletable=deletable,
            instant_delete=instant_delete
        )

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

        for product in self._prod_to_state_map:
            writes_map = self._prod_to_state_map[product]

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

        for product in self._prod_to_state_map:
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

        for tape_product in self._prod_to_state_map:
            product_terms = tape_product.get_flat_terms()

            for term in product_terms:
                tape_no, tape_cell_state = term.get_state()
                states_set.add(MultiTapeState(
                    tape_no=TapeNo(tape_no),
                    tape_cell_state=TapeCellState(tape_cell_state)
                ))

        return states_set

    def keys(self):
        return self._prod_to_state_map.keys()

    def values(self):
        return self._prod_to_state_map.values()

    def __getitem__(self, item: PyMultiTapeProduct):
        return copy.copy(self._prod_to_state_map[item])

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
        writes_map = self._prod_to_state_map[product]
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


class FrozenProductWritesMap(ProductWritesMap):
    def __init__(self,  prod_to_state_map: FreezableDefaultDict[
        PyMultiTapeProduct, FreezableDict[TapeNo, TapeCellState]
    ] | None = None):
        if prod_to_state_map and not prod_to_state_map.is_frozen:
            raise ValueError(
                "FrozenProductWritesMap only works with "
                "frozen prod_to_state_map"
            )

        super().__init__(prod_to_state_map=prod_to_state_map)
        self.freeze()

    def to_unfrozen(self) -> ProductWritesMap:
        return ProductWritesMap(
            prod_to_state_map=self._prod_to_state_map.to_unfrozen()
        )


class TapeOverlapsFSM(object):
    def __init__(self, initial_fsm_state: TapeOverlapsFSMState):
        self._initial_fsm_state = initial_fsm_state
        self._existing_fsm_states: set[TapeOverlapsFSMState] = {
            self._initial_fsm_state
        }
        self._next_fsm_states: dict[
            TapeOverlapsFSMState, TapeOverlapsFSMState
        ] = {}

    def __len__(self):
        return len(self._next_fsm_states)

    def __contains__(self, other):
        if not isinstance(other, TapeOverlapsFSMState):
            return False

        return other in self._existing_fsm_states

    def insert(
        self, state: TapeOverlapsFSMState,
        next_state: TapeOverlapsFSMState
    ) -> tuple[TapeOverlapsFSMState, bool]:
        """  
        :param state:
        :param next_state:
        :return:
        frozen copy of inserted next_fsm_state, and whether
        the inserted fsm state are newly inserted into the FSM
        (i.e. they didn't already exist)
        """
        if state not in self._existing_fsm_states:
            raise ValueError(
                f'Overlaps FSM state "{state}" does not exist'
            )

        if state not in self._next_fsm_states:
            self._next_fsm_states[state] = next_state
            self._existing_fsm_states.add(next_state)
            assert next_state in self
            return next_state, True
        else:
            existing_next_fsm_state = self._next_fsm_states[state]

            if existing_next_fsm_state != next_state:
                raise ValueError(
                    f"Conflicting next state for {state=}: "
                    f"{existing_next_fsm_state=} vs {next_state=}"
                )
            return existing_next_fsm_state, False

    def merge(self) -> TapeOverlaps:
        return TapeOverlaps.merge([
            fsm_state.tape_overlaps
            for fsm_state in self._existing_fsm_states
        ])
