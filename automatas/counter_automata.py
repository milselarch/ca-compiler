from __future__ import annotations

from enum import StrEnum, IntEnum
from typing import Final

from py_ca_compiler import D

from automatas.rule_generator import AutomataTransitionsGroup


DATA_TAPE: Final[int] = 0
SIGNALS_TAPE: Final[int] = 1
COUNTER_TAPE: Final[int] = 2

"""
For the signals tape (LSB first to MSB last):
- bits[0] - whether the rest of the state is a counter state 
    - bits[0] == 0: the state is a counter state
        - bits[1] - whether the counter state is paused or not
        - bits[2...] - the value of the counter state (in base self.base)
    - bits[0] == 1: state is a non-counter state 
        - bits[1] == 1: the state is a REDUCE_START state
"""
VOID: Final[int] = 0b0
HALT: Final[int] = 0b1

DT_DATA: Final[int] = 0b1
"""
- bits[0] == 1: state is a non-counter state 
- bits[1] == 1: the state is a REDUCE_START state
"""
ST_REDUCE_START: Final[int] = 0b11


def DT(position: int, cell_state: int) -> D:
    return D(position, DATA_TAPE, cell_state)


def ST(position: int, signal_state: int) -> D:
    return D(position, SIGNALS_TAPE, signal_state)


def CT(position: int, counter_value: int) -> D:
    return D(position, COUNTER_TAPE, counter_value)


class Tape(object):
    def __init__(self):
        self.forward = [0]
        self.backward = []

    def read_at(self, position: int) -> int:
        if position >= 0:
            if position >= len(self.forward):
                return 0

            return self.forward[position]
        else:
            idx = -position - 1
            if idx >= len(self.backward):
                return 0

            return self.backward[idx]

    def __getitem__(self, position: int) -> int:
        return self.read_at(position)


class CounterAutomataBuilder(object):
    def __init__(self, base: int = 2):
        assert base >= 2, "Base must be at least 2"
        self.base = base

    def build_transitions_group(self) -> AutomataTransitionsGroup:
        # TODO: actually precompute the number of states beforehand
        # noinspection PyTypeChecker
        transitions_group = AutomataTransitionsGroup.spawn_new(float('inf'))

        transitions_group.add_transition(
            input_terms=(ST(-1, VOID), DT(-1, DT_DATA), ST(0, VOID)),
            output_state=ST(0, ST_REDUCE_START)
        )
        return transitions_group
