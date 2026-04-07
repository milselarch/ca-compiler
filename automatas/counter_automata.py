from __future__ import annotations

from enum import StrEnum, IntEnum

from automatas.rule_generator import AutomataTransitionsGroup


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


class FixedTapeStates(IntEnum):
    VOID = 0b0000000
    HALT = 0b0000001
    # initial input on the data tape
    DATA = 0b0000010
    # counter accumulator start marker on far right
    # should not overlap with any other states
    START = 0b000100
    # carry state on counter tape
    # overlaps with signals and data states
    CARRY = 0b001000


class CounterAutomataBuilder(object):
    def __init__(self, base: int = 2):
        assert base >= 2, "Base must be at least 2"
        self.base = base

    def build_transitions_group(self) -> AutomataTransitionsGroup:
        # TODO: actually precompute the number of states beforehand
        # noinspection PyTypeChecker
        transitions_group = AutomataTransitionsGroup.spawn_new(float('inf'))

        transitions_group.add_transition(
            (A)
        )
        return transitions_group
