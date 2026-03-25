from __future__ import annotations

from enum import StrEnum, IntEnum


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


class DataTypeState(IntEnum):
    VOID = 0
    DATA = 1


class SignalsTapeState(IntEnum):
    VOID = 0
    EXP_REDUCE_START = 1
    SWEEP_RIGHT = 2
    SWEEP_LEFT = 3
    LEFT_PRE_ADD = 4


class CounterAutomataBuilder(object):
    def __init__(self, base: int = 2):
        assert base >= 2, "Base must be at least 2"
        self.base = base

    def build_automata(self):
        raise NotImplementedError()
