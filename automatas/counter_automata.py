from __future__ import annotations

import os

from typing import Final, Callable
from py_ca_compiler import D

from rule_generator_multitape import (
    MultiTapeAutomataTransitionsGroup, TapeNo, TapeCellState,
    MultiTapeRuleGenerator, MultiTapeAutomata,
    MultiTapeOutput, BLANK_INT
)

DATA_TAPE: Final[TapeNo] = TapeNo(0)
SIGNALS_TAPE: Final[TapeNo] = TapeNo(1)
CARRY_TAPE: Final[TapeNo] = TapeNo(2)

"""
For the signals tape (LSB first to MSB last):
- bits[0] - whether the rest of the state is a counter state 
    - bits[0] == 0: the state is a counter state
        - bits[1] - whether the counter state is paused or not
        - bits[2...] - 1 + the value of the counter state (in base self.base)
          we add one to the counter value to distinguish between the 
          void state (counter value 0) and the counter state with value 0
    - bits[0] == 1: state is a non-counter state 
        - bits[1] == 1: the state is a REDUCE_START state
"""
VOID_STATE: Final[TapeCellState] = TapeCellState(0b0)
HALT_STATE: Final[TapeCellState] = TapeCellState(0b1)

DT_DATA: Final[TapeCellState] = TapeCellState(0b1)
"""
- bits[0] == 1: state is a non-counter state 
- bits[1] == 1: the state is a REDUCE_START state
"""
ST_REDUCE_START: Final[TapeCellState] = TapeCellState(0b11)
CT_DATA: Final[TapeCellState] = TapeCellState(0b1)


def prefill_tape(position: int, tape_no: int) -> Callable[[int], D]:
    def set_cell_state(cell_state: int) -> D:
        return D(position, tape_no, cell_state)

    return set_cell_state


def prefill_tape_no(tape_no: int) -> Callable[[int, int], D]:
    def set_position_and_cell_state(position: int, cell_state: int) -> D:
        return D(position, tape_no, cell_state)

    return set_position_and_cell_state


LEFT: Final[int] = -1
MID: Final[int] = 0
RIGHT: Final[int] = 1

ST: Final[Callable[[int, int], D]] = prefill_tape_no(SIGNALS_TAPE)
DT: Final[Callable[[int, int], D]] = prefill_tape_no(DATA_TAPE)
CT: Final[Callable[[int, int], D]] = prefill_tape_no(CARRY_TAPE)

ST_LEFT: Final[Callable[[int], D]] = prefill_tape(LEFT, SIGNALS_TAPE)
ST_MID: Final[Callable[[int], D]] = prefill_tape(MID, SIGNALS_TAPE)
ST_RIGHT: Final[Callable[[int], D]] = prefill_tape(RIGHT, SIGNALS_TAPE)

DT_LEFT: Final[Callable[[int], D]] = prefill_tape(LEFT, DATA_TAPE)
DT_MID: Final[Callable[[int], D]] = prefill_tape(MID, DATA_TAPE)
DT_RIGHT: Final[Callable[[int], D]] = prefill_tape(RIGHT, DATA_TAPE)

CT_LEFT: Final[Callable[[int], D]] = prefill_tape(LEFT, CARRY_TAPE)
CT_MID: Final[Callable[[int], D]] = prefill_tape(MID, CARRY_TAPE)
CT_RIGHT: Final[Callable[[int], D]] = prefill_tape(RIGHT, CARRY_TAPE)


def build_st_counter_state(counter_digit: int, paused: bool) -> int:
    """
    For the signals tape (LSB first to MSB last):
    - bits[0] - whether the rest of the state is a counter state
        - bits[0] == 0: the state is a counter state
            - bits[1] - whether the counter state is paused or not
            - bits[2...] - 1 + the value of the counter state
              (in base self.base)
              we add one to the counter value to distinguish between the
              void state (counter value 0) and the counter state with
              value 0
        - bits[0] == 1: state is a non-counter state
            - bits[1] == 1: the state is a REDUCE_START state
    """
    assert counter_digit >= 0, "Counter digit must be non-negative"
    # noinspection PyRedundantParentheses
    return (
        (0b00) |  # bit 0: counter state
        (0b10 if paused else 0b00) |  # bit 1: paused or not
        ((counter_digit+1) << 2)  # bits 2...: counter value
    )


def paused_counter(counter_digit: int) -> int:
    """
    Encodes the paused counter cell state in the signals tape
    Generally, paused counter states occur in the outputs of
    transition rules rather than the inputs

    :param counter_digit:
    digit value of the counter state
    :return:
    """
    return build_st_counter_state(counter_digit, paused=True)


def active_counter(counter_digit: int) -> int:
    """
    Encodes the active counter cell state in the signals tape
    Generally, active counter states occur in the inputs of
    transition rules rather than the outputs

    :param counter_digit:
    digit value of the counter state
    :return:
    """
    return build_st_counter_state(counter_digit, paused=False)


def from_counter_state(state: int) -> tuple[int, bool]:
    """
    :param state: counter state in the signals tape encoding
    :return:
    - paused: whether the counter state is paused or not
    - counter_digit: the value of the counter state (in base self.base)
    """
    paused = (state & 0b10) != 0
    counter_digit = (state >> 2) - 1
    return counter_digit, paused


class CounterAutomataBuilder(object):
    def __init__(self, base: int = 2):
        assert base >= 2, "Base must be at least 2"
        self.base = base

    def build_transitions_group(self) -> MultiTapeAutomataTransitionsGroup:
        # TODO: actually precompute the number of states beforehand (?)
        max_counter_digit = self.base-1

        # noinspection PyTypeChecker
        transitions_group = MultiTapeAutomataTransitionsGroup.spawn_new()

        # mark exponential bit reduction start
        transitions_group.add_transition(
            input_terms=(
                ST_LEFT(VOID_STATE), DT_LEFT(DT_DATA),
                DT_MID(VOID_STATE), ST_MID(VOID_STATE)
            ),
            output_tape_no=SIGNALS_TAPE, output_cell_state=ST_REDUCE_START
        )
        # begin the counter accumulator on the right side
        transitions_group.add_transition(
            input_terms=(
                ST_MID(VOID_STATE), ST_RIGHT(VOID_STATE),
                DT_MID(DT_DATA), DT_RIGHT(VOID_STATE)
            ),
            output_tape_no=SIGNALS_TAPE,
            output_cell_state=paused_counter(1)
        )
        # shift leftmost counter value cell and increment
        for digit in range(self.base):
            if digit == max_counter_digit:
                # overflow digit from max_counter_digit to 0 and add new
                # max_counter_digit at the end
                transitions_group.add_transition(
                    input_terms=(
                        DT_MID(DT_DATA),
                        ST_MID(VOID_STATE),
                        ST_RIGHT(active_counter(max_counter_digit))
                    ),
                    output_tape_no=SIGNALS_TAPE,
                    output_cell_state=paused_counter(0),
                )
                # spawn a carry cell state to propagate to digits to the right
                transitions_group.add_transition(
                    input_terms=(
                        DT_MID(DT_DATA),
                        ST_MID(VOID_STATE),
                        ST_RIGHT(active_counter(max_counter_digit))
                    ),
                    output_tape_no=CARRY_TAPE,
                    output_cell_state=CT_DATA,
                )
            else:
                # move digit leftwards and increment by 1
                assert digit < max_counter_digit
                transitions_group.add_transition(
                    input_terms=(
                        DT_MID(DT_DATA),
                        ST_MID(VOID_STATE),
                        ST_RIGHT(active_counter(digit)),
                    ),
                    output_tape_no=SIGNALS_TAPE,
                    output_cell_state=paused_counter(digit+1),
                )

        # apply carry cells to counter cells
        # carry cells stay stationary will counter cells move left
        for mid_digit in range(self.base):
            for right_digit in range(self.base):
                # when there is no carry state to apply, shift left
                transitions_group.add_transition(
                    input_terms=(
                        ST_MID(active_counter(mid_digit)),
                        ST_RIGHT(active_counter(right_digit)),
                        CT_MID(VOID_STATE)
                    ),
                    output_tape_no=SIGNALS_TAPE,
                    output_cell_state=paused_counter(right_digit)
                )
                if right_digit < max_counter_digit:
                    # carry but no overflow (right counter digit < base)
                    carry_no_overflow_combo = (
                        ST_MID(active_counter(mid_digit)),
                        ST_RIGHT(active_counter(right_digit)),
                        CT_MID(CT_DATA)
                    )
                    # move right_digit left and increment
                    transitions_group.add_transition(
                        input_terms=carry_no_overflow_combo,
                        output_tape_no=SIGNALS_TAPE,
                        output_cell_state=paused_counter(right_digit+1)
                    )
                    # overflow right_digit to 0 and move left, cancel carry
                    transitions_group.add_transition(
                        input_terms=carry_no_overflow_combo,
                        output_tape_no=CARRY_TAPE,
                        output_cell_state=VOID_STATE
                    )
                else:
                    # overflow to 0 and move left, carry stays for next digit
                    assert right_digit == max_counter_digit
                    transitions_group.add_transition(
                        input_terms=(
                            ST_MID(active_counter(right_digit)),
                            ST_RIGHT(active_counter(max_counter_digit)),
                            CT_MID(CT_DATA)
                        ),
                        output_tape_no=SIGNALS_TAPE,
                        output_cell_state=paused_counter(0)
                    )

        for digit in range(self.base):
            # if there is a carry, and we're at the end of the built number
            # sequence and the rightmost digit is about to overflow
            right_overflow_combo = (
                ST_MID(active_counter(digit)),
                ST_RIGHT(VOID_STATE),
                CT_MID(CT_DATA)
            )
            transitions_group.add_transition(
                input_terms=right_overflow_combo,
                output_tape_no=SIGNALS_TAPE,
                output_cell_state=paused_counter(1)
            )
            transitions_group.add_transition(
                input_terms=right_overflow_combo,
                output_tape_no=CARRY_TAPE,
                output_cell_state=VOID_STATE
            )

        # clear rightmost counter cell if no carry
        for digit in range(self.base):
            # if right signals tape cell is any void cell
            transitions_group.add_transition(
                input_terms=(
                    ST_MID(active_counter(digit)),
                    ST_RIGHT(VOID_STATE),
                    CT_MID(VOID_STATE)
                ),
                output_tape_no=SIGNALS_TAPE,
                output_cell_state=VOID_STATE
            )
            # if right signals tape cell is reduction start marker
            transitions_group.add_transition(
                input_terms=(
                    ST_MID(active_counter(digit)),
                    ST_RIGHT(ST_REDUCE_START),
                    CT_MID(VOID_STATE)
                ),
                output_tape_no=SIGNALS_TAPE,
                output_cell_state=VOID_STATE
            )

        # bleed counter leftwards past data tape to void
        # unnecessary if we don't expect to leave data tape range
        for digit in range(self.base):
            transitions_group.add_transition(
                input_terms=(
                    DT_MID(VOID_STATE),
                    ST_RIGHT(active_counter(digit)),
                    CT_MID(VOID_STATE)
                ),
                output_tape_no=SIGNALS_TAPE,
                output_cell_state=paused_counter(digit)
            )

        # paused counter states will transition to unpause
        # this should be the one place where paused counter states occur in
        # the inputs of transition rules
        for digit in range(self.base):
            transitions_group.add_transition(
                input_terms=(ST_MID(paused_counter(digit)),),
                output_tape_no=SIGNALS_TAPE,
                output_cell_state=active_counter(digit)
            )

        return transitions_group


class CounterAutomataRunner(object):
    def __init__(
        self, base: int = 6, initial_write_start: int = 0,
        initial_write_end: int = 20
    ):
        self.base = base
        self.builder = CounterAutomataBuilder(base=base)
        self.transitions_group = self.builder.build_transitions_group()
        self.state_eq_map = MultiTapeRuleGenerator.generate_equations(
            self.transitions_group
        )

        self.initial_write_start = initial_write_start
        self.initial_write_end = initial_write_end
        self.multi_tape_automata = MultiTapeAutomata(self.state_eq_map)
        self.multi_tape_automata.init_tapes([DATA_TAPE, SIGNALS_TAPE, CARRY_TAPE])
        self.multi_tape_automata.write_region(
            position=0, end_position=20,
            data=[MultiTapeOutput(DATA_TAPE, DT_DATA)]
        )

    def run_simulation(
        self, num_timesteps: int = 30, terminal_width: int = BLANK_INT,
        render_start: int = -5
    ):
        # print(multi_tape.tapes)
        try:
            terminal_size = os.get_terminal_size()
            default_terminal_width = terminal_size.columns - 1
        except OSError:
            default_terminal_width = 100

        if terminal_width == BLANK_INT:
            terminal_width = default_terminal_width

        for digit in range(self.base):
            print(
                f'{digit=}: '
                f'paused={paused_counter(digit)} '
                f'active={active_counter(digit)}'
            )

        print('')

        for timestep in range(num_timesteps):
            # print(f'{terminal_width=}')
            if timestep > 0:
                self.multi_tape_automata.step()

            render_frame = self.multi_tape_automata.render_tapes(
                start_position=render_start, length=terminal_width,
                cell_width=2
            )
            # print(render_frame.get_dimensions())
            print(f'TIMESTEP {timestep}')
            print(render_frame.render())
            print('')
