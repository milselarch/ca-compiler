import os

from typing import Final
from automatas.counter_automata import (
    CARRY_TAPE, paused_counter, active_counter
)
from counter_automata import (
    CounterAutomataBuilder, DT_DATA, DATA_TAPE, SIGNALS_TAPE
)
from rule_generator_multitape import (
    MultiTapeRuleGenerator, MultiTapeAutomata,
    BiDirectionalMultiTape, MultiTapeOutput
)

BASE: Final[int] = 6
counter_automata_builder = CounterAutomataBuilder(base=BASE)
transitions_group = counter_automata_builder.build_transitions_group()
state_eq_map = MultiTapeRuleGenerator.generate_equations(transitions_group)

multi_tape_automata = MultiTapeAutomata(state_eq_map)
multi_tape_automata.init_tapes([DATA_TAPE, SIGNALS_TAPE, CARRY_TAPE])
multi_tape_automata.write_region(
    position=0, end_position=10,
    data=[MultiTapeOutput(DATA_TAPE, DT_DATA)]
)

# print(multi_tape.tapes)
try:
    terminal_size = os.get_terminal_size()
    terminal_width = terminal_size.columns
except OSError:
    terminal_width = 100

for digit in range(BASE):
    print(
        f'{digit=}: '
        f'pasued={paused_counter(digit)} '
        f'active={active_counter(digit)}'
    )

print('')

for timestep in range(30):
    # print(f'{terminal_width=}')
    if timestep > 0:
        multi_tape_automata.step()

    render_frame = multi_tape_automata.render_tapes(
        start_position=-5, length=terminal_width-1, cell_width=2
    )
    # print(render_frame.get_dimensions())
    print(f'TIMESTEP {timestep}')
    print(render_frame.render())
    print('')
