import os

from automatas.counter_automata import CARRY_TAPE
from counter_automata import (
    CounterAutomataBuilder, DT_DATA, DATA_TAPE, SIGNALS_TAPE
)
from rule_generator_multitape import (
    MultiTapeRuleGenerator, MultiTapeAutomata,
    BiDirectionalMultiTape, MultiTapeOutput
)

counter_automata_builder = CounterAutomataBuilder(base=6)
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


for timestep in range(10):
    # print(f'{terminal_width=}')
    render_frame = multi_tape_automata.render_tapes(
        start_position=-20, length=terminal_width-1, cell_width=2
    )
    # print(render_frame.get_dimensions())
    print(f'TIMESTEP {timestep}')
    print(render_frame.render())
    print('')
    multi_tape_automata.step()
