import os

from counter_automata import CounterAutomataBuilder, DT_DATA, DATA_TAPE
from rule_generator_multitape import (
    MultiTapeRuleGenerator, MultiTapeAutomata,
    BiDirectionalMultiTape, MultiTapeOutput
)

counter_automata_builder = CounterAutomataBuilder(base=6)
transitions_group = counter_automata_builder.build_transitions_group()
state_eq_map = MultiTapeRuleGenerator.generate_equations(transitions_group)

multi_tape_automata = MultiTapeAutomata(state_eq_map)
multi_tape: BiDirectionalMultiTape = multi_tape_automata.multi_tape
multi_tape.write_region(
    position=0, end_position=10,
    data=[MultiTapeOutput(DATA_TAPE, DT_DATA)]
)

# print(multi_tape.tapes)
terminal_size = os.get_terminal_size()
terminal_width = terminal_size.columns
# print(f'{terminal_width=}')

render_frame = multi_tape.render_tapes(
    start_position=0, length=terminal_width
)
# print(render_frame.get_dimensions())
print(render_frame.render())
