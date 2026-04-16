import os

from automatas.counter_automata import CounterAutomataBuilder
from automatas.rule_generator_multitape import (
    MultiTapeRuleGenerator, MultiTapeAutomata
)

counter_automata_builder = CounterAutomataBuilder(base=6)
transitions_group = counter_automata_builder.build_transitions_group()
state_eq_map = MultiTapeRuleGenerator.generate_equations(transitions_group)

multi_tape_automata = MultiTapeAutomata(state_eq_map)
multi_tape = multi_tape_automata.multi_tape
terminal_size = os.get_terminal_size()
terminal_width = terminal_size.columns

render_frame = multi_tape.render_tapes(
    start_position=0, length=terminal_width
)
print(render_frame.render())
