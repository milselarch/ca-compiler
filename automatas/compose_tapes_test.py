from automatas.counter_automata import CounterAutomataRunner
from automatas.rule_generator_multitape import MultiTapeBuilder

runner = CounterAutomataRunner(
    base=6,
    initial_write_start=0,
    initial_write_end=20,
)

multi_tape_builder = MultiTapeBuilder(
    multi_tape_automata=runner.multi_tape_automata
)
compose_result = multi_tape_builder.compose_tapes()
print(f'{compose_result=}')
