import sys

from pathlib import Path

from py_ca_compiler.py_ca_compiler import PyProduct

from automata_builder.rule_generator import RuleGenerator

_project_root_dir = Path(__file__).resolve().parents[1]
sys.path.append(str(_project_root_dir))

from automata_builder.counter_automata import (
    CounterAutomataRunner, DT_DATA, DATA_TAPE
)
from automata_builder.rule_generator_multitape import (
    MultiTapeBuilder, MultiTapeState
)

runner = CounterAutomataRunner(
    base=8,
    initial_write_start=0,
    initial_write_end=20,
)

multi_tape_builder = MultiTapeBuilder(
    multi_tape_automata=runner.multi_tape_automata
)
multi_tape_builder.declare_initial_group_overlaps(
    overlap_states={
        MultiTapeState(tape_no=DATA_TAPE, tape_cell_state=DT_DATA)
    }
)
compose_result = multi_tape_builder.compose_tapes()
transitions = compose_result.transitions_group.transitions
print(f'num transitions = {len(transitions)}')

for k, transition in enumerate(transitions[:100]):
    input_terms, output_state = transition
    input_product = PyProduct(input_terms)
    print(f'[{k}]: {input_product} -> {output_state}')

"""
^CTraceback (most recent call last):
  File "/home/milselarch/projects/ca-compiler/automata_builder/compose_tapes_test.py", line 36, in <module>
    composed_equations = RuleGenerator.generate_equations(
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/home/milselarch/projects/ca-compiler/automata_builder/rule_generator.py", line 253, in generate_equations
    next_state: cls.aggregate_bit_or(state_eq_terms_map[next_state])
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/home/milselarch/projects/ca-compiler/automata_builder/rule_generator.py", line 135, in aggregate_bit_or
    result = result | expr_list[k]
    ^^^^^^
KeyboardInterrupt
"""

"""
# print(f'{compose_result=}')
composed_equations = RuleGenerator.generate_equations(
    transitions_group=compose_result.transitions_group,
    pad_expr_length=False, pad_product_length=False,
    verbose=True
)

for state in composed_equations:
    equation = composed_equations[state]
    print(f'{state} -> {equation}')

"""