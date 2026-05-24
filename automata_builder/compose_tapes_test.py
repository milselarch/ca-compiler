import sys

from pathlib import Path

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
# print(f'{compose_result=}')
composed_equations = RuleGenerator.generate_equations(
    transitions_group=compose_result.transitions_group,
    pad_expr_length=False, pad_product_length=False,
    verbose=True
)

for state in composed_equations:
    equation = composed_equations[state]
    print(f'{state} -> {equation}')
