import sys
import argparse

from pathlib import Path
from py_ca_compiler.py_ca_compiler import PyProduct
from py_ca_compiler import A

_project_root_dir = Path(__file__).resolve().parents[1]
sys.path.append(str(_project_root_dir))

from automata_builder.counter_automata import (
    CounterAutomataRunner, DT_DATA, DATA_TAPE
)
from automata_builder.rule_generator_multitape import (
    MultiTapeBuilder, MultiTapeState
)

parser = argparse.ArgumentParser()
parser.add_argument(
    '--base',
    nargs='?', type=int, default=2, const=2,
    help='reduction base'
)
parser.add_argument(
    '--write-end',
    nargs='?', type=int, default=20, const=20,
    help='number of initially populated unary cells'
)
parser.add_argument(
    '-d', '--display-transitions',
    nargs='?', type=int, default=10, const=10,
    help='number of transitions to display from composed transition group'
)

args = parser.parse_args()

runner = CounterAutomataRunner(
    base=args.base,
    initial_write_start=0,
    initial_write_end=args.write_end,
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

for k, transition in enumerate(transitions[:args.display_transitions]):
    input_terms, output_state = transition
    input_product = PyProduct(input_terms)
    print(f'[{k}]: {input_product} -> {output_state}')

    input_multi_term_product_res = compose_result.remap_prod_to_multi_tape(
        input_product=input_product
    )
    if input_multi_term_product_res.is_ok():
        input_multi_term_product = input_multi_term_product_res.unwrap()
    else:
        input_multi_term_product = 'HALT'

    output_product_res = compose_result.remap_term_to_multi_tape(
        input_term=A(position=0, state=output_state)
    )
    if output_product_res.is_ok():
        output_product = output_product_res.unwrap()
    else:
        output_product = 'HALT'

    print(f'|{k}|: {input_multi_term_product} -> {output_product}')
