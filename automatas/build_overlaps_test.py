import sys

from pathlib import Path

_project_root_dir = Path(__file__).resolve().parents[1]
sys.path.append(str(_project_root_dir))

from automatas.counter_automata import CounterAutomataRunner
from automatas.rule_generator_multitape import MultiTapeBuilder, MultiTapeState, TapeCellState, TapeNo

runner = CounterAutomataRunner(
    base=6,
    initial_write_start=0,
    initial_write_end=20,
)

multi_tape_builder = MultiTapeBuilder(
    multi_tape_automata=runner.multi_tape_automata
)
tape_overlaps = multi_tape_builder.build_overlaps()
lines = tape_overlaps.visualize_for(MultiTapeState(
    tape_no=TapeNo(0), tape_cell_state=TapeCellState(0)
))

print('\n'.join(lines))
