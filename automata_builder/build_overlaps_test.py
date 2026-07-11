from automata_builder.rule_generator_multitape import (
    MultiTapeBuilder, MultiTapeState, TapeCellState, TapeNo, TapeOverlaps
)
from automata_builder.counter_automata import (
    CounterAutomataRunner, DT_DATA, DATA_TAPE
)

runner = CounterAutomataRunner(
    base=6,
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
tape_overlaps: TapeOverlaps = multi_tape_builder.build_overlaps()
tape_overlap_states = tape_overlaps.get_all_states()
# print(f'{tape_overlap_states=}')
# print('')

lines = tape_overlaps.visualize_for_states(tape_overlap_states)

print("\nFINAL_TAPE_OVERLAPS")
print('')
print('\n'.join(lines))
