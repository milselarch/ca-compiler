from typing import Final

from automata_builder.counter_automata import CounterAutomataRunner
from automata_builder.rule_generator_multitape import BLANK_INT

BASE: Final[int] = 2
WRITE_START: Final[int] = 0
WRITE_END: Final[int] = 1

write_length = WRITE_END - WRITE_START + 1
timesteps = write_length * 4

runner = CounterAutomataRunner(
    base=BASE,
    initial_write_start=WRITE_START,
    initial_write_end=WRITE_END,
)
runner.run_simulation(
    num_timesteps=timesteps,
    terminal_width=BLANK_INT
)

# TODO: add visualize option to show tape states each step
encoded_value = runner.read_signals_tape_value()
print(f'{encoded_value=}')
assert encoded_value == write_length, (
    f'{encoded_value=} != {write_length=}'
)
