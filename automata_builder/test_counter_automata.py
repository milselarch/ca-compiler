import argparse

from automata_builder.rule_generator_multitape import BLANK_INT
from automata_builder.counter_automata import CounterAutomataRunner


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Run the counter automata simulation.'
    )
    parser.add_argument(
        '--base', '-b',
        type=int,
        default=6,
        help='Numerical base for the counter automata (default: 6)'
    )
    parser.add_argument(
        '--write-start', '-s',
        type=int,
        default=0,
        help='Starting position to write the initial data (default: 0)'
    )
    parser.add_argument(
        '--write-end', '-e',
        type=int,
        default=20,
        help='Ending position to write the initial data (default: 20)'
    )
    parser.add_argument(
        '--render-start', '-r',
        type=int,
        default=-5,
        help='Starting position for rendering the tapes (default: -5)'
    )
    parser.add_argument(
        '--timesteps', '-t',
        type=int,
        default=BLANK_INT,
        help='Number of timesteps to run automata forward for'
    )
    parser.add_argument(
        '--terminal-width', '-w',
        type=int,
        default=BLANK_INT,
        help=f'Terminal width for rendering'
    )

    args = parser.parse_args()
    timesteps = args.timesteps
    if timesteps == BLANK_INT:
        cells_filled = args.write_end - args.write_start + 1
        timesteps = cells_filled

    runner = CounterAutomataRunner(
        base=args.base,
        initial_write_start=args.write_start,
        initial_write_end=args.write_end,
    )
    runner.run_simulation(
        num_timesteps=args.timesteps,
        terminal_width=args.terminal_width
    )

    # TODO: add visualize option to show tape states each step
    encoded_value = runner.read_signals_tape_value()
    print(f'{encoded_value=}')
