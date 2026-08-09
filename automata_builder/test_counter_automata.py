import argparse

from automata_builder.rule_generator_multitape import BLANK_INT
from automata_builder.counter_automata import CounterAutomataRunner

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
    '--apply-reduction', '-a',
    action='store_true',
    help='Use a automata ruleset with half-reduction'
)
parser.add_argument(
    '--no-render', '-n',
    action='store_true',
    help='Set to not render automata states to terminal'
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

if __name__ == '__main__':
    """
    Run the counter automata simulation with specified parameters.
    
    Examples:
    python -m automata_builder.test_counter_automata --base 8 \
        --write-start -78 --write-end -3 --timesteps 304
    >>> end encoded value = 76
    ==========================================
    python -m automata_builder.test_counter_automata --base 8 \
        --write-start -78 --write-end -3 --timesteps 304 -a
    >>> end encoded value = 38
    ==========================================
    python -m automata_builder.test_counter_automata --base 2 \
        --write-start 0 -a -n --write-end 0
    >>> end encoded value = 1
    
    The half-reducer automata is designed to transform t unary data cells 
    into encoded cells with a value of (x+1)//2 in base n and in 2*t time, 
    and such that the encoded n-ary is contained with the same 
    position range as the original data cells 
    
    (and in fact the left end of the encoded n-ary is at the same 
    position as the left end of the original unary data cell range)
    """
    args = parser.parse_args()
    timesteps = args.timesteps
    render = not args.no_render

    if timesteps == BLANK_INT:
        # write range is inclusive, hence the +1
        cells_filled = args.write_end - args.write_start + 1
        timesteps = cells_filled * 2
        print(f'using default {timesteps=}')

    runner = CounterAutomataRunner(
        base=args.base,
        initial_write_start=args.write_start,
        initial_write_end=args.write_end,
        apply_reduction=args.apply_reduction
    )
    runner.run_simulation(
        num_timesteps=timesteps,
        terminal_width=args.terminal_width,
        render=render
    )

    # TODO: add visualize option to show tape states each step
    num_transitions = len(runner.transitions_group)
    print(f'Created automata with {num_transitions} transitions')
    encoded_value = runner.read_signals_tape_value()
    print(f'End encoded value = {encoded_value}')
