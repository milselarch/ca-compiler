import argparse

from automata_builder.counter_automata import (
    build_st_counter_state,
    from_counter_state,
)


def test_counter_digit_encoding(max_num: int):
    # Treat build_st_counter_state as "to_counter_state".
    for digit in range(0, max_num):
        for paused in (False, True):
            encoded = build_st_counter_state(digit, paused)
            decoded_digit, decoded_paused = from_counter_state(encoded)
            print(f'digit {digit}:{paused} -> {encoded}')

            assert decoded_digit == digit
            assert decoded_paused == paused

            # Round-trip back to the same encoded value.
            re_encoded = build_st_counter_state(
                decoded_digit, decoded_paused
            )
            assert re_encoded == encoded


parser = argparse.ArgumentParser(
    description='Run the counter automata simulation.'
)
parser.add_argument(
    '--max-num', '-m',
    type=int,
    default=10,
    help='Max number to test (default: 10)'
)

args = parser.parse_args()
test_counter_digit_encoding(args.max_num)
