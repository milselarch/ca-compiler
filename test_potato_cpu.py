import argparse
from typing import Final

from compiler_tester import CompilerTester


DEFAULT_CHAPTERS_TO_TEST: Final[list[int]] = [1, 2]


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-c', '--chapters', nargs='+', type=int, required=False,
        default=DEFAULT_CHAPTERS_TO_TEST,
        help='List of chapter numbers to test.'
    )
    parser.add_argument(
        '--no-build', action='store_true',
        help='Force rebuild of the compiler before testing.'
    )
    parsed_args = parser.parse_args()
    # print("Parsed arguments:", parsed_args)
    build_before_test = not parsed_args.no_build

    tester = CompilerTester()
    tester.validate_potato_cpu(
        chapters_to_test=parsed_args.chapters,
        build_before_test=build_before_test
    )
