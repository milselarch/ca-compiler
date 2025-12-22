import subprocess

from enum import StrEnum
from tqdm import tqdm
from typing import Final
from py_ca_compiler import PyPotatoCPUTester


COMPILER_TESTS_DIR: Final[str] = './writing-a-c-compiler-tests'


class Prefix(StrEnum):
    invalid_lex = 'invalid_lex'
    invalid_parse = 'invalid_parse'
    valid = 'valid'


class CompilerTester(object):
    def __init__(self):
        self.built = False

    def build(self, force_rebuild: bool = False) -> int:
        if self.built and not force_rebuild:
            return 0

        build_result = subprocess.run(
            ["cargo", "build", "--release"], capture_output=True, text=True
        )
        print("Build stdout:", build_result.stdout)
        print("Build stderr:", build_result.stderr)
        return_code = build_result.returncode

        if return_code != 0:
            raise RuntimeError("Failed to build the compiler.")

        self.built = True
        return return_code

    @classmethod
    def get_test_dir(
        cls, chapter_no: int, prefix: Prefix = Prefix.valid
    ) -> str:
        return (
            f'{COMPILER_TESTS_DIR}/tests/chapter_{chapter_no}/{prefix}'
        )

    @classmethod
    def get_test_path(
        cls, test_name: str, chapter_no: int, prefix: Prefix = Prefix.valid
    ) -> str:
        return cls.get_test_dir(chapter_no, prefix) + f'/{test_name}'

    def execute_test_x86(self, test_name: str, chapter_no: int) -> int:
        test_path = self.get_test_path(test_name, chapter_no)
        # build the C test file to assembly using the compiler
        run_result = subprocess.run([
            "./target/release/ca-compiler", test_path
        ], capture_output=True, text=True)
        assert run_result.returncode == 0

        asm_path = test_path.replace('.c', '.s')
        exe_path = test_path.replace('.c', '')

        # create the executable
        gcc_result = subprocess.run(
            ["gcc", asm_path, "-o", exe_path],
            capture_output=True, text=True
        )
        if gcc_result.returncode != 0:
            print("GCC stdout:", gcc_result.stdout)
            print("GCC stderr:", gcc_result.stderr)
            raise RuntimeError("Failed to assemble and link the test.")

        # Get executable return code
        execute_result = subprocess.run(
            [exe_path], capture_output=True, text=True
        )
        return execute_result.returncode

    def execute_test_potato_cpu(
        self, test_name: str, chapter_no: int, prefix: Prefix = Prefix.valid
    ) -> int:
        test_path = self.get_test_path(test_name, chapter_no, prefix)
        print('Compiling test for Potato CPU:', test_path)
        potato_program = PyPotatoCPUTester.compile_from_source(test_path)
        result = potato_program.execute()
        return result

    def count_total_tests(self, chapters: list[int]) -> int:
        total_tests = 0
        for chapter_no in chapters:
            valid_tests = self.list_valid_tests(chapter_no)
            total_tests += len(valid_tests)

        return total_tests

    @classmethod
    def list_valid_tests(
        cls, chapter_no: int, prefix: Prefix = Prefix.valid
    ) -> list[str]:
        test_dir = cls.get_test_dir(chapter_no, prefix)
        ls_result = subprocess.run(
            ["ls", test_dir], capture_output=True, text=True
        )
        if ls_result.returncode != 0:
            raise RuntimeError(f"Failed to list tests for {repr(test_dir)}")

        test_files = ls_result.stdout.strip().split('\n')
        c_test_files = [f for f in test_files if f.endswith('.c')]
        return c_test_files

    @classmethod
    def validate_potato_cpu(
        cls, chapters_to_test: list[int], build_before_test: bool = True
    ) -> None:
        """
        Check that program output for valid c programs is the same
        when targeting both x86 and Potato CPU.
        :param chapters_to_test:
        :param build_before_test:
        :return:
        """
        tester = CompilerTester()
        total_tests = tester.count_total_tests(chapters_to_test)
        pbar = tqdm(total=total_tests)

        if build_before_test:
            tester.build(force_rebuild=False)
        else:
            print('Skipping compiler build before tests.')

        for chapter in chapters_to_test:
            print(f'Running Potato CPU tests for Chapter {chapter}...')
            valid_tests = tester.list_valid_tests(chapter)
            for test_file in valid_tests:
                pbar.set_description(
                    f'  Executing test [{chapter}]: {test_file} ... '
                )
                x86_return_code = tester.execute_test_x86(test_file, chapter)
                potato_cpu_return_code = tester.execute_test_potato_cpu(
                    test_file, chapter
                )
                print(
                    f'    x86 return code: {x86_return_code}, '
                    f'Potato CPU return code: {potato_cpu_return_code}'
                )
                if x86_return_code != potato_cpu_return_code:
                    raise ValueError(
                        f'FAILED (x86: {x86_return_code}, '
                        f'Potato CPU: {potato_cpu_return_code})'
                    )

                pbar.update(1)
