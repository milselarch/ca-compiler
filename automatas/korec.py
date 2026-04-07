import itertools

from collections import defaultdict
from typing import DefaultDict, Final
from py_ca_compiler import PyExpression, A

from automatas.rule_generator import (
    AutomataTransitionsGroup, RuleGenerator, AutomataRuleSet
)

N: Final[int] = 6
# indexed by [left_index][current_index]
RULE_MATRIX: dict[int, dict[int, int]] = {
    N: {N: N, 0: N, 1: N, 2: 1, 3: 1, 4: 2, 5: 2},
    0: {N: N, 0: 0, 1: 0, 2: 1, 3: 1, 4: 2, 5: 2},
    1: {N: 4, 0: 3, 1: 3, 2: 4, 3: 4, 4: 5, 5: 5},
    2: {N: N, 0: 0, 1: 0, 2: 1, 3: 1, 4: 2, 5: 2},
    3: {N: 4, 0: 3, 1: 3, 2: 4, 3: 4, 4: 5, 5: 5},
    4: {N: N, 0: 0, 1: 0, 2: 1, 3: 1, 4: 2, 5: 2},
    5: {N: 4, 0: 3, 1: 3, 2: 4, 3: 4, 4: 5, 5: 5},
}


class CollatzCA(object):
    @staticmethod
    def to_base(
        val: int, base: int, left_lsb: bool = True
    ) -> list[int]:
        digits = []

        while val > 0:
            digits.append(val % base)
            val //= base

        if not left_lsb:
            digits = digits[::-1]

        return digits

    @classmethod
    def print_rule_matrix(cls):
        print('pub const RULE_MATRIX: [[usize; N + 1]; N + 1] = [')

        for left_state in range(N + 1):
            row = []
            for prev_state in range(N + 1):
                next_state = RULE_MATRIX[left_state][prev_state]
                row.append(str(next_state))

            row_data = ', '.join(row)
            row_data = '[' + row_data + '],' + f' // {left_state}'
            row_data = row_data.replace(str(N), 'N')
            print('   ' + row_data)

        print(']')

    @staticmethod
    def from_base(
        digits: list[int], base: int, left_lsb: bool = True
    ) -> int:
        if not left_lsb:
            digits = digits[::-1]

        val = 0
        for k in range(len(digits)):
            val += digits[k] * base ** k

        return val

    @staticmethod
    def read_cell_state(index: int, state: list[int]) -> int:
        if index < 0 or index >= len(state):
            return N

        return state[index]

    @staticmethod
    def aggregate_bit_or(expr_list: list[PyExpression]) -> PyExpression:
        if not expr_list:
            return PyExpression()

        result = expr_list[0]
        for k in range(1, len(expr_list)):
            result = result | expr_list[k]

        return result

    @classmethod
    def build_transitions_group(cls) -> AutomataTransitionsGroup:
        num_states = N + 1
        transitions_group = AutomataTransitionsGroup.spawn_new(num_states)

        for left_state in range(num_states):
            for prev_state in range(num_states):
                next_state = RULE_MATRIX[left_state][prev_state]
                input_terms = (A(-1, left_state), A(0, prev_state))
                transitions_group.add_transition(
                    input_terms=input_terms, output_state=next_state
                )

        return transitions_group

    @classmethod
    def build_halt_transitions_group(cls) -> AutomataTransitionsGroup:
        void_state = N
        halt_state = N + 1
        num_states = N + 2
        transitions_group = AutomataTransitionsGroup.spawn_new(num_states)

        for input_states in itertools.product(range(num_states), repeat=3):
            left_state, prev_state, right_state = input_states
            input_terms = (
                A(-1, left_state), A(0, prev_state), A(1, right_state)
            )

            if halt_state in input_states:
                # halt state should propagate itself everywhere
                next_state = halt_state
            elif input_states == (void_state, 1, void_state):
                # only 1 with void neighbors should transition to halt state
                next_state = halt_state
            else:
                next_state = RULE_MATRIX[left_state][prev_state]

            transitions_group.add_transition(
                input_terms=input_terms, output_state=next_state
            )

        return transitions_group

    @classmethod
    def build_halt_transitions_group_v2(cls) -> AutomataTransitionsGroup:
        """
        create a set of transitions that includes a halt state
        that propagates itself and spawns from 1 with void neighbors
        is 10 times smaller tha the full set of transitions but
        there are undefined transition rules
        :return:
        """
        void = N
        halt = N + 1
        num_states = N + 2

        next_states_map: DefaultDict[
            int, list[tuple[A, A, A]]
        ] = defaultdict(list)

        for left_state in range(num_states):
            for prev_state in range(num_states):
                if halt in (left_state, prev_state):
                    continue

                next_state = RULE_MATRIX[left_state][prev_state]
                input_terms = (
                    A(-1, left_state), A(0, prev_state), A(0, prev_state)
                )
                next_states_map[next_state].append(input_terms)

        standard_products_length = 0
        for next_state in next_states_map:
            num_products = len(next_states_map[next_state])
            standard_products_length = max(
                standard_products_length, num_products
            )

        # void surrounded by halt states should be impossible
        dummy_invalid = (A(-1, halt), A(0, void), A(1, halt))
        # create halt state transition products
        halt_spawner = (A(-1, void), A(0, 1), A(1, void))
        halt_propagator = (A(-1, void), A(0, void), A(1, halt))
        halt_left = (A(-1, halt), A(0, void), A(0, void))
        halt_right = (A(-1, void), A(0, halt), A(0, halt))
        halt_products = [halt_left, halt_right]

        while len(halt_products) < standard_products_length:
            halt_products.append(halt_right)

        end_halt_products = [halt_spawner, halt_propagator]
        halt_products.extend(end_halt_products)

        for next_state in next_states_map:
            input_terms_list = next_states_map[next_state]
            num_products = len(input_terms_list)
            pad_length = standard_products_length - num_products
            last_product = input_terms_list[-1]

            for _ in range(pad_length):
                input_terms_list.append(last_product)
            for _ in range(2):
                input_terms_list.append(dummy_invalid)

        next_states_map[halt] = halt_products

        for next_state in next_states_map:
            assert len(next_states_map[next_state]) == (
                    standard_products_length + len(end_halt_products)
            )

            input_terms = next_states_map[next_state]
            for k in range(standard_products_length):
                product = input_terms[k]
                offsets = [term.get_position() for term in product]
                assert offsets == [-1, 0, 0]

            for k in range(2):
                product = input_terms[standard_products_length + k]
                offsets = [term.get_position() for term in product]
                assert offsets == [-1, 0, 1]

        transitions_group = AutomataTransitionsGroup.spawn_new(num_states)
        global_flat_offsets = []

        for next_state in next_states_map:
            input_terms_list = next_states_map[next_state]

            flat_offsets = []
            for product in input_terms_list:
                for term in product:
                    flat_offsets.append(term.get_position())

            if not global_flat_offsets:
                global_flat_offsets = flat_offsets
            else:
                assert global_flat_offsets == flat_offsets, (
                    f'Inconsistent flat offsets for state {next_state}: '
                    f'{flat_offsets} != {global_flat_offsets}'
                )

            for input_terms in input_terms_list:
                transitions_group.add_transition(
                    input_terms=input_terms, output_state=next_state
                )

        return transitions_group

    @classmethod
    def generate_equations(
            cls, pad_expr_length: bool = True,
            with_halt_state: bool = False, verbose: bool = False
    ) -> dict[int, PyExpression]:
        if with_halt_state:
            transitions_group = cls.build_halt_transitions_group()
        else:
            transitions_group = cls.build_transitions_group()

        rule_mappings = RuleGenerator.generate_equations(
            transitions_group,
            pad_product_length=True,
            pad_expr_length=pad_expr_length,
            verbose=verbose
        )
        return rule_mappings

    @classmethod
    def generate_ruleset(
        cls, with_halt_state: bool = False, verbose: bool = False
    ) -> AutomataRuleSet:
        if with_halt_state:
            transitions_group = cls.build_halt_transitions_group()
        else:
            transitions_group = cls.build_transitions_group()

        ruleset = RuleGenerator.to_ruleset(
            transitions_group, verbose=verbose
        )
        return ruleset

    @classmethod
    def generate_halt_ruleset(
        cls, verbose: bool = False
    ) -> AutomataRuleSet:
        transitions_group = cls.build_halt_transitions_group_v2()
        ruleset = RuleGenerator.to_ruleset(
            transitions_group, verbose=verbose
        )
        return ruleset

    @classmethod
    def generate_equations_old(
        cls, pad_expr_length: bool = True, verbose: bool = False
    ) -> dict[int, PyExpression]:
        state_eq_terms_map = {}

        for left_state in range(N+1):
            for prev_state in range(N+1):
                next_state = RULE_MATRIX[left_state][prev_state]
                if next_state not in state_eq_terms_map:
                    state_eq_terms_map[next_state] = []

                state_eq_terms_map[next_state].append(
                    A(-1, left_state) * A(0, prev_state)
                )

        state_eq_map = {
            next_state: cls.aggregate_bit_or(state_eq_terms_map[next_state])
            for next_state in state_eq_terms_map
        }
        if pad_expr_length:
            max_expr_length = max([
                len(state_eq_map[next_state]) for next_state in state_eq_map
            ])
            for next_state in state_eq_map:
                start_expr_length = len(state_eq_map[next_state])
                pad_length = max_expr_length - start_expr_length
                end_product = state_eq_map[next_state][start_expr_length - 1]

                for _ in range(pad_length):
                    state_eq_map[next_state] |= end_product

        sorted_states = sorted(list(state_eq_map.keys()))
        if verbose:
            for next_state in sorted_states:
                print(f'{next_state} -> {state_eq_map[next_state]}')

        """
        state_pairs_iterator = itertools.product(sorted_states, sorted_states)
        for next_state, state in state_pairs_iterator:
            expr = state_eq_map[next_state]
        """
        return state_eq_map

    def get_next_cell_state(self, index: int, state: list[int]) -> int:
        left_index = index - 1
        left_state = self.read_cell_state(left_index, state)
        current_state = self.read_cell_state(index, state)
        return RULE_MATRIX[left_state][current_state]

    @staticmethod
    def apply_collatz(num: int) -> int:
        if not num % 2:
            return num // 2
        else:
            return 3 * num + 1

    @classmethod
    def run_collatz(cls, num: int) -> int:
        steps = 0
        while num > 1:
            steps += 1
            num = cls.apply_collatz(num)

        return steps

    @classmethod
    def run_collatz_sum(cls, num: int) -> int:
        """
        Sum all numbers encountered in the Collatz sequence
        from num down to 1
        :param num:
        :return:
        """
        steps = 0
        total = 0

        while num > 1:
            steps += 1
            total += num
            num = cls.apply_collatz(num)

        return total

    @classmethod
    def number_to_base(
        cls, number: int, base: int,
        msb_first: bool = True
    ) -> list[int]:
        # https://stackoverflow.com/a/28666223/2451130
        if number == 0:
            return [0]

        digits = []
        while number:
            digits.append(int(number % base))
            number //= base

        if msb_first:
            digits = digits[::-1]

        return digits

    @classmethod
    def count_collatz_steps_till_base_fall(
        cls, num: int, base: int = 6
    ):
        """
        Count the number of collatz steps till the number
        falls to value with a lower number of digits in the
        specified base
        """
        digits = cls.number_to_base(num, base, msb_first=False)
        initial_num_digits = len(digits)

        max_target_number = 1
        if initial_num_digits > 0:
            max_target_number = 6 ** (initial_num_digits - 1)

        time_steps = 0
        while num > max_target_number:
            num = cls.apply_collatz(num)
            time_steps += 1

        # print('FALL', num, time_steps)
        return time_steps

    @classmethod
    def get_max_collatz_steps(
        cls, start: int = 1, max_num: int = 100000,
        verbose: bool = False
    ):
        max_ratio = 0
        max_steps = 0

        def print_verbose_info(*args, **kwargs):
            if verbose:
                print(*args, **kwargs)

        for num in range(start, max_num):
            steps = cls.run_collatz(num)
            max_steps = max(max_steps, steps)
            ratio = steps / num
            print_verbose_info(f'NUM {num} STEPS {steps} RATIO {ratio}')

            if ratio > max_ratio:
                max_ratio = ratio
                print(f'NEW MAX RATIO FOR {num} = {ratio}')

        return max_steps

    def run_ca(self, num: int, max_steps: int = 1000, terminate_at: int = 1):
        next_num = num
        initial_state = self.to_base(num, base=6, left_lsb=False)
        state = initial_state.copy()
        time_steps = 0

        while time_steps < max_steps:
            int_value = self.from_base(digits=state, base=6, left_lsb=False)
            assert int_value == next_num, f'{int_value} != {next_num}'
            print(f'STEP {time_steps} = {int_value} {state}')
            if int_value == terminate_at:
                break

            new_state = []
            for k in range(len(state) + 1):
                next_state = self.get_next_cell_state(k, state)
                if next_state is N:
                    continue

                new_state.append(next_state)

            next_num = self.apply_collatz(next_num)
            state = new_state
            time_steps += 1

        return time_steps


if __name__ == '__main__':
    collatz_ca = CollatzCA()
    # steps = collatz_ca.run_ca(837799, terminate_at=1)
    # print(f'steps = {steps}')
    collatz_ca.generate_equations(verbose=True)
    # collatz_ca.get_max_collatz_steps(start=200)
    collatz_ca.build_halt_transitions_group_v2()
    print(collatz_ca.print_rule_matrix())
