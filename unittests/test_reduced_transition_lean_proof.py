import unittest

from automata_builder.counter_automata import (
    CounterAutomataBuilder,
)


class TestReducedTransitionLeanProof(unittest.TestCase):
    def test_generated_lean_proofs_are_valid_for_bases_2_to_8(self) -> None:
        for base in range(2, 9):
            with self.subTest(base=base):
                proof = (
                    CounterAutomataBuilder
                    .generate_reduced_transitions_lean_proof(base=base)
                )
                self.assertTrue(
                    proof.is_valid,
                    msg=(
                        f'Generated proof obligations should all be valid '
                        f'for base {base}.'
                    )
                )
                lean_check_passed, lean_output = proof.check_in_lean()
                self.assertTrue(
                    lean_check_passed,
                    msg=(
                        f'Generated Lean proof should typecheck for base '
                        f'{base}. Lean output:\n{lean_output}'
                    )
                )
                self.assertIn(
                    f'def b : Nat := {base}',
                    proof.lean_source,
                )
                self.assertIn(
                    f'theorem {proof.theorem_name}',
                    proof.lean_source,
                )
                self.assertIn('decide', proof.lean_source)
                for obligation in proof.obligations:
                    self.assertIn(
                        f'-- {obligation.name}',
                        proof.lean_source,
                    )


if __name__ == "__main__":
    unittest.main()
