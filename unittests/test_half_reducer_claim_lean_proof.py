import shutil
import unittest

from automata_builder.counter_automata import CounterAutomataBuilder


class TestHalfReducerClaimLeanProof(unittest.TestCase):
    def test_generated_base_2_claim_contains_expected_sections(self) -> None:
        proof = (
            CounterAutomataBuilder
            .generate_half_reducer_claim_lean_proof_base_2()
        )

        self.assertEqual(proof.base, 2)
        self.assertEqual(
            proof.theorem_name,
            'half_reducer_base_2_convergence_claim',
        )
        self.assertIn('def b : Nat := 2', proof.lean_source)
        self.assertIn(
            f'theorem {proof.theorem_name}',
            proof.lean_source,
        )
        for assumption in proof.assumptions:
            self.assertIn(assumption, proof.lean_source)

    def test_generated_base_2_claim_typechecks_in_lean_when_available(
        self
    ) -> None:
        if shutil.which('lean') is None:
            self.skipTest('lean executable was not found in PATH.')

        proof = (
            CounterAutomataBuilder
            .generate_half_reducer_claim_lean_proof_base_2()
        )
        lean_check_passed, lean_output = proof.check_in_lean()
        self.assertTrue(
            lean_check_passed,
            msg=(
                'Generated Lean theorem should typecheck. '
                f'Lean output:\n{lean_output}'
            ),
        )


if __name__ == "__main__":
    unittest.main()
