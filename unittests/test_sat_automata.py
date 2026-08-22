import unittest

from automata_builder.sat_automata import (
    ASSIGNMENTS_TAPE,
    CONSTRAINTS_TAPE,
    VERDICT_SAT,
    VERDICT_UNSAT,
    ThreeSATAutomataRunner,
)


class TestThreeSATAutomata(unittest.TestCase):
    def test_satisfiable_equation_assignment_pair(self):
        runner = ThreeSATAutomataRunner(
            equation='(x1|~x2|x3)&(~x1|x2|x3)',
            assignment='x1=1,x2=0,x3=1',
        )
        runner.run_simulation(num_timesteps=3, render=False)
        self.assertEqual(runner.read_verdict_state(), VERDICT_SAT)
        self.assertTrue(runner.is_satisfiable())

    def test_unsatisfiable_equation_assignment_pair(self):
        runner = ThreeSATAutomataRunner(
            equation='(x1|x2|x3)&(~x1|~x2|~x3)',
            assignment='x1=1,x2=1,x3=1',
        )
        runner.run_simulation(num_timesteps=3, render=False)
        self.assertEqual(runner.read_verdict_state(), VERDICT_UNSAT)
        self.assertFalse(runner.is_satisfiable())

    def test_invalid_equation_is_unsat(self):
        runner = ThreeSATAutomataRunner(
            equation='(x1|x2)&(x3|~x4|x5)',
            assignment='x1=1,x2=1,x3=0,x4=0,x5=1',
        )
        runner.run_simulation(num_timesteps=2, render=False)
        self.assertEqual(runner.read_verdict_state(), VERDICT_UNSAT)

    def test_invalid_assignment_is_unsat(self):
        runner = ThreeSATAutomataRunner(
            equation='(x1|x2|x3)',
            assignment='x1=1,x2=maybe,x3=0',
        )
        runner.run_simulation(num_timesteps=2, render=False)
        self.assertEqual(runner.read_verdict_state(), VERDICT_UNSAT)

    def test_missing_variable_assignment_is_unsat(self):
        runner = ThreeSATAutomataRunner(
            equation='(x1|x2|x3)',
            assignment='x1=1,x2=0',
        )
        runner.run_simulation(num_timesteps=2, render=False)
        self.assertEqual(runner.read_verdict_state(), VERDICT_UNSAT)

    def test_constraints_and_assignments_are_on_separate_tapes(self):
        runner = ThreeSATAutomataRunner(
            equation='(x1|~x2|x3)&(x1|x2|~x3)',
            assignment='x1=1,x2=0,x3=1',
        )

        constraints_region = runner.multi_tape_automata[
            CONSTRAINTS_TAPE
        ].get_minimal_data_region()
        assignments_region = runner.multi_tape_automata[
            ASSIGNMENTS_TAPE
        ].get_minimal_data_region()

        self.assertEqual(len(constraints_region), 2)
        self.assertEqual(len(assignments_region), 3)


if __name__ == '__main__':
    unittest.main()
