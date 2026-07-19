import unittest

from automata_builder.counter_automata import CounterAutomataRunner


class TestCounterAutomataConvergence(unittest.TestCase):
    @staticmethod
    def _run_steps_and_read(
        base: int,
        write_start: int,
        write_end: int,
        steps: int,
    ) -> int:
        runner = CounterAutomataRunner(
            base=base,
            initial_write_start=write_start,
            initial_write_end=write_end,
        )
        runner.run_simulation(num_timesteps=steps)
        return runner.read_signals_tape_value()

    def test_encoded_value_equals_cells_filled_after_2n_steps(self) -> None:
        # Check several ranges and bases to avoid overfitting one setup.
        scenarios = [
            # (base, write_start, write_end)
            (2, 0, 0),    # 1 cell
            (2, 0, 1),    # 2 cells
            (2, 0, 5),    # 6 cells
            (3, -2, 2),   # 5 cells
            (6, 0, 10),   # 11 cells
            (8, 3, 12),   # 10 cells
        ]

        for base, write_start, write_end in scenarios:
            cells_filled = write_end - write_start + 1
            min_steps = cells_filled * 4

            # Validate exactly at 2*n and a few larger timesteps.
            for steps in (min_steps, min_steps + 1, min_steps + 5):
                with self.subTest(
                    base=base,
                    write_start=write_start,
                    write_end=write_end,
                    cells_filled=cells_filled,
                    steps=steps,
                ):
                    encoded = self._run_steps_and_read(
                        base=base,
                        write_start=write_start,
                        write_end=write_end,
                        steps=steps,
                    )
                    self.assertEqual(
                        encoded,
                        cells_filled,
                        msg=(
                            f"Expected encoded value {cells_filled} "
                            f"after {steps} steps "
                            f"(>= 2*cells_filled={min_steps}), got {encoded}."
                        ),
                    )


if __name__ == "__main__":
    unittest.main()
