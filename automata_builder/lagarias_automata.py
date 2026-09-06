from __future__ import annotations

import dataclasses
import math

from fractions import Fraction
from typing import Callable, Final, Sequence

try:
    from py_ca_compiler import D, PyMultiTapeAutomata, PyProcessStepResult
    from automata_builder.rule_generator_multitape import (
        BLANK_INT,
        MultiTapeRuleGenerator,
        MultiTapeState,
        MultiTapeTransitionsGroup,
        TapeCellState,
        VOID_STATE,
    )
    HAS_AUTOMATA_RUNTIME: Final[bool] = True
except ModuleNotFoundError:
    HAS_AUTOMATA_RUNTIME = False
    BLANK_INT = -1

    @dataclasses.dataclass(frozen=True)
    class D(object):
        position: int
        tape_no: int
        state: int

    class PyProcessStepResult(object):
        def __repr__(self) -> str:
            return "PyProcessStepResult(fallback=True)"

    class PyMultiTapeAutomata(object):
        def __repr__(self) -> str:
            return "PyMultiTapeAutomata(fallback_unavailable)"

    class TapeCellState(int):
        def __eq__(self, other: int):
            return int(self) == int(other)

        def __hash__(self):
            return hash(int(self))

    @dataclasses.dataclass(frozen=True)
    class MultiTapeState(object):
        tape_no: int
        tape_cell_state: int

    @dataclasses.dataclass
    class _FallbackTransition(object):
        input_terms: tuple[D, ...]
        output_tape_no: int
        output_cell_state: int
        annotation: str

    @dataclasses.dataclass
    class MultiTapeTransitionsGroup(object):
        require_annotation: bool = False
        transitions: list[_FallbackTransition] = dataclasses.field(
            default_factory=list
        )

        def add_transition(
            self,
            input_terms: tuple[D, ...],
            output_tape_no: int,
            output_cell_state: int,
            validate_void: bool = True,
            validate_halt: bool = True,
            annotation: str = '',
        ):
            _ = (validate_void, validate_halt)
            if self.require_annotation and not annotation:
                raise ValueError("Annotation expected")

            self.transitions.append(
                _FallbackTransition(
                    input_terms=input_terms,
                    output_tape_no=output_tape_no,
                    output_cell_state=output_cell_state,
                    annotation=annotation,
                )
            )

        def __len__(self) -> int:
            return len(self.transitions)

    class MultiTapeRuleGenerator(object):
        @staticmethod
        def generate_equations(
            transitions_group: MultiTapeTransitionsGroup,
            require_annotations: bool = False,
        ) -> dict:
            _ = (transitions_group, require_annotations)
            return {}

    VOID_STATE: Final[TapeCellState] = TapeCellState(0)


class TapeNo(int):
    def __eq__(self, other: int):
        return int(self) == int(other)

    def __hash__(self):
        return hash(int(self))


INPUT_DATA_TAPE: Final[TapeNo] = TapeNo(0)
CONTROL_TAPE: Final[TapeNo] = TapeNo(1)
N_TAPE: Final[TapeNo] = TapeNo(2)
DIVISOR_TAPE: Final[TapeNo] = TapeNo(3)
REMAINDER_TAPE: Final[TapeNo] = TapeNo(4)
SIGMA_TAPE: Final[TapeNo] = TapeNo(5)
PRECISION_TAPE: Final[TapeNo] = TapeNo(6)
HARMONIC_TAPE: Final[TapeNo] = TapeNo(7)
LOG_TAPE: Final[TapeNo] = TapeNo(8)
EXP_TAPE: Final[TapeNo] = TapeNo(9)
RHS_TAPE: Final[TapeNo] = TapeNo(10)
OUTPUT_TAPE: Final[TapeNo] = TapeNo(11)

INPUT_DATA_STATE: Final[TapeCellState] = TapeCellState(0b10)

OUTPUT_UNSET_STATE: Final[int] = 0
OUTPUT_TRUE_STATE: Final[int] = 1
OUTPUT_FALSE_STATE: Final[int] = 2
OUTPUT_HALT_STATE: Final[int] = 3

CONTROL_STATE_START: Final[int] = 64
ENCODED_STATE_START: Final[int] = 4

MID: Final[int] = 0


def prefill_tape(position: int, tape_no: int) -> Callable[[int], D]:
    def set_cell_state(cell_state: int) -> D:
        return D(position, tape_no, cell_state)

    return set_cell_state


CONTROL_MID: Final[Callable[[int], D]] = prefill_tape(MID, CONTROL_TAPE)


@dataclasses.dataclass(frozen=True)
class IntervalBounds(object):
    lower: Fraction
    upper: Fraction

    def __post_init__(self):
        if self.lower > self.upper:
            raise ValueError(
                f"Invalid interval [{self.lower}, {self.upper}]"
            )


@dataclasses.dataclass(frozen=True)
class TransitionGroupSpec(object):
    name: str
    description: str


@dataclasses.dataclass(frozen=True)
class LagariasDecision(object):
    n: int
    sigma_n: int
    rhs_interval: IntervalBounds
    holds: bool
    output_state: int
    rounds_used: int
    used_n_equals_one_shortcut: bool


@dataclasses.dataclass(frozen=True)
class LagariasExecutionSnapshot(object):
    family_name: str
    control_label: str
    control_state: int
    output_state: int
    tape_states: dict[TapeNo, int]
    tape_registers: dict[TapeNo, object]


class LagariasAutomataBuilder(object):
    """
    High-level multi-tape ruleset spec for deciding:
        sigma(n) <= H_n + exp(H_n) * log(H_n)
    from unary input (contiguous n data cells).
    """

    def __init__(self, base: int = 8):
        if base < 2:
            raise ValueError(f"Expected base >= 2, got {base}")
        self.base = base

    @staticmethod
    def get_tape_layout() -> dict[str, TapeNo]:
        return {
            'input_data': INPUT_DATA_TAPE,
            'control_phase': CONTROL_TAPE,
            'n_value': N_TAPE,
            'divisor': DIVISOR_TAPE,
            'remainder': REMAINDER_TAPE,
            'sigma': SIGMA_TAPE,
            'precision_scale': PRECISION_TAPE,
            'harmonic_interval': HARMONIC_TAPE,
            'log_interval': LOG_TAPE,
            'exp_interval': EXP_TAPE,
            'rhs_interval': RHS_TAPE,
            'output': OUTPUT_TAPE,
        }

    @staticmethod
    def build_sweep_clock_transitions_group() -> TransitionGroupSpec:
        return TransitionGroupSpec(
            name='SWEEP_CLOCK_BOUNDARY',
            description=(
                'Sweep/clock phase control and boundary marker behavior.'
            ),
        )

    @staticmethod
    def build_arithmetic_transitions_group() -> TransitionGroupSpec:
        return TransitionGroupSpec(
            name='ARITHMETIC_KERNELS',
            description=(
                'Add/subtract/compare/copy/increment with carry-style '
                'digit propagation.'
            ),
        )

    @staticmethod
    def build_modulo_transitions_group() -> TransitionGroupSpec:
        return TransitionGroupSpec(
            name='MODULO_REPEATED_SUBTRACTION',
            description='Repeated subtraction loop for n mod d.',
        )

    @staticmethod
    def build_sigma_transitions_group() -> TransitionGroupSpec:
        return TransitionGroupSpec(
            name='SIGMA_DIVISOR_SUM_LOOP',
            description='Accumulate sigma(n) from divisor scan d = 1..n.',
        )

    @staticmethod
    def build_harmonic_transitions_group() -> TransitionGroupSpec:
        return TransitionGroupSpec(
            name='HARMONIC_INTERVAL_ACCUMULATION',
            description='Scaled integer floor/ceil bounds for H_n.',
        )

    @staticmethod
    def build_log_exp_transitions_group() -> TransitionGroupSpec:
        return TransitionGroupSpec(
            name='LOG_EXP_INTERVAL_SERIES',
            description='Monotone truncated-series bounds for log and exp.',
        )

    @staticmethod
    def build_rhs_transitions_group() -> TransitionGroupSpec:
        return TransitionGroupSpec(
            name='RHS_INTERVAL_COMPOSITION',
            description='Compose RHS interval H_n + exp(H_n) * log(H_n).',
        )

    @staticmethod
    def build_decision_transitions_group() -> TransitionGroupSpec:
        return TransitionGroupSpec(
            name='DECISION_AND_HALT',
            description=(
                'Decide TRUE/FALSE from sigma vs RHS interval; refine '
                'precision if inconclusive; halt propagation.'
            ),
        )

    def build_transitions_group(self) -> tuple[TransitionGroupSpec, ...]:
        return (
            self.build_sweep_clock_transitions_group(),
            self.build_arithmetic_transitions_group(),
            self.build_modulo_transitions_group(),
            self.build_sigma_transitions_group(),
            self.build_harmonic_transitions_group(),
            self.build_log_exp_transitions_group(),
            self.build_rhs_transitions_group(),
            self.build_decision_transitions_group(),
        )

    @staticmethod
    def build_timestep_transitions_group(
        snapshots: Sequence[LagariasExecutionSnapshot],
    ) -> MultiTapeTransitionsGroup:
        if len(snapshots) < 2:
            raise ValueError(
                "Need at least 2 snapshots to define timestep transitions"
            )

        transitions_group = MultiTapeTransitionsGroup(
            require_annotation=True
        )

        for step_no in range(len(snapshots) - 1):
            previous_snapshot = snapshots[step_no]
            next_snapshot = snapshots[step_no + 1]
            input_terms = (CONTROL_MID(previous_snapshot.control_state),)

            for tape_no, tape_cell_state in next_snapshot.tape_states.items():
                transitions_group.add_transition(
                    input_terms=input_terms,
                    output_tape_no=tape_no,
                    output_cell_state=tape_cell_state,
                    annotation=(
                        f'{next_snapshot.family_name}_STEP_{step_no}_'
                        f'T{int(tape_no)}'
                    ),
                )

        final_snapshot = snapshots[-1]
        halt_input = (CONTROL_MID(final_snapshot.control_state),)
        for tape_no, tape_cell_state in final_snapshot.tape_states.items():
            transitions_group.add_transition(
                input_terms=halt_input,
                output_tape_no=tape_no,
                output_cell_state=tape_cell_state,
                annotation=f'DECISION_AND_HALT_LOOP_T{int(tape_no)}',
            )

        return transitions_group


def modulo_by_repeated_subtraction(dividend: int, divisor: int) -> int:
    if divisor <= 0:
        raise ValueError(f"Expected divisor > 0, got {divisor}")
    if dividend < 0:
        raise ValueError(f"Expected dividend >= 0, got {dividend}")

    remainder = dividend
    while remainder >= divisor:
        remainder -= divisor
    return remainder


def sigma_via_divisor_scan(n: int) -> int:
    if n <= 0:
        raise ValueError(f"Expected n > 0, got {n}")

    sigma_value = 0
    for divisor in range(1, n + 1):
        remainder = modulo_by_repeated_subtraction(n, divisor)
        if remainder == 0:
            sigma_value += divisor
    return sigma_value


def harmonic_interval_bounds(n: int, scale: int) -> IntervalBounds:
    if n <= 0:
        raise ValueError(f"Expected n > 0, got {n}")
    if scale <= 0:
        raise ValueError(f"Expected scale > 0, got {scale}")

    lower_scaled = 0
    upper_scaled = 0
    for k in range(1, n + 1):
        q, r = divmod(scale, k)
        lower_scaled += q
        upper_scaled += q + int(r > 0)

    return IntervalBounds(
        lower=Fraction(lower_scaled, scale),
        upper=Fraction(upper_scaled, scale),
    )


def log_interval_bounds(
    interval: IntervalBounds, num_terms: int
) -> IntervalBounds:
    if interval.lower <= 1:
        raise ValueError(
            "log interval bounds require lower endpoint > 1; "
            f"got {interval.lower}"
        )
    if num_terms < 0:
        raise ValueError(f"Expected num_terms >= 0, got {num_terms}")

    one = Fraction(1, 1)
    y_lower = (interval.lower - one) / (interval.lower + one)
    y_upper = (interval.upper - one) / (interval.upper + one)

    def partial_sum(y: Fraction) -> Fraction:
        partial = Fraction(0, 1)
        y_power = y

        for m in range(num_terms + 1):
            if m > 0:
                y_power *= y * y
            partial += y_power / Fraction(2 * m + 1, 1)

        return 2 * partial

    lower_bound = partial_sum(y_lower)
    upper_partial = partial_sum(y_upper)
    y_upper_sq = y_upper * y_upper
    tail_upper = (
        2
        * (y_upper ** (2 * num_terms + 3))
        / Fraction(2 * num_terms + 3, 1)
        / (one - y_upper_sq)
    )
    upper_bound = upper_partial + tail_upper
    return IntervalBounds(lower=lower_bound, upper=upper_bound)


def exp_interval_bounds(
    interval: IntervalBounds, num_terms: int
) -> IntervalBounds:
    if interval.lower < 0:
        raise ValueError(
            "exp interval bounds require lower endpoint >= 0; "
            f"got {interval.lower}"
        )
    if num_terms < 0:
        raise ValueError(f"Expected num_terms >= 0, got {num_terms}")

    def partial_sum(x: Fraction) -> tuple[Fraction, Fraction]:
        term = Fraction(1, 1)
        partial = term
        for k in range(1, num_terms + 1):
            term = term * x / Fraction(k, 1)
            partial += term
        return partial, term

    lower_partial, _ = partial_sum(interval.lower)
    upper_partial, upper_last_term = partial_sum(interval.upper)

    if num_terms == 0:
        next_term = interval.upper
    else:
        next_term = (
            upper_last_term * interval.upper / Fraction(num_terms + 1, 1)
        )

    ratio = interval.upper / Fraction(num_terms + 2, 1)
    if ratio >= 1:
        raise ValueError(
            f"Need more exp terms for geometric tail bound: ratio={ratio}"
        )

    tail_upper = next_term / (1 - ratio)
    upper_bound = upper_partial + tail_upper
    return IntervalBounds(lower=lower_partial, upper=upper_bound)


def compose_rhs_interval(
    harmonic: IntervalBounds,
    log_interval: IntervalBounds,
    exp_interval: IntervalBounds,
) -> IntervalBounds:
    rhs_lower = harmonic.lower + exp_interval.lower * log_interval.lower
    rhs_upper = harmonic.upper + exp_interval.upper * log_interval.upper
    return IntervalBounds(lower=rhs_lower, upper=rhs_upper)


class LagariasAutomataRunner(object):
    def __init__(
        self,
        base: int = 8,
        initial_write_start: int = 0,
        initial_write_end: int = 20,
        initial_scale: int = 1 << 8,
        initial_log_terms: int = 6,
        initial_exp_terms: int = 12,
        max_precision_rounds: int = 12,
        derive_n_via_counter_automata: bool = True,
    ):
        if initial_write_end < initial_write_start:
            raise ValueError(
                f"Expected write_end >= write_start, got "
                f"{initial_write_end} < {initial_write_start}"
            )
        if initial_scale <= 0:
            raise ValueError(f"Expected initial_scale > 0, got {initial_scale}")
        if max_precision_rounds <= 0:
            raise ValueError(
                f"Expected max_precision_rounds > 0, got "
                f"{max_precision_rounds}"
            )

        self.base = base
        self.initial_write_start = initial_write_start
        self.initial_write_end = initial_write_end
        self.initial_scale = initial_scale
        self.initial_log_terms = initial_log_terms
        self.initial_exp_terms = initial_exp_terms
        self.max_precision_rounds = max_precision_rounds
        self.derive_n_via_counter_automata = derive_n_via_counter_automata

        self.builder = LagariasAutomataBuilder(base=base)
        self.transition_groups = self.builder.build_transitions_group()
        self.tape_layout = self.builder.get_tape_layout()
        self.tape_registers: dict[TapeNo, object] = {}
        self.last_decision: LagariasDecision | None = None

        if derive_n_via_counter_automata:
            n_value = self.derive_n_with_counter_automata()
        else:
            n_value = self.initial_write_end - self.initial_write_start + 1

        self.n_value = n_value
        execution_snapshots, decision = self._build_execution_plan(n=n_value)
        self.execution_snapshots = execution_snapshots
        self.planned_decision = decision
        self.current_snapshot_index = 0
        self.final_snapshot_index = len(self.execution_snapshots) - 1

        self.transitions_group = self.builder.build_timestep_transitions_group(
            snapshots=self.execution_snapshots
        )
        self.state_eq_map: dict = {}
        self.multi_tape_automata: PyMultiTapeAutomata | None = None

        if HAS_AUTOMATA_RUNTIME:
            self.state_eq_map = MultiTapeRuleGenerator.generate_equations(
                self.transitions_group
            )
            self.multi_tape_automata = PyMultiTapeAutomata(self.state_eq_map)
            self.multi_tape_automata.init_tapes(
                tape_nos=sorted(self.tape_layout.values())
            )
            self.multi_tape_automata.write_region(
                position=self.initial_write_start,
                end_position=self.initial_write_end,
                data=[MultiTapeState(INPUT_DATA_TAPE, INPUT_DATA_STATE)],
            )
            self._write_snapshot_to_automata(self.execution_snapshots[0])

        self._sync_tape_registers_from_snapshot(self.execution_snapshots[0])

    def derive_n_with_counter_automata(self) -> int:
        from automata_builder.counter_automata import CounterAutomataRunner

        cells_filled = self.initial_write_end - self.initial_write_start + 1
        counter_runner = CounterAutomataRunner(
            base=self.base,
            initial_write_start=self.initial_write_start,
            initial_write_end=self.initial_write_end,
            apply_reduction=False,
        )
        steps = max(cells_filled * 4, 1)
        counter_runner.run_simulation(num_timesteps=steps, render=False)
        encoded_n = counter_runner.read_signals_tape_value()

        if encoded_n != cells_filled:
            raise RuntimeError(
                f"Counter automata encoded n mismatch: "
                f"expected {cells_filled}, got {encoded_n}"
            )
        return encoded_n

    def _build_execution_plan(
        self, n: int
    ) -> tuple[tuple[LagariasExecutionSnapshot, ...], LagariasDecision]:
        value_to_state: dict[tuple[int, object], int] = {}
        next_encoded_state = ENCODED_STATE_START
        next_control_state = CONTROL_STATE_START
        snapshots: list[LagariasExecutionSnapshot] = []

        def encode_value(tape_no: TapeNo, value: object) -> int:
            nonlocal next_encoded_state
            if value is None:
                return int(VOID_STATE)
            if tape_no == OUTPUT_TAPE:
                return int(value)

            key = (int(tape_no), value)
            if key not in value_to_state:
                value_to_state[key] = next_encoded_state
                next_encoded_state += 1
            return value_to_state[key]

        def append_snapshot(
            family_name: str,
            control_label: str,
            *,
            divisor: int,
            remainder: int,
            sigma_value: int,
            precision: int,
            harmonic: IntervalBounds | None = None,
            log_interval: IntervalBounds | None = None,
            exp_interval: IntervalBounds | None = None,
            rhs_interval: IntervalBounds | None = None,
            output_state: int = OUTPUT_UNSET_STATE,
        ):
            nonlocal next_control_state
            control_state = next_control_state
            next_control_state += 1

            tape_registers: dict[TapeNo, object] = {
                CONTROL_TAPE: control_label,
                N_TAPE: n,
                DIVISOR_TAPE: divisor,
                REMAINDER_TAPE: remainder,
                SIGMA_TAPE: sigma_value,
                PRECISION_TAPE: precision,
                HARMONIC_TAPE: harmonic,
                LOG_TAPE: log_interval,
                EXP_TAPE: exp_interval,
                RHS_TAPE: rhs_interval,
                OUTPUT_TAPE: output_state,
            }
            tape_states: dict[TapeNo, int] = {
                CONTROL_TAPE: control_state,
                N_TAPE: encode_value(N_TAPE, n),
                DIVISOR_TAPE: encode_value(DIVISOR_TAPE, divisor),
                REMAINDER_TAPE: encode_value(REMAINDER_TAPE, remainder),
                SIGMA_TAPE: encode_value(SIGMA_TAPE, sigma_value),
                PRECISION_TAPE: encode_value(PRECISION_TAPE, precision),
                HARMONIC_TAPE: encode_value(HARMONIC_TAPE, harmonic),
                LOG_TAPE: encode_value(LOG_TAPE, log_interval),
                EXP_TAPE: encode_value(EXP_TAPE, exp_interval),
                RHS_TAPE: encode_value(RHS_TAPE, rhs_interval),
                OUTPUT_TAPE: encode_value(OUTPUT_TAPE, output_state),
            }
            snapshots.append(
                LagariasExecutionSnapshot(
                    family_name=family_name,
                    control_label=control_label,
                    control_state=control_state,
                    output_state=output_state,
                    tape_states=tape_states,
                    tape_registers=tape_registers,
                )
            )

        append_snapshot(
            family_name='SWEEP_CLOCK_BOUNDARY',
            control_label='INIT',
            divisor=0,
            remainder=0,
            sigma_value=0,
            precision=self.initial_scale,
            output_state=OUTPUT_UNSET_STATE,
        )

        if n <= 0:
            raise ValueError(f"Expected n > 0, got {n}")

        if n == 1:
            unit_interval = IntervalBounds(Fraction(1, 1), Fraction(1, 1))
            decision = LagariasDecision(
                n=1,
                sigma_n=1,
                rhs_interval=unit_interval,
                holds=True,
                output_state=OUTPUT_TRUE_STATE,
                rounds_used=0,
                used_n_equals_one_shortcut=True,
            )
            append_snapshot(
                family_name='DECISION_AND_HALT',
                control_label='HALT_TRUE',
                divisor=1,
                remainder=0,
                sigma_value=1,
                precision=self.initial_scale,
                harmonic=unit_interval,
                rhs_interval=unit_interval,
                output_state=OUTPUT_TRUE_STATE,
            )
            return tuple(snapshots), decision

        sigma_value = 0
        for divisor in range(1, n + 1):
            remainder = modulo_by_repeated_subtraction(n, divisor)
            if remainder == 0:
                sigma_value += divisor

            append_snapshot(
                family_name='SIGMA_DIVISOR_SUM_LOOP',
                control_label=f'SIGMA_LOOP:d={divisor}',
                divisor=divisor,
                remainder=remainder,
                sigma_value=sigma_value,
                precision=self.initial_scale,
                output_state=OUTPUT_UNSET_STATE,
            )

        sigma_n = sigma_value
        sigma_fraction = Fraction(sigma_n, 1)
        scale = self.initial_scale
        log_terms = self.initial_log_terms
        exp_terms = self.initial_exp_terms
        decision: LagariasDecision | None = None

        for round_no in range(1, self.max_precision_rounds + 1):
            harmonic = harmonic_interval_bounds(n=n, scale=scale)
            append_snapshot(
                family_name='HARMONIC_INTERVAL_ACCUMULATION',
                control_label=f'HARMONIC:round={round_no}',
                divisor=n,
                remainder=0,
                sigma_value=sigma_n,
                precision=scale,
                harmonic=harmonic,
                output_state=OUTPUT_UNSET_STATE,
            )

            try:
                log_interval = log_interval_bounds(
                    interval=harmonic, num_terms=log_terms
                )
            except ValueError:
                append_snapshot(
                    family_name='LOG_EXP_INTERVAL_SERIES',
                    control_label=f'LOG_REFINE:round={round_no}',
                    divisor=n,
                    remainder=0,
                    sigma_value=sigma_n,
                    precision=scale,
                    harmonic=harmonic,
                    output_state=OUTPUT_UNSET_STATE,
                )
                scale *= 2
                log_terms += 2
                exp_terms += 4
                continue

            append_snapshot(
                family_name='LOG_EXP_INTERVAL_SERIES',
                control_label=f'LOG_READY:round={round_no}',
                divisor=n,
                remainder=0,
                sigma_value=sigma_n,
                precision=scale,
                harmonic=harmonic,
                log_interval=log_interval,
                output_state=OUTPUT_UNSET_STATE,
            )

            try:
                exp_interval = exp_interval_bounds(
                    interval=harmonic, num_terms=exp_terms
                )
            except ValueError:
                append_snapshot(
                    family_name='LOG_EXP_INTERVAL_SERIES',
                    control_label=f'EXP_REFINE:round={round_no}',
                    divisor=n,
                    remainder=0,
                    sigma_value=sigma_n,
                    precision=scale,
                    harmonic=harmonic,
                    log_interval=log_interval,
                    output_state=OUTPUT_UNSET_STATE,
                )
                scale *= 2
                log_terms += 2
                exp_terms = max(
                    exp_terms + 4,
                    int(math.ceil(float(harmonic.upper))) + 8,
                )
                continue

            append_snapshot(
                family_name='LOG_EXP_INTERVAL_SERIES',
                control_label=f'EXP_READY:round={round_no}',
                divisor=n,
                remainder=0,
                sigma_value=sigma_n,
                precision=scale,
                harmonic=harmonic,
                log_interval=log_interval,
                exp_interval=exp_interval,
                output_state=OUTPUT_UNSET_STATE,
            )

            rhs_interval = compose_rhs_interval(
                harmonic=harmonic,
                log_interval=log_interval,
                exp_interval=exp_interval,
            )
            append_snapshot(
                family_name='RHS_INTERVAL_COMPOSITION',
                control_label=f'RHS_READY:round={round_no}',
                divisor=n,
                remainder=0,
                sigma_value=sigma_n,
                precision=scale,
                harmonic=harmonic,
                log_interval=log_interval,
                exp_interval=exp_interval,
                rhs_interval=rhs_interval,
                output_state=OUTPUT_UNSET_STATE,
            )

            if sigma_fraction <= rhs_interval.lower:
                decision = LagariasDecision(
                    n=n,
                    sigma_n=sigma_n,
                    rhs_interval=rhs_interval,
                    holds=True,
                    output_state=OUTPUT_TRUE_STATE,
                    rounds_used=round_no,
                    used_n_equals_one_shortcut=False,
                )
                append_snapshot(
                    family_name='DECISION_AND_HALT',
                    control_label=f'HALT_TRUE:round={round_no}',
                    divisor=n,
                    remainder=0,
                    sigma_value=sigma_n,
                    precision=scale,
                    harmonic=harmonic,
                    log_interval=log_interval,
                    exp_interval=exp_interval,
                    rhs_interval=rhs_interval,
                    output_state=OUTPUT_TRUE_STATE,
                )
                break

            if sigma_fraction > rhs_interval.upper:
                decision = LagariasDecision(
                    n=n,
                    sigma_n=sigma_n,
                    rhs_interval=rhs_interval,
                    holds=False,
                    output_state=OUTPUT_FALSE_STATE,
                    rounds_used=round_no,
                    used_n_equals_one_shortcut=False,
                )
                append_snapshot(
                    family_name='DECISION_AND_HALT',
                    control_label=f'HALT_FALSE:round={round_no}',
                    divisor=n,
                    remainder=0,
                    sigma_value=sigma_n,
                    precision=scale,
                    harmonic=harmonic,
                    log_interval=log_interval,
                    exp_interval=exp_interval,
                    rhs_interval=rhs_interval,
                    output_state=OUTPUT_FALSE_STATE,
                )
                break

            append_snapshot(
                family_name='DECISION_AND_HALT',
                control_label=f'REFINE_PRECISION:round={round_no}',
                divisor=n,
                remainder=0,
                sigma_value=sigma_n,
                precision=scale,
                harmonic=harmonic,
                log_interval=log_interval,
                exp_interval=exp_interval,
                rhs_interval=rhs_interval,
                output_state=OUTPUT_UNSET_STATE,
            )
            scale *= 2
            log_terms += 2
            exp_terms += 4

        if decision is None:
            raise RuntimeError(
                "Failed to resolve decision within max_precision_rounds "
                f"for n={n}"
            )

        return tuple(snapshots), decision

    def _write_snapshot_to_automata(
        self, snapshot: LagariasExecutionSnapshot
    ):
        if self.multi_tape_automata is None:
            return

        for tape_no, tape_cell_state in snapshot.tape_states.items():
            if tape_no == INPUT_DATA_TAPE:
                continue
            if tape_cell_state == VOID_STATE:
                continue

            self.multi_tape_automata.write_region(
                position=0,
                end_position=0,
                data=[
                    MultiTapeState(
                        tape_no=tape_no,
                        tape_cell_state=TapeCellState(tape_cell_state),
                    )
                ],
            )

    def _sync_tape_registers_from_snapshot(
        self, snapshot: LagariasExecutionSnapshot
    ):
        self.tape_registers[INPUT_DATA_TAPE] = (
            self.initial_write_start,
            self.initial_write_end,
        )

        for tape_no, tape_value in snapshot.tape_registers.items():
            self.tape_registers[tape_no] = tape_value

        if snapshot.control_label.startswith('HALT_'):
            self.tape_registers[CONTROL_TAPE] = 'HALT'
            if snapshot.output_state in (
                OUTPUT_TRUE_STATE,
                OUTPUT_FALSE_STATE,
            ):
                self.tape_registers[OUTPUT_TAPE] = (
                    snapshot.output_state,
                    OUTPUT_HALT_STATE,
                )

    def step(self, verbose: bool = False) -> PyProcessStepResult:
        if self.multi_tape_automata is not None:
            step_result = self.multi_tape_automata.step(verbose=verbose)
        else:
            _ = verbose
            step_result = PyProcessStepResult()

        if self.current_snapshot_index < self.final_snapshot_index:
            self.current_snapshot_index += 1

        snapshot = self.execution_snapshots[self.current_snapshot_index]
        self._sync_tape_registers_from_snapshot(snapshot)
        return step_result

    def run_simulation(
        self,
        num_timesteps: int = BLANK_INT,
        terminal_width: int = 100,
        render_start: int = -5,
        render: bool = False,
    ):
        if num_timesteps == BLANK_INT:
            num_timesteps = self.final_snapshot_index

        for timestep in range(max(num_timesteps, 0)):
            if timestep > 0:
                self.step(verbose=render)

            if not render:
                continue
            if self.multi_tape_automata is None:
                raise RuntimeError(
                    "Automata runtime is unavailable for tape rendering"
                )

            render_frame = self.multi_tape_automata.render_tapes(
                start_position=render_start,
                length=terminal_width,
                cell_width=2,
            )
            print(f'\nTIMESTEP {timestep}:')
            print(render_frame.render())

    def evaluate(self) -> LagariasDecision:
        if self.last_decision is not None:
            return self.last_decision

        while self.current_snapshot_index < self.final_snapshot_index:
            self.step(verbose=False)

        self.last_decision = self.planned_decision
        return self.last_decision
