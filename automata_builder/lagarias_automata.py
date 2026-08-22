from __future__ import annotations

import dataclasses
import math

from fractions import Fraction
from typing import Final


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

OUTPUT_UNSET_STATE: Final[int] = 0
OUTPUT_TRUE_STATE: Final[int] = 1
OUTPUT_FALSE_STATE: Final[int] = 2
OUTPUT_HALT_STATE: Final[int] = 3


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
        self._init_registers()
        self.last_decision: LagariasDecision | None = None

        if derive_n_via_counter_automata:
            n_value = self.derive_n_with_counter_automata()
        else:
            n_value = self.initial_write_end - self.initial_write_start + 1

        self.n_value = n_value
        self.tape_registers[N_TAPE] = n_value

    def _init_registers(self):
        for _, tape_no in self.tape_layout.items():
            self.tape_registers[tape_no] = 0

        self.tape_registers[INPUT_DATA_TAPE] = (
            self.initial_write_start,
            self.initial_write_end,
        )
        self.tape_registers[CONTROL_TAPE] = 'INIT'
        self.tape_registers[OUTPUT_TAPE] = OUTPUT_UNSET_STATE

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

    def _compute_sigma(self, n: int) -> int:
        self.tape_registers[CONTROL_TAPE] = 'SIGMA_LOOP'
        sigma_value = 0

        for divisor in range(1, n + 1):
            self.tape_registers[DIVISOR_TAPE] = divisor
            remainder = modulo_by_repeated_subtraction(n, divisor)
            self.tape_registers[REMAINDER_TAPE] = remainder

            if remainder == 0:
                sigma_value += divisor

            self.tape_registers[SIGMA_TAPE] = sigma_value

        return sigma_value

    def evaluate(self) -> LagariasDecision:
        n = self.n_value
        if n <= 0:
            raise ValueError(f"Expected n > 0, got {n}")

        if n == 1:
            decision = LagariasDecision(
                n=1,
                sigma_n=1,
                rhs_interval=IntervalBounds(Fraction(1, 1), Fraction(1, 1)),
                holds=True,
                output_state=OUTPUT_TRUE_STATE,
                rounds_used=0,
                used_n_equals_one_shortcut=True,
            )
            self.tape_registers[CONTROL_TAPE] = 'HALT'
            self.tape_registers[OUTPUT_TAPE] = (
                OUTPUT_TRUE_STATE,
                OUTPUT_HALT_STATE,
            )
            self.last_decision = decision
            return decision

        sigma_n = self._compute_sigma(n)
        sigma_fraction = Fraction(sigma_n, 1)
        scale = self.initial_scale
        log_terms = self.initial_log_terms
        exp_terms = self.initial_exp_terms

        self.tape_registers[CONTROL_TAPE] = 'PRECISION_LOOP'
        for round_no in range(1, self.max_precision_rounds + 1):
            self.tape_registers[PRECISION_TAPE] = scale
            harmonic = harmonic_interval_bounds(n=n, scale=scale)
            self.tape_registers[HARMONIC_TAPE] = harmonic

            log_interval = log_interval_bounds(
                interval=harmonic, num_terms=log_terms
            )
            self.tape_registers[LOG_TAPE] = log_interval

            try:
                exp_interval = exp_interval_bounds(
                    interval=harmonic, num_terms=exp_terms
                )
            except ValueError:
                scale *= 2
                log_terms += 2
                exp_terms = max(
                    exp_terms + 4,
                    int(math.ceil(float(harmonic.upper))) + 8,
                )
                continue

            self.tape_registers[EXP_TAPE] = exp_interval
            rhs_interval = compose_rhs_interval(
                harmonic=harmonic,
                log_interval=log_interval,
                exp_interval=exp_interval,
            )
            self.tape_registers[RHS_TAPE] = rhs_interval

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
                self.tape_registers[CONTROL_TAPE] = 'HALT'
                self.tape_registers[OUTPUT_TAPE] = (
                    OUTPUT_TRUE_STATE,
                    OUTPUT_HALT_STATE,
                )
                self.last_decision = decision
                return decision

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
                self.tape_registers[CONTROL_TAPE] = 'HALT'
                self.tape_registers[OUTPUT_TAPE] = (
                    OUTPUT_FALSE_STATE,
                    OUTPUT_HALT_STATE,
                )
                self.last_decision = decision
                return decision

            scale *= 2
            log_terms += 2
            exp_terms += 4

        raise RuntimeError(
            "Failed to resolve decision within max_precision_rounds "
            f"for n={n}"
        )
