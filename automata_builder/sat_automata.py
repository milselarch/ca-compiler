from __future__ import annotations

import itertools
import os
import re

from dataclasses import dataclass
from typing import Callable, Final

from py_ca_compiler import D

from automata_builder.rule_generator_multitape import (
    BLANK_INT,
    MultiTapeAutomata,
    MultiTapeRuleGenerator,
    MultiTapeState,
    MultiTapeTransitionsGroup,
    ProcessStepResult,
    TapeCellState,
    TapeNo,
    VOID_STATE,
)

CONSTRAINTS_TAPE: Final[TapeNo] = TapeNo(0)
ASSIGNMENTS_TAPE: Final[TapeNo] = TapeNo(1)
CLAUSE_RESULTS_TAPE: Final[TapeNo] = TapeNo(2)
VERDICT_TAPE: Final[TapeNo] = TapeNo(3)

ASSIGN_FALSE: Final[TapeCellState] = TapeCellState(2)
ASSIGN_TRUE: Final[TapeCellState] = TapeCellState(3)
CLAUSE_SAT: Final[TapeCellState] = TapeCellState(4)
CLAUSE_UNSAT: Final[TapeCellState] = TapeCellState(5)
VERDICT_PENDING: Final[TapeCellState] = TapeCellState(6)
VERDICT_SAT: Final[TapeCellState] = TapeCellState(7)
VERDICT_UNSAT: Final[TapeCellState] = TapeCellState(8)
INVALID_INPUT_MARKER: Final[TapeCellState] = TapeCellState(9)
FIRST_CLAUSE_STATE: Final[TapeCellState] = TapeCellState(16)

LEFT: Final[int] = -1
MID: Final[int] = 0
RIGHT: Final[int] = 1

_VAR_NAME_RE: Final[re.Pattern[str]] = re.compile(r'^[A-Za-z][A-Za-z0-9_]*$')


def prefill_tape(position: int, tape_no: int) -> Callable[[int], D]:
    def set_cell_state(cell_state: int) -> D:
        return D(position, tape_no, cell_state)

    return set_cell_state


def prefill_tape_no(tape_no: int) -> Callable[[int, int], D]:
    def set_position_and_cell_state(position: int, cell_state: int) -> D:
        return D(position, tape_no, cell_state)

    return set_position_and_cell_state


CONSTRAINTS_MID: Final[Callable[[int], D]] = prefill_tape(
    MID, CONSTRAINTS_TAPE
)
ASSIGNMENTS: Final[Callable[[int, int], D]] = prefill_tape_no(
    ASSIGNMENTS_TAPE
)
CLAUSE_RESULTS_MID: Final[Callable[[int], D]] = prefill_tape(
    MID, CLAUSE_RESULTS_TAPE
)
CLAUSE_RESULTS: Final[Callable[[int, int], D]] = prefill_tape_no(
    CLAUSE_RESULTS_TAPE
)
VERDICT_MID: Final[Callable[[int], D]] = prefill_tape(MID, VERDICT_TAPE)


@dataclass(frozen=True)
class Literal(object):
    variable: str
    negated: bool = False


@dataclass(frozen=True)
class Parsed3SAT(object):
    clauses: tuple[tuple[Literal, Literal, Literal], ...]

    @property
    def variable_names(self) -> tuple[str, ...]:
        names: list[str] = []
        for clause in self.clauses:
            for literal in clause:
                names.append(literal.variable)

        return tuple(sorted(set(names)))


def _parse_literal(raw_literal: str) -> Literal | None:
    literal = raw_literal.strip()
    if not literal:
        return None

    negated = False
    if literal[0] in ('~', '!'):
        negated = True
        literal = literal[1:].strip()

    if not _VAR_NAME_RE.match(literal):
        return None

    return Literal(variable=literal, negated=negated)


def parse_3sat_equation(equation: str) -> Parsed3SAT | None:
    """
    Parse a strict 3SAT equation in CNF form:
    (a|~b|c)&(~a|d|e)&...
    with optional whitespace.
    """
    expression = equation.strip()
    if not expression:
        return None

    clauses: list[tuple[Literal, Literal, Literal]] = []
    index = 0
    expr_len = len(expression)

    expect_clause = True

    while index < expr_len:
        while index < expr_len and expression[index].isspace():
            index += 1
        if index >= expr_len:
            break

        if expect_clause:
            if expression[index] != '(':
                return None

            close_idx = expression.find(')', index + 1)
            if close_idx < 0:
                return None

            inner_clause = expression[index + 1:close_idx]
            literal_chunks = [
                chunk.strip() for chunk in inner_clause.split('|')
            ]
            if len(literal_chunks) != 3:
                return None

            parsed_literals: list[Literal] = []
            for chunk in literal_chunks:
                literal = _parse_literal(chunk)
                if literal is None:
                    return None
                parsed_literals.append(literal)

            clauses.append(
                (parsed_literals[0], parsed_literals[1], parsed_literals[2])
            )
            index = close_idx + 1
            expect_clause = False
        else:
            if expression[index] != '&':
                return None
            index += 1
            expect_clause = True

    if not clauses:
        return None
    if expect_clause:
        return None

    return Parsed3SAT(clauses=tuple(clauses))


def parse_assignment(assignment: str) -> dict[str, bool] | None:
    """
    Parse assignment text:
    x1=1,x2=0,x3=true
    """
    raw_assignment = assignment.strip()
    if not raw_assignment:
        return None

    mapping: dict[str, bool] = {}
    pairs = raw_assignment.split(',')

    for pair in pairs:
        if '=' not in pair:
            return None
        raw_name, raw_value = pair.split('=', 1)
        variable = raw_name.strip()
        value = raw_value.strip().lower()

        if not _VAR_NAME_RE.match(variable):
            return None
        if variable in mapping:
            return None

        if value in ('1', 'true', 't'):
            mapping[variable] = True
        elif value in ('0', 'false', 'f'):
            mapping[variable] = False
        else:
            return None

    if not mapping:
        return None

    return mapping


def evaluate_clause(
    clause: tuple[Literal, Literal, Literal],
    assignment: dict[str, bool]
) -> bool:
    for literal in clause:
        variable_val = assignment[literal.variable]
        literal_val = (not variable_val) if literal.negated else variable_val
        if literal_val:
            return True

    return False


class ThreeSATAutomataBuilder(object):
    def __init__(
        self,
        parsed_equation: Parsed3SAT | None,
        variable_positions: dict[str, int],
        invalid_input: bool = False,
    ):
        self.parsed_equation = parsed_equation
        self.variable_positions = variable_positions
        self.invalid_input = invalid_input

    def get_clause_state(self, clause_index: int) -> TapeCellState:
        return TapeCellState(int(FIRST_CLAUSE_STATE) + clause_index)

    def build_transitions_group(self) -> MultiTapeTransitionsGroup:
        transitions_group = MultiTapeTransitionsGroup(require_annotation=True)
        if self.invalid_input or self.parsed_equation is None:
            transitions_group.add_transition(
                input_terms=(VERDICT_MID(VERDICT_PENDING),),
                output_tape_no=VERDICT_TAPE,
                output_cell_state=VERDICT_UNSAT,
                annotation='INVALID_INPUT_UNSAT'
            )
            return transitions_group

        for clause_index, clause in enumerate(self.parsed_equation.clauses):
            clause_state = self.get_clause_state(clause_index)
            unique_vars = sorted({literal.variable for literal in clause})

            for bool_pattern in itertools.product(
                (False, True), repeat=len(unique_vars)
            ):
                local_assignment = {
                    unique_vars[k]: bool_pattern[k]
                    for k in range(len(unique_vars))
                }
                clause_is_satisfied = evaluate_clause(clause, local_assignment)
                output_state = (
                    CLAUSE_SAT if clause_is_satisfied else CLAUSE_UNSAT
                )

                input_terms: list[D] = [
                    CONSTRAINTS_MID(clause_state),
                    CLAUSE_RESULTS_MID(VOID_STATE),
                ]
                assignment_bits: list[str] = []

                for variable in unique_vars:
                    variable_position = self.variable_positions[variable]
                    variable_state = (
                        ASSIGN_TRUE if local_assignment[variable]
                        else ASSIGN_FALSE
                    )
                    input_terms.append(
                        ASSIGNMENTS(variable_position - clause_index,
                                    variable_state)
                    )
                    assignment_bits.append('1' if local_assignment[variable]
                                           else '0')

                annotation = (
                    f'CLAUSE_{clause_index}_'
                    f'{"".join(assignment_bits)}_'
                    f'{"SAT" if clause_is_satisfied else "UNSAT"}'
                )
                transitions_group.add_transition(
                    input_terms=tuple(input_terms),
                    output_tape_no=CLAUSE_RESULTS_TAPE,
                    output_cell_state=output_state,
                    annotation=annotation
                )

        sat_terms: list[D] = [VERDICT_MID(VERDICT_PENDING)]
        for clause_index in range(len(self.parsed_equation.clauses)):
            sat_terms.append(CLAUSE_RESULTS(clause_index, CLAUSE_SAT))

        transitions_group.add_transition(
            input_terms=tuple(sat_terms),
            output_tape_no=VERDICT_TAPE,
            output_cell_state=VERDICT_SAT,
            annotation='VERDICT_SAT'
        )

        for clause_index in range(len(self.parsed_equation.clauses)):
            transitions_group.add_transition(
                input_terms=(
                    VERDICT_MID(VERDICT_PENDING),
                    CLAUSE_RESULTS(clause_index, CLAUSE_UNSAT),
                ),
                output_tape_no=VERDICT_TAPE,
                output_cell_state=VERDICT_UNSAT,
                annotation=f'VERDICT_UNSAT_{clause_index}'
            )

        return transitions_group


class ThreeSATAutomataRunner(object):
    def __init__(self, equation: str, assignment: str):
        self.equation = equation
        self.assignment = assignment
        self.invalid_input: bool = False
        self.invalid_reason: str | None = None

        parsed_equation = parse_3sat_equation(equation)
        parsed_assignment = parse_assignment(assignment)
        if parsed_equation is None:
            self.invalid_input = True
            self.invalid_reason = 'INVALID_EQUATION'
        elif parsed_assignment is None:
            self.invalid_input = True
            self.invalid_reason = 'INVALID_ASSIGNMENT'
        else:
            missing_variables = [
                name for name in parsed_equation.variable_names
                if name not in parsed_assignment
            ]
            if missing_variables:
                self.invalid_input = True
                self.invalid_reason = (
                    f'MISSING_ASSIGNMENT:{",".join(missing_variables)}'
                )

        self.parsed_equation = parsed_equation if not self.invalid_input else None
        self.parsed_assignment = (
            parsed_assignment if not self.invalid_input else None
        )

        if self.parsed_equation is None:
            self.variable_positions: dict[str, int] = {}
        else:
            variable_names = self.parsed_equation.variable_names
            self.variable_positions = {
                name: index for index, name in enumerate(variable_names)
            }

        self.builder = ThreeSATAutomataBuilder(
            parsed_equation=self.parsed_equation,
            variable_positions=self.variable_positions,
            invalid_input=self.invalid_input,
        )
        self.transitions_group = self.builder.build_transitions_group()
        self.state_eq_map = MultiTapeRuleGenerator.generate_equations(
            self.transitions_group
        )
        self.multi_tape_automata = MultiTapeAutomata(self.state_eq_map)
        self.multi_tape_automata.init_tapes(
            tape_nos=[
                CONSTRAINTS_TAPE,
                ASSIGNMENTS_TAPE,
                CLAUSE_RESULTS_TAPE,
                VERDICT_TAPE,
            ]
        )
        self._initialize_tapes()

    def _initialize_tapes(self):
        self.multi_tape_automata.write_region(
            position=0,
            end_position=0,
            data=[MultiTapeState(VERDICT_TAPE, VERDICT_PENDING)],
        )

        if self.invalid_input:
            self.multi_tape_automata.write_region(
                position=0,
                end_position=0,
                data=[MultiTapeState(CONSTRAINTS_TAPE, INVALID_INPUT_MARKER)],
            )
            return

        assert self.parsed_equation is not None
        assert self.parsed_assignment is not None

        for clause_index in range(len(self.parsed_equation.clauses)):
            self.multi_tape_automata.write_region(
                position=clause_index,
                end_position=clause_index,
                data=[MultiTapeState(
                    CONSTRAINTS_TAPE, self.builder.get_clause_state(clause_index)
                )],
            )

        for variable_name, variable_position in self.variable_positions.items():
            variable_value = self.parsed_assignment[variable_name]
            encoded_value = ASSIGN_TRUE if variable_value else ASSIGN_FALSE
            self.multi_tape_automata.write_region(
                position=variable_position,
                end_position=variable_position,
                data=[MultiTapeState(ASSIGNMENTS_TAPE, encoded_value)],
            )

    def step(self, verbose: bool = False) -> ProcessStepResult:
        return self.multi_tape_automata.step(verbose=verbose)

    def read_verdict_state(self) -> TapeCellState:
        verdict_tape = self.multi_tape_automata[VERDICT_TAPE]
        return TapeCellState(verdict_tape.read(0))

    def read_verdict(self) -> str:
        verdict_state = self.read_verdict_state()
        if verdict_state == VERDICT_SAT:
            return 'SAT'
        if verdict_state == VERDICT_UNSAT:
            return 'UNSAT'
        return 'PENDING'

    def is_satisfiable(self) -> bool:
        return self.read_verdict_state() == VERDICT_SAT

    def run_simulation(
        self,
        num_timesteps: int = 3,
        terminal_width: int = BLANK_INT,
        render_start: int = -3,
        render: bool = False,
    ):
        try:
            terminal_size = os.get_terminal_size()
            default_terminal_width = terminal_size.columns - 1
        except OSError:
            default_terminal_width = 100

        if terminal_width == BLANK_INT:
            terminal_width = default_terminal_width

        for timestep in range(num_timesteps):
            if timestep > 0:
                self.step(verbose=render)

            if render:
                frame = self.multi_tape_automata.render_tapes(
                    start_position=render_start,
                    length=terminal_width,
                    cell_width=2
                )
                print(f'\nTIMESTEP {timestep}:')
                print(frame.render())
                print(f'verdict={self.read_verdict()}')

    def evaluate(self, num_timesteps: int = 3) -> str:
        self.run_simulation(num_timesteps=num_timesteps, render=False)
        return self.read_verdict()
