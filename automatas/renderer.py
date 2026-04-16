from __future__ import annotations

import copy
from typing import Sequence


class RenderFrame(object):
    def __init__(self, lines: Sequence[str] = ()):
        lengths = [len(line) for line in lines]
        assert len(set(lengths)) == 1, "All lines must have the same length"
        self.lines = list(lines)[:]

    def get_lines(self) -> list[str]:
        return self.lines[:]

    def get_width(self) -> int:
        if not self.lines:
            return 0

        return len(self.lines[0])

    def get_height(self) -> int:
        return len(self.lines)

    @classmethod
    def from_line(cls, line: str) -> RenderFrame:
        return RenderFrame([line])

    def __str__(self) -> str:
        return '\n'.join(self.lines)

    def add_line(self, line: str) -> None:
        if len(line) != self.get_width():
            raise ValueError("Line length must match frame width")

        self.lines.append(line)

    def extend_down(self, other: RenderFrame) -> RenderFrame:
        if self.get_height() == 0:
            return self.__class__(other.get_lines())

        if self.get_width() != other.get_width():
            raise ValueError("Frame widths must match to extend down")

        return self.__class__(
            self.get_lines() + other.get_lines()
        )

    def extend_up(self, other: RenderFrame) -> RenderFrame:
        if self.get_height() == 0:
            return self.__class__(other.get_lines())

        if self.get_width() != other.get_width():
            raise ValueError("Frame widths must match to extend up")

        return self.__class__(
            other.get_lines() + self.get_lines()
        )

    def extend_left(self, other: RenderFrame) -> RenderFrame:
        if self.get_height() == 0:
            return self.__class__(other.get_lines())

        if self.get_height() != other.get_height():
            raise ValueError("Frame heights must match to extend left")

        new_lines = []
        for line_self, line_other in zip(self.get_lines(), other.get_lines()):
            new_lines.append(line_other + line_self)

        return self.__class__(new_lines)

    def extend_right(self, other: RenderFrame) -> RenderFrame:
        if self.get_height() == 0:
            return self.__class__(other.get_lines())

        if self.get_height() != other.get_height():
            raise ValueError("Frame heights must match to extend right")

        new_lines = []
        for line_self, line_other in zip(self.get_lines(), other.get_lines()):
            new_lines.append(line_self + line_other)

        return self.__class__(new_lines)

    @classmethod
    def join_vertically(cls, frames: list[RenderFrame]) -> RenderFrame:
        combined_frame = RenderFrame()
        for frame in frames:
            combined_frame.extend_down(frame)

        return combined_frame

    @classmethod
    def join_horizontally(cls, frames: list[RenderFrame]) -> RenderFrame:
        combined_frame = RenderFrame()
        for frame in frames:
            combined_frame.extend_right(frame)

        return combined_frame
