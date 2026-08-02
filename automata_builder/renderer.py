from __future__ import annotations

import copy
from typing import Sequence


class RenderFrame(object):
    def __init__(self, lines: Sequence[str] = ()):
        lengths = [len(line) for line in lines]
        assert len(set(lengths)) <= 1, "All lines must have the same length"
        self.lines = list(lines)[:]

    def get_lines(self) -> list[str]:
        return self.lines[:]

    def get_width(self) -> int:
        if not self.lines:
            return 0

        return len(self.lines[0])

    def get_height(self) -> int:
        return len(self.lines)

    def get_dimensions(self):
        return self.get_height(), self.get_width()

    @classmethod
    def from_line(cls, line: str) -> RenderFrame:
        return RenderFrame([line])

    def render(self) -> str:
        return '\n'.join(self.lines)

    def __repr__(self):
        name = self.__class__.__name__
        return f'{name}(lines={self.lines})'

    def add_line(self, line: str) -> None:
        if len(line) != self.get_width():
            raise ValueError("Line length must match frame width")

        self.lines.append(line)

    def extend_down(self, other: RenderFrame) -> RenderFrame:
        if self.get_height() == 0:
            self.lines = other.get_lines()
            return self

        if self.get_width() != other.get_width():
            print(self.get_width(), other.get_width())
            raise ValueError("Frame widths must match to extend down")

        self.lines.extend(other.lines)
        return self

    def extend_up(self, other: RenderFrame) -> RenderFrame:
        if self.get_height() == 0:
            self.lines = other.get_lines()
            return self

        if self.get_width() != other.get_width():
            raise ValueError("Frame widths must match to extend up")

        self.lines = other.lines + self.lines
        return self

    def extend_left(self, other: RenderFrame) -> RenderFrame:
        if self.get_height() == 0:
            self.lines = other.get_lines()
            return self

        if self.get_height() != other.get_height():
            raise ValueError("Frame heights must match to extend left")

        new_lines = []
        for line_self, line_other in zip(self.get_lines(), other.get_lines()):
            new_lines.append(line_other + line_self)

        self.lines = new_lines
        return self

    def extend_right(self, other: RenderFrame) -> RenderFrame:
        if self.get_height() == 0:
            self.lines = other.get_lines()
            return self

        if self.get_height() != other.get_height():
            raise ValueError("Frame heights must match to extend right")

        new_lines = []
        for line_self, line_other in zip(self.get_lines(), other.get_lines()):
            new_lines.append(line_self + line_other)

        self.lines = new_lines
        return self

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

        # print("Combined frame lines:", combined_frame.get_lines())
        return combined_frame
