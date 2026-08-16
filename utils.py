import copy
import typing

from abc import ABCMeta, abstractmethod
from collections import defaultdict
from typing import TypeVar, Iterator, Tuple, Sequence, Generic, Callable
from dataclasses import is_dataclass

T = TypeVar('T')
U = TypeVar('U')


def zip_preserve_types(
    a: Sequence[T], b: Sequence[U]
) -> Iterator[Tuple[T, U]]:
    if len(a) != len(b):
        raise ValueError("Input sequences must have the same length")

    for k in range(len(a)):
        yield a[k], b[k]


def cartesian_product(
    sequences: Sequence[Sequence[T]]
) -> Iterator[Tuple[T, ...]]:
    if not sequences:
        yield ()
        return

    first, *rest = sequences
    for item in first:
        for rest_items in cartesian_product(rest):
            yield item, *rest_items


def is_frozen_dataclass_instance(obj: object) -> bool:
    if not is_dataclass(obj) or isinstance(obj, type):
        return False

    # __dataclass_params__ exists on dataclass types
    return bool(getattr(type(obj), "__dataclass_params__").frozen)


K = TypeVar("K")
V = TypeVar("V")


class Freezable(metaclass=ABCMeta):
    def __init__(self) -> None:
        self._frozen: bool = False

    @staticmethod
    def is_allowed_value(value: typing.Any) -> bool:
        if isinstance(value, (int, float, str, bool)):
            return True
        elif is_frozen_dataclass_instance(value):
            return True
        if isinstance(value, Freezable):
            return True

        return False

    def freeze(self) -> bool:
        if self._frozen:
            return False

        self._freeze()
        self._frozen = True
        return True

    @abstractmethod
    def _freeze(self) -> None:
        raise NotImplementedError

    def encode(self):
        return self._encode()

    @abstractmethod
    def _encode(self) -> tuple:
        raise NotImplementedError

    def decode(self, data: tuple):
        if not self._frozen:
            raise ValueError("Cannot encode a non-frozen Freezable object")

        return self._decode(data)

    @classmethod
    @abstractmethod
    def _decode(cls, data: tuple) -> typing.Self:
        raise NotImplementedError

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, type(self)):
            return False

        return self.encode() == other.encode()

    def __hash__(self) -> int:
        classname = self.__class__.__name__
        if not self._frozen:
            raise ValueError(f"Cannot hash {classname} when not frozen")

        return hash(self.encode())


class FreezableSet(Freezable, Generic[V]):
    def __init__(self, *args: list[V]) -> None:
        super().__init__()
        self._data: set[V] = set(args)

    def __iter__(self) -> Iterator[V]:
        return iter(self._data)

    def remove(self, value: V):
        if self._frozen:
            raise ValueError(
                f"Cannot delete from frozen {self.__class__.__name__}"
            )

        self._data.remove(value)

    def clone_data(self) -> set[V]:
        return copy.copy(self._data)

    def add(self, other: V) -> None:
        if self._frozen:
            raise ValueError("Cannot add to frozen set")

        if not self.is_allowed_value(other):
            raise ValueError(f"Value {other} is not allowed")

        self._data.add(other)

    def _freeze(self) -> None:
        for value in self._data:
            if isinstance(value, Freezable):
                value.freeze()

    def _encode(self) -> tuple[V, ...]:
        return tuple(sorted(list(self._data)))

    @classmethod
    def _decode(cls, data: tuple[V, ...]) -> typing.Self:
        return cls(*data)


class FreezableDefaultDict(Freezable, Generic[K, V]):
    def __init__(self, default_factory: Callable[[], V]) -> None:
        super().__init__()
        self._data: defaultdict[K, V] = defaultdict(default_factory)
        self._frozen: bool = False

    def _freeze(self) -> None:
        for value in self._data.values():
            if isinstance(value, Freezable):
                value.freeze()

    def __delitem__(self, key: K):
        if self._frozen:
            raise ValueError(
                f"Cannot delete from frozen {self.__class__.__name__}"
            )

        del self._data[key]

    def __getitem__(self, key: K) -> V:
        classname = self.__class__.__name__
        if self._frozen and key not in self._data:
            raise ValueError(
                f"Cannot create missing key in frozen {classname}"
            )

        return self._data[key]

    def __setitem__(self, key: K, value: V) -> None:
        classname = self.__class__.__name__
        if self._frozen:
            raise ValueError(f"Cannot modify frozen {classname}")

        if not self.is_allowed_value(value):
            raise ValueError(f"Value {value} is not allowed")

        self._data[key] = value

    def __contains__(self, key: object) -> bool:
        return key in self._data

    def get(self, key: K, default: V | None = None) -> V | None:
        return self._data.get(key, default)

    def items(self):
        return self._data.items()

    def keys(self):
        return self._data.keys()

    def values(self):
        return self._data.values()

    def pop(self, key: K) -> V:
        return self._data.pop(key)

    def __iter__(self) -> Iterator[K]:
        return iter(self._data)

    def __len__(self) -> int:
        return len(self._data)

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({dict(self._data)!r})"

    def _encode(self) -> tuple[tuple[K, V], ...]:
        keys = sorted(list(self.keys()))
        return tuple([
            (key, self._data[key]) for key in keys
        ])

    @classmethod
    def _decode(cls, data: tuple[tuple[K, V], ...]) -> typing.Self:
        instance = cls(lambda: None)  # Provide a default factory
        for key, value in data:
            instance[key] = value

        instance.freeze()
        return instance
