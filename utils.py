from typing import TypeVar, Iterator, Tuple, Sequence

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
