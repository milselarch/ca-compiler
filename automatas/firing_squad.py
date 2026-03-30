from py_ca_compiler import A as Cell

X = 0  # void state
H = 1  # invalid / halt state

L = 2
A = 3
B = 4
C = 5
G = 6  # general state
F = 7  # firing state

"""
# korec rule matrix
RULE_MATRIX: dict[int, dict[int, int]] = {
    N: {N: N, 0: N, 1: N, 2: 1, 3: 1, 4: 2, 5: 2},
    0: {N: N, 0: 0, 1: 0, 2: 1, 3: 1, 4: 2, 5: 2},
    1: {N: 4, 0: 3, 1: 3, 2: 4, 3: 4, 4: 5, 5: 5},
    2: {N: N, 0: 0, 1: 0, 2: 1, 3: 1, 4: 2, 5: 2},
    3: {N: 4, 0: 3, 1: 3, 2: 4, 3: 4, 4: 5, 5: 5},
    4: {N: N, 0: 0, 1: 0, 2: 1, 3: 1, 4: 2, 5: 2},
    5: {N: 4, 0: 3, 1: 3, 2: 4, 3: 4, 4: 5, 5: 5},
}
"""

RULE_MATRICES: dict[int, dict[int, dict[int, int]]] = {
    L: {
        X: {X: H, L: L, A: H, B: H, C: H, G: H},
        L: {X: L, L: L, A: H, B: L, C: L, G: L},
        A: {X: C, L: G, A: L, B: L, C: L, G: C},
        B: {X: L, L: L, A: L, B: L, C: L, G: L},
        C: {X: G, L: A, A: L, B: L, C: L, G: G},
        G: {X: A, L: C, A: L, B: L, C: L, G: A},
    },
    A: {
        X: {X: H, L: H, A: F, B: H, C: G, G: H},
        L: {X: H, L: H, A: A, B: L, C: G, G: H},
        A: {X: F, L: A, A: A, B: B, C: C, G: B},
        B: {X: C, L: G, A: H, B: G, C: C, G: C},
        C: {X: H, L: A, A: A, B: H, C: H, G: H},
        G: {X: C, L: H, A: H, B: H, C: C, G: C},
    },
    B: {
        X: {X: H, L: H, A: H, B: H, C: H, G: H},
        L: {X: H, L: H, A: G, B: B, C: L, G: B},
        A: {X: H, L: G, A: B, B: B, C: L, G: H},
        B: {X: H, L: G, A: A, B: B, C: C, G: B},
        C: {X: L, L: L, A: A, B: H, C: H, G: L},
        G: {X: G, L: C, A: C, B: H, C: B, G: G},
    },
    C: {
        X: {X: H, L: H, A: H, B: H, C: H, G: H},
        L: {X: H, L: H, A: H, B: H, C: H, G: H},
        A: {X: H, L: H, A: H, B: H, C: H, G: H},
        B: {X: H, L: H, A: H, B: H, C: H, G: H},
        C: {X: H, L: H, A: H, B: H, C: H, G: H},
        G: {X: H, L: H, A: H, B: H, C: H, G: H},
    },
    G: {
        X: {X: H, L: H, A: H, B: H, C: H, G: H},
        L: {X: H, L: H, A: H, B: H, C: H, G: H},
        A: {X: H, L: H, A: H, B: H, C: H, G: H},
        B: {X: H, L: H, A: H, B: H, C: H, G: H},
        C: {X: H, L: H, A: H, B: H, C: H, G: H},
        G: {X: H, L: H, A: H, B: H, C: H, G: H},
    }
}