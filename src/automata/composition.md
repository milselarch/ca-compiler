Plan to compose a multi-tape automata into a single-tape automata

1. pre-requisites
   - There are an arbitrary number of tapes
   - There is a designated input tape with a fixed number of valid input states
   - There is a VOID state that represents an empty cell on all tapes
     - there cannot be any write rule that transitions void-only cells to non-void states
     - i.e. given a bunch of input cells that are all in the VOID state, 
       there should be no write rule to transition a cell to a non-VOID state.
   - All tapes are such that all cells are in the VOID state by default
     - except for a contiguous block of cells from positions 1 onward
       on the input tape that are initialized to valid input states
     - all other cells on all tapes are initialized to the VOID state
   - Every tape's write rules will only write to its own tape, but the input 
     tapes referenced in the rule can be from any tape
   - there is HALT state that represents the halting condition
     - the HALT state will propagate in all directions on all tapes once written
   - no tape can write to the left of position 1 on the input tape
     (except for HALT propagation)
   - each rule should only read from the same position,
     or one position to the left or right on all tapes
2. composition algorithm
   - we can do a BFS traversal for filling which rules are ambiguous
   - the start frontier would be the input tape


