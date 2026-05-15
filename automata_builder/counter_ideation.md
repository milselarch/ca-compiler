
signals tape - ST
   1. right to left sweeper
   1. mark exponential bit reduction start
   1. ST+LEFT[VOID] DT+LEFT[DATA] ST+MID[VOID]
   1. ST+MID[REDUCE_START]
   2. begin the counter accumulator
   1. ST+MID[VOID] DT+MID[DATA] ST+RIGHT[VOID] =>
   ST+MID[COUNTER_{1}\_PAUSE]
   3. shift leftmost counter cell and increment
   1. if counter state n == MAX
   1. ST+MID[VOID] ST+RIGHT[COUNTER_{MAX}]
   1. ST+MID[COUNTER_{0}\_PAUSE]
   2. ST+LEFT[VOID] ST+MID[COUNTER_{MAX}]
   1. CT+MID[CARRY]
   2. asigns a carry signal to the current cell if the previous state was gonna overflow
   2. else (counter state n < MAX)
   1. ST+MID[VOID] ST+RIGHT[COUNTER_{n}]
   1. ST+MID[COUNTER_{n+1}\_PAUSE]
   4. shift counter cells left
   1. if there is no carry
   1. ST+MID[COUNTER_{m}] ST+RIGHT[COUNTER_{n}] CT+MID[VOID]
   1. ST+MID[COUNTER_{n}\_PAUSE]
   2. if there is a carry but no overflow (n < MAX)
   1. ST+MID[COUNTER_{m}] ST+RIGHT[COUNTER_{n}] CT+MID[CARRY]
   1. ST+MID[COUNTER_{n+1}\_PAUSE]
   2. CT+MID[VOID]
   3. if there is a carry and there is overflow (n=MAX)
   1. ST+MID[COUNTER_{m}] ST+RIGHT[COUNTER_{MAX}] CT+MID[CARRY]
   1. ST+MID[COUNTER_{0}\_PAUSE]
   2. CT+MID[CARRY]
   5. handle right end of built number
   1. if there is a carry and we're at the end of the built number sequence
   1. ST+MID[COUNTER_{m}] ST+RIGHT[VOID] CT+MID[CARRY]
   1. ST+MID[COUNTER_{1}\_PAUSE]
   2. CT+MID[VOID]
   2. clear rightmost cell if no carry
   1. ST+MID[COUNTER_{m}] ST+RIGHT[VOID] CT+MID[VOID]
   1. ST+MID[VOID]
   6. pause to unpause
   1. ST+MID[COUNTER_{n}\_PAUSE]
   1. ST+MID[COUNTER_{n}]

4. so there are 3 tapes involved:
    1. a data tape with states VOID and DATA - 1 bit
    2. a counter tape with states VOID and CARRY - 1 bit
    3. a universal halt state - 1 bit
    4. a signals tape -
        1. digit states
            1. log2(n+1) + 1 bits, n is the base of the accumulator
            2. we have to add 1 bit cause every digit has a pause variant
            3. round up to the nearest integer
        2. 
