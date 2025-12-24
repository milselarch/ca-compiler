## Potato CPU Requirements

1. There are a finite number of registers
2. registers can have infinite size
3. The stack address space is infinite (the stack address is also a register after all)
4. Each stack address can hold a fixed, finite size value
5. There is no heap memory inherently
   - dynamic memory allocation might be simulated
     by interleaving every other cell as heap memory?
   - Or maybe another approach would be to translate dynamic memory
     to a stack allocated VLA 
     (but then the compiler would have to support VLAs)
6. There should be a single fixed cell position to check for halting
7. support for an infinite length unsigned integer datatype
   - twos complement doesn't quite make sense for infinite length integers
     (since there is no max value to wrap around from) so at some higher
     level support should be added with a sign bit maybe
