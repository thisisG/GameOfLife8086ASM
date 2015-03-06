# GameOfLife8086ASM
Conway's Game of Life in 8086 assembly.

A homage to the set of rules put forward by John Horton Conway.

1: Any live cell with fewer than two live neighbours dies, as if caused by under-population.
2: Any live cell with two or three live neighbours lives on to the next generation.
3: Any live cell with more than three live neighbours dies, as if by overcrowding.
4: Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

Ref: http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

Written using the 80x86 assembly instruction set.
Compiled with TASM 1.2 in DOSBox.

Example initial and end (if they excist) states in figures folder.
