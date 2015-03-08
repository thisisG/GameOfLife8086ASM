# GameOfLife8086ASM
Conway's Game of Life in 8086 assembly.

A homage to the set of rules put forward by John Horton Conway.

+ Any live cell with fewer than two live neighbours dies, as if caused by under-population.
+ Any live cell with two or three live neighbours lives on to the next generation.
+ Any live cell with more than three live neighbours dies, as if by overcrowding.
+ Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

Ref: http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

Written using the 80x86 assembly instruction set.
Compiled with TASM 1.2 in DOSBox.

Examples:
![alt text][linearInitial]
![alt text][linearFinal]

[linearInitial]: https://github.com/thisisG/GameOfLife8086ASM/blob/master/figures/initialStateLinearGun.png "Linear Gun Initial State"

[linearFinal]: https://github.com/thisisG/GameOfLife8086ASM/blob/master/figures/finalStateLinearGun.png "Linear Gun Final State"
