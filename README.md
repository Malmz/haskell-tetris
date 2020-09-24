# Tetris

In this lab assignment you will implement a simple variant of Tetris, a
popular computer game that was created in the 1980s. In Tetris, a sequence of
random puzzle pieces, made up of four connected squares, fall towards the
bottom of a well. The player can rotate the pieces and control where they end
up by moving them left and right as they fall. If the pieces fit together and
fill a complete row when they reach the bottom of the well, the row
disappears and the player gets some points. If there are gaps in the rows,
the pieces can start to pile up, and if the well gets so full of piled up
pieces that there is no room for new pieces, the game ends.

## The assignment

You will not need to write all the Haskell code needed to implement the game
yourself. We provide some modules that help with the user interface, and you
write the following to parts:

- Part A: Basic functions for working with the geometric shapes that appear
  in the game: the falling pieces and the contents of the “well”.
- Part B: Functions for combining shapes and basic actions on the state of
  the game: drawing the well, moving pieces and ticking of the clock.
- Part C: Functions that complete the game experience: collisions, rotations
  and clearing rows.