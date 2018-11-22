# pacman-hskl

My attempt at creating a mini version of the classic retro game Pacman with haskell. The game uses a graphics library called gloss which uses OpenGL.

To run the game use

`cabal install`

`cabal run`

Use the arrow keys to control pacman
Move pacman over the dots to eat them. 

Goal of the game is to eat all the dots before running out of lives.
A life is lost when a ghost touches pacman. Total lives = 3.

For a more challenging experience change the pacSpeed in GameState.hs line 10 and change the ghostSpeed in Main.hs line 47

This project was made as an entry to the INF1A functional programming competition at the University of Edinburgh
