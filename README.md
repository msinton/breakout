# breakout

Using [this guide to making pong](http://andrew.gibiansky.com/blog/haskell/haskell-gloss/)
as a starting point, we create the classic game Breakout.

This is an experiment to see if we can all learn Haskell and collaborate
together without a rigid process in place!

Commits
-------

Policy:

    Use `git pull --rebase` always.

    Do commit directly to master.

    Think about what tasks you pick up and any conflicts that are likely.

    Small commits are preferred.


Tasks
-----
Assign yourself to a task by prefixing with your name.

- tidy code:
    - [Matt SH] separate collision logic
    - generalise collision fns/ use function composition to get DRY
    - encapsulate all dimensions in a data record?

- improve collisions with blocks/paddle
    - [John R] detect collisions on corners
    - take velocity into account
    - change vx and vy proportionally wrt corner collisions
    - allow size of paddle to vary independently to size of blocks

- score
    - add to game state
    - increase for each block

- complete collisions with blocks
    - [John R] blocks disappear
    - score increases

- blocks
    - different coloured blocks earn different points and require multiple hits
    - interpreter - read block data from file to generate level
    - obstructions - blocks that do not disappear but do not need to be cleared

- dying
    - restarts game after next key press

- levels
    - when clear all blocks progress to next level

- scoreboard
    - display highscores
    - enter name for highscore

- tests
