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

- bugs
    - ball can get stuck inside paddle when entering from corner and moving paddle

- tidy code:
    - separate collision logic
    - generalise collision fns/ use function composition to get DRY
    - encapsulate all dimensions in a data record?

- improve collisions with blocks/paddle
    - [Matt S-H] take velocity into account
    - change vx and vy proportionally wrt corner collisions
    - allow size of paddle to vary independently to size of blocks

- blocks
    - different coloured blocks earn different points and require multiple hits
    - interpreter - read block data from file to generate level
    - obstructions - blocks that do not disappear but do not need to be cleared

- dying
    - restarts game after next key press

- scoreboard
    - display highscores
    - enter name for highscore

- tests
