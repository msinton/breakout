# breakout

Using [this guide to making pong](http://andrew.gibiansky.com/blog/haskell/haskell-gloss/)
as a starting point, we create the classic game Breakout.

This is an experiment to see if we can all learn Haskell and collaborate
together without a rigid process in place!

Commits
-------

Policy:

    Use git pull rebase always.

    Do commit directly to master.

    Think about what tasks you pick up and any conflicts that are likely.

    Small commits are preferred.


Tasks
-----
Assign yourself to a task by prefixing with your name.

- tidy code:
    - separate collision logic
    - generalise collision fns/ use function composition to get DRY
    - [Matt SH] separate grid properties - convert to a data type

- improve collisions with blocks/paddle
    - detect collisions on corners
    - take velocity into account
    - change vx and vy proportionally wrt corner collisions
    - allow size of paddle to vary independently to size of blocks

- score
    - add to game state
    - increase for each block

- complete collisions with blocks
    - collide on all sides
    - blocks disappear
    - score increases

- dying
    - restarts game after next key press


- tests