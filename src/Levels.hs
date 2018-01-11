module Levels (
    levels
) where

levels :: [[(Float, Float)]]
levels = [
    [(0, 0), (0, 1), (0, 2), (1, 1), (2, 2), (3, 3), (4, 4), (4, 6)],
    [(1, 0), (3, 0), (5, 0), (6, 0), (7, 0),
     (1, 1), (2, 1), (3, 1), (6, 1),
     (1, 2), (3, 2), (5, 2), (6, 2), (7, 2)]
    ]
