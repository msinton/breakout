module LayoutDimensions (
    height, width,

    borderSize,

    blockWidth, blockHeight,
    gridWidth, gridHeight

) where


blockWidth, blockHeight, borderSize :: Float
blockWidth = 70
blockHeight = 22
borderSize = 100

gridWidth, gridHeight, width, height :: Float
gridWidth = 7 * blockWidth
gridHeight = 500
width = gridWidth + 2 * borderSize
height = gridHeight + 2 * borderSize
