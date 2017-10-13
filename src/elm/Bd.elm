module Bd exposing (..)

import Array exposing (Array)
import Random exposing (Generator)
import Random.Array
import Color exposing (Color)
import List.Extra as Lextra


type Row
    = Row (List (Maybe Color))


type Rows
    = Rows (List Row)


type Board
    = Board Rows



-- BOARD INITIALIZATION


{-| Generate a random color
-}
genRandomColor : Generator Color
genRandomColor =
    Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)


{-| Generate a color palette to use in the board
-}
genColorPalette : Int -> Generator (Array Color)
genColorPalette numColors =
    Random.Array.array numColors genRandomColor


colorsToRows : Int -> List (Maybe Color) -> Rows
colorsToRows numColumns boardColors =
    boardColors
        |> Lextra.groupsOf numColumns
        |> List.map Row
        |> Rows


{-| Generate the board colors
-}
genBoardColors : Int -> Int -> Array Color -> Generator Rows
genBoardColors numRows numColumns colorPalette =
    Random.list (numRows * numColumns) (Random.Array.sample colorPalette)
        |> Random.map (colorsToRows numColumns)


{-| Generate color palette then board colors
-}
genPaletteThenBoard : Int -> Int -> Int -> Generator Rows
genPaletteThenBoard numRows numColumns numColors =
    genColorPalette numColors
        |> Random.andThen (genBoardColors numRows numColumns)



-- splitBoardColors numRows numColumns boardColors =
-- genPaletteThenBoardColors numRows numColumns numColors =
--     genColorPalette numColors
--         |> Random.andThen (genBoardColors numRows numColumns)
