module Bd exposing (..)

import Array exposing (Array)
import Random exposing (Generator)
import Random.Array
import Color exposing (Color)
import List.Extra as Lextra
import Board.Properties exposing (Properties)


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


colorsToBoard : Int -> List (Maybe Color) -> Board
colorsToBoard numColumns boardColors =
    boardColors
        |> Lextra.groupsOf numColumns
        |> List.map Row
        |> Rows
        |> Board


{-| Generate the board & its colors
-}
genBoard : Int -> Int -> Array Color -> Generator Board
genBoard numRows numColumns colorPalette =
    Random.list (numRows * numColumns) (Random.Array.sample colorPalette)
        |> Random.map (colorsToBoard numColumns)


{-| Generate color palette & then the board and its colors
-}
genPaletteThenBoard : Int -> Int -> Int -> Generator Board
genPaletteThenBoard numRows numColumns numColors =
    genColorPalette numColors
        |> Random.andThen (genBoard numRows numColumns)


default : Generator Board
default =
    let
        ( rows, columns, colors, pieceLength ) =
            (Board.Properties.raw Board.Properties.default)
    in
        genPaletteThenBoard rows columns colors
