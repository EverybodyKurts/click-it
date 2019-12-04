module Board exposing (..)

import Array exposing (Array)
import Board.Position as Position exposing (Position)
import Board.Properties as Properties exposing (NumColumns(..), PieceLength(..), Properties)
import Board.Row as Row exposing (Row(..))
import Board.Rows as Rows exposing (Rows(..))
import Color exposing (Color)
import Html exposing (Html)
import List.Extra as List
import Random exposing (Generator)
import Random.Array
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, width)


type Board
    = Board Rows


type ColorBlock
    = ColorBlock (List Position)


type Destinations
    = Destinations (List Position)



-- BOARD INITIALIZATION


generate : (Board -> msg) -> Generator Board -> Cmd msg
generate =
    Random.generate


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
colorsToBoard numColumns =
    List.groupsOf numColumns
        >> Rows.fromList
        >> Board


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


{-| Initialize a board based on specified properties
-}
init : Properties -> Generator Board
init properties =
    let
        ( rows, columns, colors, pieceLength ) =
            Properties.raw properties
    in
    genPaletteThenBoard rows columns colors


{-| Generate the default board
-}
default : Generator Board
default =
    init Properties.default



-- ACCESSING THE BOARD


unwrap : Board -> Rows
unwrap (Board rows) =
    rows


pieceAt : Position -> Board -> Maybe Color
pieceAt position =
    let
        rowIndex =
            Position.rowIndex position

        columnIndex =
            Position.columnIndex position
    in
    unwrap
        >> Rows.getRow rowIndex
        >> Maybe.andThen (Row.getColumnPiece columnIndex)


pieceExistsAt : Position -> Board -> Bool
pieceExistsAt position board =
    case pieceAt position board of
        Just _ ->
            True

        Nothing ->
            False


neighborsWithSameColor : Board -> Position -> List Position
neighborsWithSameColor board position =
    case pieceAt position board of
        Just color ->
            let
                keepPositionIfSameColor : Board -> Color -> Position -> Maybe Position
                keepPositionIfSameColor board color position =
                    board
                        |> pieceAt position
                        |> Maybe.andThen
                            (\pieceColor ->
                                if pieceColor == color then
                                    Just position

                                else
                                    Nothing
                            )
            in
            Position.neighbors position
                |> List.filterMap (keepPositionIfSameColor board color)

        Nothing ->
            []


fcb : Board -> ColorBlock -> Destinations -> List Position
fcb board (ColorBlock colorBlock) (Destinations destinations) =
    case List.uncons destinations of
        Just ( nextPosition, restDestinations ) ->
            let
                updatedColorBlock =
                    nextPosition :: colorBlock

                alreadyAdded =
                    \a -> List.member a updatedColorBlock
            in
            nextPosition
                |> neighborsWithSameColor board
                |> List.filterNot alreadyAdded
                |> List.append restDestinations
                |> Destinations
                |> fcb board (ColorBlock updatedColorBlock)

        Nothing ->
            colorBlock


findBlockAt : Board -> Position -> List Position
findBlockAt board position =
    case pieceExistsAt position board of
        True ->
            let
                destinations =
                    Destinations (neighborsWithSameColor board position)

                colorBlock =
                    ColorBlock [ position ]
            in
            fcb board colorBlock destinations
                |> Position.sort

        False ->
            []



-- UPDATING THE BOARD


minimumBlockSize : Int
minimumBlockSize =
    3


toRowsList : Board -> List Row
toRowsList =
    unwrap
        >> Rows.unwrap


{-| Remove the block of pieces from the board.
-}
removeBlock : Board -> List Position -> Board
removeBlock (Board rows) colorBlock =
    let
        blockGroupedByRow =
            Position.groupColumnIndicesByRow colorBlock
    in
    Rows.removeBlock blockGroupedByRow rows
        |> Board


{-| Remove the block from the board if it's at least the minimum specified # of pieces.
-}
removeBlockIfMinSize : Int -> Board -> List Position -> Board
removeBlockIfMinSize minSize board positions =
    if List.length positions >= minSize then
        removeBlock board positions
            |> unwrap
            |> Rows.slideDownLeft
            |> Board

    else
        board


removeBlockAt : Board -> Position -> Board
removeBlockAt board =
    findBlockAt board
        >> removeBlockIfMinSize 3 board



-- VIEW --


draw : PieceLength -> (Position -> msg) -> Board -> List (Svg msg)
draw pieceLength clickPieceMsg =
    unwrap
        >> Rows.draw pieceLength clickPieceMsg


view : (Position -> msg) -> Properties -> Board -> List (Html msg)
view clickPieceMsg ({ numRows, numColumns, numColors } as properties) board =
    let
        boardWidth =
            Properties.width properties

        boardHeight =
            Properties.height properties

        drawnBoard =
            board
                |> draw properties.pieceLength clickPieceMsg
    in
    [ svg
        [ width (toString boardWidth), height (toString boardHeight) ]
        drawnBoard
    ]
