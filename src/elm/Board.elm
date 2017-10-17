module Board exposing (..)

import Array exposing (Array)
import Random exposing (Generator)
import Random.Array
import Color exposing (Color)
import List.Extra as Lextra
import Board.Properties exposing (Properties, PieceLength(..))
import Board.Position as Position exposing (RowIndex(..), ColumnIndex(..), Position(..))
import Maybe.Extra


type Row
    = Row (List (Maybe Color))


type Rows
    = Rows (List Row)


type Board
    = Board Rows


type BoardRows
    = BoardRows List (List (Maybe Color))


type XCoord
    = XCoord Int


type YCoord
    = YCoord Int



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


{-| Initialize a board based on specified properties
-}
init : Properties -> Generator Board
init properties =
    let
        ( rows, columns, colors, pieceLength ) =
            (Board.Properties.raw properties)
    in
        genPaletteThenBoard rows columns colors


{-| Generate the default board
-}
default : Generator Board
default =
    init Board.Properties.default



-- ACCESSING THE BOARD


unwrapBoard : Board -> Rows
unwrapBoard (Board rows) =
    rows


unwrapRows : Rows -> List Row
unwrapRows (Rows rows) =
    rows


unwrapRow : Row -> List (Maybe Color)
unwrapRow (Row row) =
    row


to2dList : Board -> List (List (Maybe Color))
to2dList =
    unwrapBoard
        >> unwrapRows
        >> List.map unwrapRow


from2dList : List (List (Maybe Color)) -> Board
from2dList =
    List.map Row
        >> Rows
        >> Board


getPiece : Position -> Board -> Maybe Color
getPiece (Position ( RowIndex rowIndex, ColumnIndex columnIndex )) =
    to2dList
        >> Lextra.getAt rowIndex
        >> Maybe.andThen (Lextra.getAt columnIndex)
        >> Maybe.Extra.join


equivNeighborPositions : Board -> Color -> Position -> List Position
equivNeighborPositions board color position =
    let
        keepPositionIfSameColor : Board -> Color -> Position -> Maybe Position
        keepPositionIfSameColor board color position =
            getPiece position board
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


type ColorBlock
    = ColorBlock (List Position)


type Destinations
    = Destinations (List Position)


fcb : Color -> Board -> ColorBlock -> Destinations -> List Position
fcb color board (ColorBlock colorBlock) (Destinations destinations) =
    case Lextra.uncons destinations of
        Just ( blockPosition, restDestinations ) ->
            let
                updatedColorBlock =
                    blockPosition :: colorBlock
            in
                blockPosition
                    |> (equivNeighborPositions board color)
                    |> Lextra.filterNot (flip List.member updatedColorBlock)
                    |> List.append restDestinations
                    |> Destinations
                    |> (fcb color board (ColorBlock updatedColorBlock))

        Nothing ->
            colorBlock


findBlockAt : Board -> Position -> List Position
findBlockAt board position =
    case getPiece position board of
        Just color ->
            let
                destinations =
                    Destinations (equivNeighborPositions board color position)

                colorBlock =
                    ColorBlock [ position ]
            in
                fcb color board colorBlock destinations
                    |> Position.sort

        Nothing ->
            []



-- UPDATING THE BOARD


unwrapColumnIndex : ColumnIndex -> Int
unwrapColumnIndex (ColumnIndex ci) =
    ci


rpr : List (Maybe a) -> List Int -> List (Maybe a)
rpr row remainingColumnIndices =
    case Lextra.uncons remainingColumnIndices of
        Just ( columnIndex, restColumnIndices ) ->
            let
                updatedRow =
                    Lextra.setAt columnIndex Nothing row
                        |> Maybe.withDefault row
            in
                rpr updatedRow restColumnIndices

        Nothing ->
            row


removePiecesInRow : Row -> List ColumnIndex -> Row
removePiecesInRow (Row colors) columnIndices =
    let
        remainingColumnIndices =
            columnIndices
                |> List.map unwrapColumnIndex
    in
        rpr colors remainingColumnIndices
            |> Row


toRowsList : Board -> List Row
toRowsList =
    unwrapBoard
        >> unwrapRows


rb rowsList indexedColorBlockRows =
    case Lextra.uncons indexedColorBlockRows of
        Just ( ( rowIndex, columnIndices ), restRows ) ->
            let
                row =
                    Lextra.getAt rowIndex rowsList
            in
                rowsList

        Nothing ->
            rowsList


removeBlock board sortedColorBlock =
    let
        indexedColorBlockRows =
            sortedColorBlock
                |> Position.sort
                |> Lextra.groupWhile Position.haveSameRow
                |> List.indexedMap (,)
    in
        indexedColorBlockRows


xCoord : PieceLength -> ColumnIndex -> XCoord
xCoord (PieceLength pieceLength) (ColumnIndex columnIndex) =
    XCoord (pieceLength * columnIndex)


yCoord : PieceLength -> RowIndex -> YCoord
yCoord (PieceLength pieceLength) (RowIndex rowIndex) =
    YCoord (pieceLength * rowIndex)
