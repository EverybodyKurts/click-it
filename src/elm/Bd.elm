module Bd exposing (..)

import Array exposing (Array)
import Random exposing (Generator)
import Random.Array
import Color exposing (Color)
import List.Extra as Lextra
import Board.Properties exposing (Properties, RowIndex(..), ColumnIndex(..), Position(..))
import Maybe.Extra


type Row
    = Row (List (Maybe Color))


type Rows
    = Rows (List Row)


type Board
    = Board Rows


type BoardRows
    = BoardRows List (List (Maybe Color))



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


getPiece : Board -> Position -> Maybe Color
getPiece board (Position ( RowIndex rowIndex, ColumnIndex columnIndex )) =
    board
        |> to2dList
        |> Lextra.getAt rowIndex
        |> Maybe.andThen (Lextra.getAt columnIndex)
        |> Maybe.Extra.join


neighborPositions : Position -> List Position
neighborPositions (Position ( RowIndex rowIndex, ColumnIndex columnIndex )) =
    [ Position ( RowIndex rowIndex, ColumnIndex (columnIndex - 1) ) -- north
    , Position ( RowIndex rowIndex, ColumnIndex (columnIndex + 1) ) -- south
    , Position ( RowIndex (rowIndex - 1), ColumnIndex columnIndex ) -- east
    , Position ( RowIndex (rowIndex + 1), ColumnIndex columnIndex ) -- west
    ]


equivNeighborPositions : Board -> Color -> Position -> List Position
equivNeighborPositions board color position =
    let
        keepPositionIfSameColor : Board -> Color -> Position -> Maybe Position
        keepPositionIfSameColor board color position =
            getPiece board position
                |> Maybe.andThen
                    (\pieceColor ->
                        if pieceColor == color then
                            Just position
                        else
                            Nothing
                    )
    in
        neighborPositions position
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


positionRowIndex : Position -> Int
positionRowIndex (Position ( RowIndex r, _ )) =
    r


findBlockAt : Board -> Position -> List Position
findBlockAt board position =
    case getPiece board position of
        Just color ->
            let
                destinations =
                    Destinations (equivNeighborPositions board color position)

                colorBlock =
                    ColorBlock [ position ]
            in
                fcb color board colorBlock destinations
                    |> List.sortBy positionRowIndex

        Nothing ->
            []



-- UPDATING THE BOARD


removePiece : Board -> Position -> Board
removePiece (Board boardRows) (Position ( RowIndex rowIndex, ColumnIndex columnIndex )) =
    let
        (Rows rows) =
            boardRows

        setRow : Int -> List Row -> Row -> Maybe (List Row)
        setRow rowIndex rows row =
            Lextra.setAt rowIndex row rows
    in
        rows
            |> Lextra.getAt rowIndex
            |> Maybe.map unwrapRow
            |> Maybe.andThen (Lextra.setAt columnIndex Nothing)
            |> Maybe.map Row
            |> Maybe.andThen (setRow rowIndex rows)
            |> Maybe.map Rows
            |> Maybe.map Board
            |> Maybe.withDefault (Board boardRows)


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


positionToColumnIndex : Position -> Int
positionToColumnIndex (Position ( RowIndex r, ColumnIndex c )) =
    c


positionsHaveSameRow : Position -> Position -> Bool
positionsHaveSameRow pos1 pos2 =
    let
        (Position ( RowIndex a, _ )) =
            pos1

        (Position ( RowIndex b, _ )) =
            pos2
    in
        a == b


rb boardRows indexedColorBlockRows =
    if List.isEmpty indexedColorBlockRows then
        boardRows
    else
        []


removeBlock (Board boardRows) sortedColorBlock =
    let
        indexedColorBlockRows =
            sortedColorBlock
                |> Lextra.groupWhile positionsHaveSameRow
                |> List.indexedMap (,)
    in
        indexedColorBlockRows



-- colorBlockRows
--     |> List.indexedMap (,)
--     |> List.map
--         (\( rowIndex, blockRow ) ->
--             let
--                 columnIndices =
--                     List.map positionToColumnIndex blockRow
--             in
--                 ( rowIndex, columnIndices )
--         )
-- removeBlockAt board position =
--     let
--         colorBlock =
--             findColorBlock board position
--     in
--         if List.length colorBlock >= 3 then
--             colorBlock
--         else
--             []
