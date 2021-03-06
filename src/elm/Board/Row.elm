module Board.Row exposing (..)

import Board.Piece as Piece
import Board.Position as Position exposing (Position)
import Board.Position.ColumnIndex as ColumnIndex exposing (ColumnIndex)
import Board.Position.RowIndex exposing (RowIndex)
import Board.Properties exposing (PieceLength)
import Color exposing (Color)
import List.Extra as List
import Maybe.Extra as Maybe
import Svg exposing (Svg)
import Util.Tuple as Tuple
import Util.Color


type Row
    = Row (List (Maybe Color))


fromMaybeColors : List (Maybe Color) -> Row
fromMaybeColors =
    Row 


{-| For a board row, remove pieces that correspond to the given column indices.

This function recursively sets the row's pieces to nothing until it runs out of column indices.

-}
removePieces : List ColumnIndex -> Row -> Row
removePieces columnIndices row =
    case List.uncons columnIndices of
        Just ( columnIndex, restColumnIndices ) ->
            row
                |> removeColumnPiece columnIndex
                |> removePieces restColumnIndices

        Nothing ->
            row


{-| Remove a piece from a specific column in the row. ̰
-}
removeColumnPiece : ColumnIndex -> Row -> Row
removeColumnPiece columnIndex row =
    let
        (Row colors) =
            row
    in
    colors
        |> List.setAt (ColumnIndex.unwrap columnIndex) Nothing
        |> Row


getAt : ColumnIndex -> Row -> Maybe Color
getAt columnIndex =
    let
        ci =
            ColumnIndex.unwrap columnIndex
    in
    toList
        >> List.getAt ci
        >> Maybe.join


toList : Row -> List (Maybe Color)
toList (Row row) =
    row


{-| Slide remaining color pieces to the end of the row.
-}
slideRight : Row -> Row
slideRight (Row row) =
    let
        existingPieces =
            row |> List.filter Util.Color.exists

        emptySpaces =
            row |> List.filter (not << Util.Color.exists)
    in
    List.append emptySpaces existingPieces
        |> Row


isNotEmpty : Row -> Bool
isNotEmpty =
    let
        isEmpty : Row -> Bool
        isEmpty =
            toList
                >> List.filter Util.Color.exists
                >> List.isEmpty
    in
    isEmpty >> not


draw : PieceLength -> (Position -> msg) -> ( RowIndex, Row ) -> List (Svg msg)
draw pieceLength clickPieceMsg ( rowIndex, Row row ) =
    let
        keepExistingIndexedColors : ( ColumnIndex, Maybe Color ) -> Maybe ( ColumnIndex, Color )
        keepExistingIndexedColors ( index, maybeColor ) =
            maybeColor
                |> Maybe.map (Tuple.create index)

        indexedColumn : Int -> Maybe Color -> ( ColumnIndex, Maybe Color )
        indexedColumn index maybeColor =
            ( ColumnIndex.fromInt index, maybeColor )
    in
    row
        |> List.indexedMap indexedColumn
        |> List.filterMap keepExistingIndexedColors
        |> List.map
            (\( columnIndex, color ) ->
                Position.create rowIndex columnIndex
                    |> Piece.create color pieceLength
                    |> Piece.draw clickPieceMsg
            )
