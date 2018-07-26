module Board.Row exposing (..)

import Color exposing (Color)
import Board.Properties as Properties exposing (PieceLength)
import Board.Position as Position exposing (Position)
import Board.Piece as Piece
import Board.Position.ColumnIndex as ColumnIndex exposing (ColumnIndex)
import Board.Position.RowIndex as RowIndex exposing (RowIndex)
import Util.Tuple as Tuple
import List.Extra as Lextra
import Maybe.Extra
import Svg exposing (Svg)


type Row
    = Row (List (Maybe Color))


fromMaybeColors : List (Maybe Color) -> Row
fromMaybeColors colorsList =
    Row colorsList


{-| For a board row, remove pieces that correspond to the given column indices.

This function recursively sets the row's pieces to nothing until it runs out of column indices.

-}
removePieces : List ColumnIndex -> Row -> Row
removePieces columnIndices row =
    case Lextra.uncons columnIndices of
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
        Lextra.setAt (ColumnIndex.unwrap columnIndex) Nothing colors
            |> Maybe.withDefault colors
            |> Row


getColumnPiece : ColumnIndex -> Row -> Maybe Color
getColumnPiece columnIndex =
    let
        ci =
            ColumnIndex.unwrap columnIndex
    in
        unwrap
            >> Lextra.getAt ci
            >> Maybe.Extra.join


unwrap : Row -> List (Maybe Color)
unwrap (Row row) =
    row


colorExists : Maybe Color -> Bool
colorExists maybeColor =
    case maybeColor of
        Just _ ->
            True

        _ ->
            False


{-| Slide remaining color pieces to the end of the row.
-}
slideRight : Row -> Row
slideRight (Row row) =
    let
        existingPieces =
            row |> List.filter colorExists

        emptySpaces =
            row |> List.filter (not << colorExists)
    in
        List.append emptySpaces existingPieces
            |> Row


isNotEmpty : Row -> Bool
isNotEmpty =
    let
        isEmpty : Row -> Bool
        isEmpty =
            unwrap
                >> List.filter colorExists
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
                        |> (Piece.create color pieceLength)
                        |> Piece.draw clickPieceMsg
                )
