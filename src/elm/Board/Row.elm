module Board.Row exposing (..)

import Color exposing (Color)
import Board.Position.ColumnIndex as ColumnIndex exposing (ColumnIndex)
import List.Extra as Lextra
import Maybe.Extra


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
