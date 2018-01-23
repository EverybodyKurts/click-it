module Board.Row exposing (..)

import Color exposing (Color)
import Board.Position exposing (ColumnIndex(..), unwrapColumnIndex)
import List.Extra as Lextra


type Row
    = Row (List (Maybe Color))


{-| For a board row, remove pieces that correspond to the given column indices.

This function recursively sets the row's pieces to nothing until it runs out of column indices.

-}
removePieces : List ColumnIndex -> Row -> Row
removePieces columnIndices (Row row) =
    case Lextra.uncons columnIndices of
        Just ( ColumnIndex ci, restColumnIndices ) ->
            Lextra.setAt ci Nothing row
                |> Maybe.withDefault row
                |> Row
                |> removePieces restColumnIndices

        Nothing ->
            (Row row)


unwrap : Row -> List (Maybe Color)
unwrap (Row row) =
    row


{-| Slide remaining color pieces to the end of the row.
-}
slideRight : Row -> Row
slideRight (Row row) =
    let
        colorExists : Maybe Color -> Bool
        colorExists maybeColor =
            case maybeColor of
                Just _ ->
                    True

                _ ->
                    False

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
