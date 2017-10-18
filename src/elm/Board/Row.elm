module Board.Row exposing (..)

import Color exposing (Color)
import Board.Position exposing (ColumnIndex(..), unwrapColumnIndex)
import List.Extra as Lextra


type Row
    = Row (List (Maybe Color))


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


colorExists : Maybe Color -> Bool
colorExists maybeColor =
    case maybeColor of
        Just _ ->
            True

        _ ->
            False


slideRight : Row -> Row
slideRight (Row row) =
    let
        existingPieces =
            row
                |> List.filter colorExists

        emptySpaces =
            row
                |> List.filter (not << colorExists)
    in
        List.append emptySpaces existingPieces
            |> Row


slideLeft : Row -> Row
slideLeft (Row row) =
    let
        existingPieces =
            row
                |> List.filter colorExists

        emptySpaces =
            row
                |> List.filter (not << colorExists)
    in
        List.append emptySpaces existingPieces
            |> Row
