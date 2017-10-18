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
