module Board.Rows exposing (..)

import Color exposing (Color)
import Board.Position.RowIndex as RowIndex exposing (RowIndex)
import Board.Position.ColumnIndex as ColumnIndex exposing (ColumnIndex)
import Board.Row as Row exposing (Row)
import List.Extra as Lextra


type Rows
    = Rows (List Row)


unwrap : Rows -> List Row
unwrap (Rows rows) =
    rows


fromList : List (List (Maybe Color)) -> Rows
fromList list =
    list
        |> List.map Row.fromMaybeColors
        |> Rows


toList : Rows -> List (List (Maybe Color))
toList (Rows rows) =
    rows
        |> List.map Row.unwrap


setRow : RowIndex -> Rows -> Row -> Maybe Rows
setRow rowIndex (Rows rows) row =
    let
        ri =
            RowIndex.unwrap rowIndex
    in
        rows
            |> Lextra.setAt ri row
            |> Maybe.map Rows


getRow : RowIndex -> Rows -> Maybe Row
getRow rowIndex (Rows rows) =
    let
        ri =
            RowIndex.unwrap rowIndex
    in
        rows
            |> Lextra.getAt ri


removeBlock : List ( RowIndex, List ColumnIndex ) -> Rows -> Rows
removeBlock groupedColumnIndices rows =
    case Lextra.uncons groupedColumnIndices of
        Just ( ( rowIndex, columnIndices ), restRowGroups ) ->
            getRow rowIndex rows
                |> Maybe.map (Row.removePieces columnIndices)
                |> Maybe.andThen (setRow rowIndex rows)
                |> Maybe.withDefault rows
                |> removeBlock restRowGroups

        Nothing ->
            rows


slideDownLeft : Rows -> Rows
slideDownLeft =
    toList
        >> Lextra.transpose
        >> List.map (Row.fromMaybeColors >> Row.slideRight)
        >> List.filter (Row.isNotEmpty)
        >> List.map Row.unwrap
        >> Lextra.transpose
        >> List.map Row.fromMaybeColors
        >> Rows


indexRow : Int -> Row -> ( RowIndex, Row )
indexRow ri row =
    ( RowIndex.fromInt ri, row )
