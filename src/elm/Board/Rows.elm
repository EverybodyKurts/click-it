module Board.Rows exposing (..)

import Color exposing (Color)
import Board.Position exposing (ColumnIndex(..), RowIndex(..))
import Board.Row as Row exposing (Row(..))
import List.Extra as Lextra


type Rows
    = Rows (List Row)


unwrap : Rows -> List Row
unwrap (Rows rows) =
    rows


fromList : List (List (Maybe Color)) -> Rows
fromList list =
    list
        |> List.map Row
        |> Rows


toList : Rows -> List (List (Maybe Color))
toList (Rows rows) =
    rows
        |> List.map Row.unwrap


setRow : RowIndex -> List Row -> Row -> Maybe (List Row)
setRow (RowIndex rowIndex) rows row =
    Lextra.setAt rowIndex row rows


removeBlock : List ( RowIndex, List ColumnIndex ) -> Rows -> Rows
removeBlock groupedColumnIndices (Rows rows) =
    case Lextra.uncons groupedColumnIndices of
        Just ( ( RowIndex rowIndex, columnIndices ), restRowGroups ) ->
            Lextra.getAt rowIndex rows
                |> Maybe.map (Row.removePieces columnIndices)
                |> Maybe.andThen (setRow (RowIndex rowIndex) rows)
                |> Maybe.withDefault rows
                |> Rows
                |> removeBlock restRowGroups

        Nothing ->
            (Rows rows)


slideDown : Rows -> Rows
slideDown =
    toList
        >> Lextra.transpose
        >> List.map (Row >> Row.slideRight >> Row.unwrap)
        >> Lextra.transpose
        >> List.map Row
        >> Rows
