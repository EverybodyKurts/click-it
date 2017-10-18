module Board.Rows exposing (..)

import Color exposing (Color)
import Board.Row as Row exposing (Row(..))


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
