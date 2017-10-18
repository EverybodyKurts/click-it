module Board.Rows exposing (..)

import Board.Row as Row exposing (Row(..))


type Rows
    = Rows (List Row)


unwrap : Rows -> List Row
unwrap (Rows rows) =
    rows
