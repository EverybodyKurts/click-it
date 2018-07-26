module Board.Position.RowIndex exposing (..)


type RowIndex
    = RowIndex Int


fromInt : Int -> RowIndex
fromInt ri =
    RowIndex ri


unwrap : RowIndex -> Int
unwrap (RowIndex ri) =
    ri


equals : RowIndex -> RowIndex -> Bool
equals (RowIndex ri1) (RowIndex ri2) =
    ri1 == ri2
