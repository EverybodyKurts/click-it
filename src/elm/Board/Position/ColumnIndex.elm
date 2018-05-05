module Board.Position.ColumnIndex exposing (..)


type ColumnIndex
    = ColumnIndex Int


fromInt : Int -> ColumnIndex
fromInt ci =
    ColumnIndex ci


unwrap : ColumnIndex -> Int
unwrap (ColumnIndex ci) =
    ci
