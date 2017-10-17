module Board.Position exposing (..)


type RowIndex
    = RowIndex Int


type ColumnIndex
    = ColumnIndex Int


type Position
    = Position ( RowIndex, ColumnIndex )


unwrapRow : Position -> Int
unwrapRow (Position ( RowIndex r, _ )) =
    r


toTuple : Position -> ( Int, Int )
toTuple (Position ( RowIndex r, ColumnIndex c )) =
    ( r, c )


sort : List Position -> List Position
sort =
    List.sortBy toTuple


haveSameRow : Position -> Position -> Bool
haveSameRow pos1 pos2 =
    let
        (Position ( RowIndex a, _ )) =
            pos1

        (Position ( RowIndex b, _ )) =
            pos2
    in
        a == b
