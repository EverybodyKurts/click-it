module Board.Position exposing (..)

import List.Extra as Lextra
import Dict exposing (Dict(..))
import Dict.Extra as Dixtra


type RowIndex
    = RowIndex Int


type ColumnIndex
    = ColumnIndex Int


type Position
    = Position ( RowIndex, ColumnIndex )


unwrapRow : Position -> Int
unwrapRow (Position ( RowIndex r, _ )) =
    r


unwrapColumnIndex : ColumnIndex -> Int
unwrapColumnIndex (ColumnIndex c) =
    c


toTuple : Position -> ( Int, Int )
toTuple (Position ( RowIndex r, ColumnIndex c )) =
    ( r, c )


fromTuple : ( Int, Int ) -> Position
fromTuple ( r, c ) =
    Position ( RowIndex r, ColumnIndex c )


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


neighbors : Position -> List Position
neighbors position =
    let
        ( r, c ) =
            toTuple position
    in
        [ ( r, c - 1 ) -- north
        , ( r, c + 1 ) -- south
        , ( r - 1, c ) -- east
        , ( r + 1, c ) -- west
        ]
            |> List.map fromTuple


toDict : List Position -> Dict Int (List Position)
toDict =
    Dixtra.groupBy unwrapRow


{-| Group positions by row
-}
groupByRow : List Position -> List ( Int, List Position )
groupByRow =
    toDict >> Dict.toList


columnIndex : Position -> ColumnIndex
columnIndex (Position ( _, columnIndex )) =
    columnIndex


groupColumnIndicesByRow : List Position -> List ( RowIndex, List ColumnIndex )
groupColumnIndicesByRow positions =
    let
        positionsToColumnIndices : ( Int, List Position ) -> ( RowIndex, List ColumnIndex )
        positionsToColumnIndices ( rowIndex, rowPositions ) =
            ( RowIndex rowIndex
            , rowPositions |> List.map columnIndex
            )
    in
        groupByRow positions
            |> List.map positionsToColumnIndices
