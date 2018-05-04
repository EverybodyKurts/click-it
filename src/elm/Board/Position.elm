module Board.Position exposing (..)

import Dict exposing (Dict(..))
import Dict.Extra as Dixtra
import Board.Position.RowIndex as RowIndex exposing (RowIndex)
import Board.Position.ColumnIndex as ColumnIndex exposing (ColumnIndex)


type Position
    = Position ( RowIndex, ColumnIndex )


toTuple : Position -> ( Int, Int )
toTuple (Position ( rowIndex, columnIndex )) =
    ( RowIndex.unwrap rowIndex, ColumnIndex.unwrap columnIndex )


fromTuple : ( Int, Int ) -> Position
fromTuple ( r, c ) =
    Position ( RowIndex.fromInt r, ColumnIndex.fromInt c )


fromIndices : RowIndex -> ColumnIndex -> Position
fromIndices rowIndex columnIndex =
    Position ( rowIndex, columnIndex )


sort : List Position -> List Position
sort =
    List.sortBy toTuple


{-| Return true if the positions are in the same row.
-}
haveSameRow : Position -> Position -> Bool
haveSameRow pos1 pos2 =
    let
        (Position ( ri1, _ )) =
            pos1

        (Position ( ri2, _ )) =
            pos2
    in
        RowIndex.equals ri1 ri2


{-| Return the position's neighbors: north, south, east, west
-}
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


columnIndex : Position -> ColumnIndex
columnIndex (Position ( _, columnIndex )) =
    columnIndex


columnIndices : List Position -> List ColumnIndex
columnIndices =
    List.map columnIndex


rowIndex : Position -> RowIndex
rowIndex (Position ( rowIndex, _ )) =
    rowIndex


groupByRow : List Position -> List ( RowIndex, List Position )
groupByRow =
    Dixtra.groupBy (rowIndex >> RowIndex.unwrap)
        >> Dict.toList
        >> List.map (\( ri, positions ) -> ( RowIndex.fromInt ri, positions ))


groupColumnIndicesByRow : List Position -> List ( RowIndex, List ColumnIndex )
groupColumnIndicesByRow positions =
    let
        toRowGroupedColumnIndices: ( RowIndex, List Position ) -> ( RowIndex, List ColumnIndex )
        toRowGroupedColumnIndices ( rowIndex, rowPositions ) =
            ( rowIndex
            , rowPositions |> columnIndices
            )
    in
        groupByRow positions
            |> List.map toRowGroupedColumnIndices
