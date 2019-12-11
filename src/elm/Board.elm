module Board exposing (..)

import Array exposing (Array)
import Board.Position as Position exposing (Position)
import Board.Position.ColumnIndex exposing (ColumnIndex)
import Board.Position.RowIndex as RowIndex exposing (RowIndex)
import Board.Properties as Properties exposing (PieceLength(..), Properties)
import Board.Row as Row exposing (Row)
import Color exposing (Color)
import Html exposing (Html)
import List.Extra as List
import Random exposing (Generator)
import Random.Array
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, width)
import Util.Color


type Board
    = Board (List Row)


type ColorBlock
    = ColorBlock (List Position)


type Destinations
    = Destinations (List Position)



-- BOARD INITIALIZATION


fromList : List (List (Maybe Color)) -> Board
fromList =
    List.map Row.fromMaybeColors
        >> Board


colorsToBoard : Int -> List (Maybe Color) -> Board
colorsToBoard numColumns =
    List.groupsOf numColumns
        >> fromList


generate : Properties -> Generator Board
generate properties =
    let
        genBoard : Int -> Int -> Array Color -> Generator Board
        genBoard numRows numColumns =
            Random.Array.sample
                >> Random.list (numRows * numColumns)
                >> Random.map (colorsToBoard numColumns)

        rows =
            Properties.rowInt properties

        columns =
            Properties.columnInt properties

        colors =
            Properties.colorInt properties
    in
    Util.Color.randomArray colors
        |> Random.andThen (genBoard rows columns)


{-| Generate the default board
-}
default : Generator Board
default =
    generate Properties.default



-- ACCESSING THE BOARD


toList : Board -> List Row
toList (Board rows) =
    rows


to2dList : Board -> List (List (Maybe Color))
to2dList =
    toList
        >> List.map Row.toList


pieceAt : Position -> Board -> Maybe Color
pieceAt position =
    let
        rowIndex =
            Position.rowIndex position

        columnIndex =
            Position.columnIndex position
    in
    getRow rowIndex
        >> Maybe.andThen (Row.getAt columnIndex)


pieceExistsAt : Position -> Board -> Bool
pieceExistsAt position board =
    case pieceAt position board of
        Just _ ->
            True

        Nothing ->
            False


neighborsWithSameColor : Board -> Position -> List Position
neighborsWithSameColor board position =
    case pieceAt position board of
        Just color ->
            let
                keepPositionIfSameColor : Board -> Color -> Position -> Maybe Position
                keepPositionIfSameColor bd col pos =
                    bd
                        |> pieceAt pos
                        |> Maybe.andThen
                            (\pieceColor ->
                                if pieceColor == col then
                                    Just position

                                else
                                    Nothing
                            )
            in
            Position.neighbors position
                |> List.filterMap (keepPositionIfSameColor board color)

        Nothing ->
            []


fcb : Board -> ColorBlock -> Destinations -> List Position
fcb board (ColorBlock colorBlock) (Destinations destinations) =
    case List.uncons destinations of
        Just ( nextPosition, restDestinations ) ->
            let
                updatedColorBlock =
                    nextPosition :: colorBlock

                alreadyAdded =
                    \a -> List.member a updatedColorBlock
            in
            nextPosition
                |> neighborsWithSameColor board
                |> List.filterNot alreadyAdded
                |> List.append restDestinations
                |> Destinations
                |> fcb board (ColorBlock updatedColorBlock)

        Nothing ->
            colorBlock


findBlockAt : Position -> Board -> List Position
findBlockAt position board =
    if pieceExistsAt position board == True then
        let
            destinations =
                Destinations (neighborsWithSameColor board position)

            colorBlock =
                ColorBlock [ position ]
        in
        fcb board colorBlock destinations
            |> Position.sort

    else
        []



-- UPDATING THE BOARD


minimumBlockSize : Int
minimumBlockSize =
    3


setRow : RowIndex -> Row -> Board -> Board
setRow rowIndex row =
    let
        ri =
            RowIndex.unwrap rowIndex
    in
    toList
        >> List.setAt ri row
        >> Board


getRow : RowIndex -> Board -> Maybe Row
getRow rowIndex =
    let
        ri =
            RowIndex.unwrap rowIndex
    in
    toList
        >> List.getAt ri


{-| Remove the block of pieces from the board.
-}
removeBlock : List Position -> Board -> Board
removeBlock colorBlock =
    let
        blockGroupedByRow =
            Position.groupColumnIndicesByRow colorBlock

        removePiecesByRow : List ( RowIndex, List ColumnIndex ) -> Board -> Board
        removePiecesByRow groupedColumnIndices bd =
            case List.uncons groupedColumnIndices of
                Just ( ( rowIndex, columnIndices ), restRowGroups ) ->
                    getRow rowIndex bd
                        |> Maybe.map (Row.removePieces columnIndices)
                        |> Maybe.map (\row -> setRow rowIndex row bd)
                        |> Maybe.map (removePiecesByRow restRowGroups)
                        |> Maybe.withDefault bd

                Nothing ->
                    bd
    in
    removePiecesByRow blockGroupedByRow


slideDownLeft : Board -> Board
slideDownLeft =
    to2dList
        >> List.transpose
        >> List.map (Row.fromMaybeColors >> Row.slideRight)
        >> List.filter Row.isNotEmpty
        >> List.map Row.toList
        >> List.transpose
        >> List.map Row.fromMaybeColors
        >> Board


{-| Remove the block from the board if it's at least the minimum specified # of pieces.
-}
removeBlockIfMinSize : Int -> List Position -> Board -> Board
removeBlockIfMinSize minSize positions board =
    if List.length positions >= minSize then
        board
            |> removeBlock positions
            |> slideDownLeft

    else
        board


removeBlockAt : Position -> Board -> Board
removeBlockAt position board =
    let
        block =
            board |> findBlockAt position
    in
    board
        |> removeBlockIfMinSize 3 block



-- VIEW --


indexRow : Int -> Row -> ( RowIndex, Row )
indexRow ri row =
    ( RowIndex.fromInt ri, row )


draw : PieceLength -> (Position -> msg) -> Board -> List (Svg msg)
draw pieceLength clickPieceMsg =
    toList
        >> List.indexedMap indexRow
        >> List.concatMap (Row.draw pieceLength clickPieceMsg)


view : (Position -> msg) -> Properties -> Board -> List (Html msg)
view clickPieceMsg properties board =
    let
        boardWidth =
            Properties.width properties

        boardHeight =
            Properties.height properties

        drawnBoard =
            board
                |> draw properties.pieceLength clickPieceMsg
    in
    [ svg
        [ width (String.fromInt boardWidth), height (String.fromInt boardHeight) ]
        drawnBoard
    ]
