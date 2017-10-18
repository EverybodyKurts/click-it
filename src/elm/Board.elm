module Board exposing (..)

import Array exposing (Array)
import Color exposing (Color)
import Random exposing (Generator)
import Random.Array
import List.Extra as Lextra
import Board.Properties exposing (Properties, PieceLength(..), NumColumns(..))
import Board.Position as Position exposing (RowIndex(..), ColumnIndex(..), Position(..))
import Maybe.Extra
import Board.Row as Row exposing (Row(..))
import Board.Rows as Rows exposing (Rows(..))


type Board
    = Board Rows


type XCoord
    = XCoord Int


type YCoord
    = YCoord Int



-- BOARD INITIALIZATION


{-| Generate a random color
-}
genRandomColor : Generator Color
genRandomColor =
    Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)


{-| Generate a color palette to use in the board
-}
genColorPalette : Int -> Generator (Array Color)
genColorPalette numColors =
    Random.Array.array numColors genRandomColor


colorsToBoard : Int -> List (Maybe Color) -> Board
colorsToBoard numColumns =
    Lextra.groupsOf numColumns
        >> Rows.fromList
        >> Board


{-| Generate the board & its colors
-}
genBoard : Int -> Int -> Array Color -> Generator Board
genBoard numRows numColumns colorPalette =
    Random.list (numRows * numColumns) (Random.Array.sample colorPalette)
        |> Random.map (colorsToBoard numColumns)


{-| Generate color palette & then the board and its colors
-}
genPaletteThenBoard : Int -> Int -> Int -> Generator Board
genPaletteThenBoard numRows numColumns numColors =
    genColorPalette numColors
        |> Random.andThen (genBoard numRows numColumns)


{-| Initialize a board based on specified properties
-}
init : Properties -> Generator Board
init properties =
    let
        ( rows, columns, colors, pieceLength ) =
            (Board.Properties.raw properties)
    in
        genPaletteThenBoard rows columns colors


{-| Generate the default board
-}
default : Generator Board
default =
    init Board.Properties.default



-- ACCESSING THE BOARD


unwrapBoard : Board -> Rows
unwrapBoard (Board rows) =
    rows


to2dList : Board -> List (List (Maybe Color))
to2dList =
    unwrapBoard
        >> Rows.unwrap
        >> List.map Row.unwrap


from2dList : List (List (Maybe Color)) -> Board
from2dList =
    List.map Row
        >> Rows
        >> Board


getPiece : Position -> Board -> Maybe Color
getPiece (Position ( RowIndex rowIndex, ColumnIndex columnIndex )) =
    to2dList
        >> Lextra.getAt rowIndex
        >> Maybe.andThen (Lextra.getAt columnIndex)
        >> Maybe.Extra.join


equivNeighborPositions : Board -> Color -> Position -> List Position
equivNeighborPositions board color position =
    let
        keepPositionIfSameColor : Board -> Color -> Position -> Maybe Position
        keepPositionIfSameColor board color position =
            getPiece position board
                |> Maybe.andThen
                    (\pieceColor ->
                        if pieceColor == color then
                            Just position
                        else
                            Nothing
                    )
    in
        Position.neighbors position
            |> List.filterMap (keepPositionIfSameColor board color)


type ColorBlock
    = ColorBlock (List Position)


type Destinations
    = Destinations (List Position)


fcb : Color -> Board -> ColorBlock -> Destinations -> List Position
fcb color board (ColorBlock colorBlock) (Destinations destinations) =
    case Lextra.uncons destinations of
        Just ( blockPosition, restDestinations ) ->
            let
                updatedColorBlock =
                    blockPosition :: colorBlock
            in
                blockPosition
                    |> (equivNeighborPositions board color)
                    |> Lextra.filterNot (flip List.member updatedColorBlock)
                    |> List.append restDestinations
                    |> Destinations
                    |> (fcb color board (ColorBlock updatedColorBlock))

        Nothing ->
            colorBlock


findBlockAt : Board -> Position -> List Position
findBlockAt board position =
    case getPiece position board of
        Just color ->
            let
                destinations =
                    Destinations (equivNeighborPositions board color position)

                colorBlock =
                    ColorBlock [ position ]
            in
                fcb color board colorBlock destinations
                    |> Position.sort

        Nothing ->
            []



-- UPDATING THE BOARD


toRowsList : Board -> List Row
toRowsList =
    unwrapBoard
        >> Rows.unwrap


rb : List Row -> List ( Int, List ColumnIndex ) -> List Row
rb rowsList rowGroupedColumnIndices =
    case Lextra.uncons rowGroupedColumnIndices of
        Just ( ( rowIndex, columnIndices ), restRowGroups ) ->
            let
                updatedRowsList =
                    Lextra.getAt rowIndex rowsList
                        |> Maybe.map (Row.removePieces columnIndices)
                        |> Maybe.andThen (\row -> Lextra.setAt rowIndex row rowsList)
                        |> Maybe.withDefault rowsList
            in
                rb updatedRowsList restRowGroups

        Nothing ->
            rowsList


fromRowsList : List Row -> Board
fromRowsList =
    Rows >> Board


removeBlock : Board -> List Position -> Board
removeBlock board colorBlock =
    if List.length colorBlock >= 3 then
        let
            blockGroupedByRow =
                Position.groupColumnIndicesByRow colorBlock
        in
            rb (toRowsList board) blockGroupedByRow
                |> fromRowsList
    else
        board


removeBlockAt : Board -> Position -> Board
removeBlockAt board =
    findBlockAt board
        >> removeBlock board


xCoord : PieceLength -> ColumnIndex -> XCoord
xCoord (PieceLength pieceLength) (ColumnIndex columnIndex) =
    XCoord (pieceLength * columnIndex)


yCoord : PieceLength -> RowIndex -> YCoord
yCoord (PieceLength pieceLength) (RowIndex rowIndex) =
    YCoord (pieceLength * rowIndex)
