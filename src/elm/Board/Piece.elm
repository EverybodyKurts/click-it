module Board.Piece exposing (..)

import Board.Position as Position exposing (Position)
import Board.Position.ColumnIndex as ColumnIndex
import Board.Position.RowIndex as RowIndex
import Board.Properties as Properties exposing (PieceLength)
import Color exposing (Color)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (fill, height, stroke, width, x, y)
import Svg.Events exposing (onClick)


type alias Piece =
    { color : Color
    , length : PieceLength
    , position : Position
    }


create : Color -> PieceLength -> Position -> Piece
create color length position =
    Piece color length position



-- VIEW --


rawXCoord : Piece -> Int
rawXCoord { length, position } =
    let
        ci =
            position |> Position.columnIndex |> ColumnIndex.unwrap

        len =
            Properties.unwrapPieceLength length
    in
    ci * len


rawYCoord : Piece -> Int
rawYCoord { length, position } =
    let
        ri =
            position |> Position.rowIndex |> RowIndex.unwrap

        len =
            Properties.unwrapPieceLength length
    in
    ri * len


draw : (Position -> a) -> Piece -> Svg a
draw clickMsg ({ position, color, length } as piece) =
    let
        columnIndex =
            Position.columnIndex position

        rowIndex =
            Position.rowIndex position

        rawPieceLength =
            Properties.unwrapPieceLength length

        xCoord =
            rawXCoord piece

        yCoord =
            rawYCoord piece

        pos =
            Position.fromIndices rowIndex columnIndex
    in
    rect
        [ x (String.fromInt xCoord)
        , y (String.fromInt yCoord)
        , width (String.fromInt rawPieceLength)
        , height (String.fromInt rawPieceLength)
        , fill (Color.toCssString color)
        , stroke "#ddd"
        , onClick (clickMsg pos)
        ]
        []
