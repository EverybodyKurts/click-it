module Board.Piece exposing (..)

import Board
import Board.Position as Position exposing (Position)
import Board.Properties as Properties exposing (PieceLength)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (x, y, width, height, fill, stroke)
import Svg.Events exposing (onClick)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)


type alias Piece =
    { color : Color
    , length : PieceLength
    , position : Position
    }


create : Color -> PieceLength -> Position -> Piece
create color length position =
    Piece color length position


-- VIEW --

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
            Board.rawXCoord length columnIndex

        yCoord =
            Board.rawYCoord length rowIndex

        pos =
            Position.fromIndices rowIndex columnIndex
    in
        rect
            [ x (toString xCoord)
            , y (toString yCoord)
            , width (toString rawPieceLength)
            , height (toString rawPieceLength)
            , fill (colorToHex color)
            , stroke "#ddd"
            , onClick (clickMsg pos)
            ]
            []
