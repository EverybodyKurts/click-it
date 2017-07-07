module BoardPiece exposing (..)

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry)


type Color
    = Color String


type alias Position =
    { xPos : Int
    , yPos : Int
    }


type alias Piece =
    { length : Int
    , color : Color
    , position : Position
    }


draw : Piece -> Svg msg
draw { length, color, position } =
    let
        { xPos, yPos } =
            position
    in
        rect
            [ x (toString xPos)
            , y (toString yPos)
            , width (toString length)
            , height (toString length)
            ]
            []
