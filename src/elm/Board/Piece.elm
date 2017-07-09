module Board.Piece exposing (..)

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)


type Color
    = Color String


type Length
    = Length Int


type alias Position =
    { xPos : Int
    , yPos : Int
    }


type alias Piece =
    { color : Color
    }
