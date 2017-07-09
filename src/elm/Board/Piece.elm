module Board.Piece exposing (..)

import Color exposing (Color)


type Length
    = Length Int


type alias Position =
    { xPos : Int
    , yPos : Int
    }


type alias Piece =
    { color : Color
    }
