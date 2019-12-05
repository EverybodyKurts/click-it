module Util.Color exposing (random, randomArray)

import Array exposing (Array)
import Color exposing (Color)
import Random exposing (Generator)
import Random.Array


{-| Generate a random color
-}
random : Generator Color
random =
    Random.map3 Color.rgb255 (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)

{-| Generate a color palette to use in the board
-}
randomArray : Int -> Generator (Array Color)
randomArray numColors =
    Random.Array.array numColors random
