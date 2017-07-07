module Main exposing (..)

import Html exposing (Html, h1, text)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill)
import BoardPiece exposing (Piece, Color(..), Position)


-- MODEL


type alias Model =
    {}



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model, Cmd.none )


piecePos : Position
piecePos =
    { xPos = 10, yPos = 10 }


piece : Piece
piece =
    { length = 25
    , color = (Color "#f00")
    , position = piecePos
    }



-- VIEW


view : Model -> Html Msg
view model =
    svg
        [ width "120", height "120", viewBox "0 0 120 120" ]
        [ BoardPiece.draw piece
        ]



-- Message


type Msg
    = None



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
