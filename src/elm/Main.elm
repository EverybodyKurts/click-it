module Main exposing (..)

import Html exposing (Html, h1, text, div, input, label)
import Html.Attributes exposing (id, class, for, type_, value)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)
import Board.Piece as Piece exposing (Piece, Color(..), Position)
import Board exposing (Board)
import Bootstrap exposing (formGroup)


-- MODEL


type alias Model =
    { board : Board }


initModel : Model
initModel =
    { board = Board.default }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


piecePos : Position
piecePos =
    { xPos = 0, yPos = 0 }


piece : Piece
piece =
    { length = (Piece.Length 25)
    , color = (Color "#f00")
    , position = piecePos
    }



-- VIEW


view : Model -> Html Msg
view { board } =
    let
        ( Board.Width bWidth, Board.Height bHeight ) =
            Board.dimensions board
    in
        div []
            [ div [ class "d-flex flex-row" ]
                [ div [ class "p-2" ]
                    [ formGroup
                        [ label [ for "boardHeight" ] [ (text "Height") ]
                        , input
                            [ type_ "number"
                            , value (toString bHeight)
                            , class "form-control"
                            , id "boardHeight"
                            ]
                            []
                        ]
                    ]
                , div [ class "p-2" ]
                    [ formGroup
                        [ label [ for "boardWidth" ] [ (text "Width") ]
                        , input
                            [ type_ "number"
                            , value (toString bWidth)
                            , class "form-control"
                            , id "boardWidth"
                            ]
                            []
                        ]
                    ]
                ]
            , div [ class "d-flex flex-row" ]
                [ div [ class "p-12" ]
                    [ svg
                        [ width (toString bWidth), height (toString bHeight) ]
                        []
                    ]
                ]
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
