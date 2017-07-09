module Main exposing (..)

import Html exposing (Html, h1, text, div, input, label)
import Html.Attributes exposing (id, class, for, type_, value)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)
import Board.Piece as Piece exposing (Piece, Color(..))
import Board exposing (Board)
import Bootstrap exposing (formGroup)
import Array exposing (Array)


-- MODEL


type alias Model =
    { board : Board
    }


initModel : Model
initModel =
    { board = Board.default
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- VIEW


drawPiece : Board -> Color -> Board.Index -> Svg Msg
drawPiece ({ pieceLength } as board) color index =
    let
        { xPos, yPos } =
            Board.piecePos board index

        (Color col) =
            color

        (Piece.Length len) =
            pieceLength
    in
        rect
            [ x (toString xPos)
            , y (toString yPos)
            , width (toString len)
            , height (toString len)
            , fill col
            , stroke "#ddd"
            ]
            []


view : Model -> Html Msg
view { board } =
    let
        { rows, columns } =
            board

        (Board.Rows bdRows) =
            rows

        (Board.Columns bdCols) =
            columns

        ( Board.Width bWidth, Board.Height bHeight ) =
            Board.dimensions board

        drawnPieces =
            Board.indices board
                |> List.map (drawPiece board (Color "#f00"))
    in
        div []
            [ div [ class "d-flex flex-row" ]
                [ div [ class "p-2" ]
                    [ formGroup
                        [ label [ for "boardRows" ] [ (text "Rows") ]
                        , input
                            [ type_ "number"
                            , value (toString bdRows)
                            , class "form-control"
                            , id "boardRows"
                            ]
                            []
                        ]
                    ]
                , div [ class "p-2" ]
                    [ formGroup
                        [ label [ for "boardColumns" ] [ (text "Columns") ]
                        , input
                            [ type_ "number"
                            , value (toString bdCols)
                            , class "form-control"
                            , id "boardColumns"
                            ]
                            []
                        ]
                    ]
                ]
            , div [ class "d-flex flex-row" ]
                [ div [ class "p-12" ]
                    [ svg
                        [ width (toString bWidth), height (toString bHeight) ]
                        drawnPieces
                    ]
                ]
            ]



-- UPDATE


type Msg
    = None


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
