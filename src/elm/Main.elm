module Main exposing (..)

import Html exposing (Html, h1, text, div, input, label)
import Html.Attributes exposing (id, class, for, type_, value)
import Html.Events exposing (onInput)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)
import Board.Piece as Piece exposing (Piece)
import Board exposing (Board)
import Bootstrap exposing (formGroup)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Random exposing (Generator)


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
    update (GenerateRandomColors 3) initModel



-- UPDATE


randomRgb : Generator Color
randomRgb =
    Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)


randomRgbs : Int -> Generator (List Color)
randomRgbs numColors =
    Random.list numColors randomRgb


type Msg
    = UpdateRows String
    | UpdateColumns String
    | GenerateRandomColors Int
    | GeneratedRandomColors (List Color)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ board } as model) =
    case msg of
        UpdateRows numRows ->
            let
                bd =
                    String.toInt numRows
                        |> Result.map (clamp 1 100)
                        |> Result.map (\r -> { board | rows = (Board.Rows r) })
                        |> Result.withDefault board
            in
                ( { model | board = bd }, Cmd.none )

        UpdateColumns numCols ->
            let
                bd =
                    String.toInt numCols
                        |> Result.map (clamp 1 100)
                        |> Result.map (\c -> { board | columns = (Board.Columns c) })
                        |> Result.withDefault board
            in
                ( { model | board = bd }, Cmd.none )

        GenerateRandomColors numColors ->
            ( model, Random.generate GeneratedRandomColors (randomRgbs numColors) )

        GeneratedRandomColors randColors ->
            let
                bd =
                    { board | colors = randColors }
            in
                ( { model | board = bd }, Cmd.none )



-- VIEW


drawPiece : Board -> Color -> Board.Index -> Svg Msg
drawPiece ({ pieceLength } as board) color index =
    let
        { xPos, yPos } =
            Board.piecePos board index

        hexColor =
            colorToHex color

        (Piece.Length len) =
            pieceLength
    in
        rect
            [ x (toString xPos)
            , y (toString yPos)
            , width (toString len)
            , height (toString len)
            , fill hexColor
            , stroke "#ddd"
            ]
            []


view : Model -> Html Msg
view { board } =
    let
        { rows, columns, colors } =
            board

        col =
            List.head colors
                |> Maybe.withDefault Color.red

        (Board.Rows bdRows) =
            rows

        (Board.Columns bdCols) =
            columns

        ( Board.Width bWidth, Board.Height bHeight ) =
            Board.dimensions board

        drawnPieces =
            Board.indices board
                |> List.map (drawPiece board col)
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
                            , onInput UpdateRows
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
                            , onInput UpdateColumns
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
