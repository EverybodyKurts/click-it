module Main exposing (..)

import Array exposing (Array)
import Maybe exposing (Maybe)
import Html exposing (Html, h1, text, div, input, label)
import Html.Attributes exposing (id, class, for, type_, value)
import Html.Events exposing (onInput)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)
import Board.Piece exposing (Piece)
import Board exposing (Board)
import Bootstrap exposing (formGroup)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Random exposing (Generator)
import Random.Array


-- MODEL


type alias Model =
    { board : Board
    , pieces : Array ( Board.Index, Maybe Piece )
    }


initModel : Model
initModel =
    { board = Board.default
    , pieces = Array.fromList []
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    update (GeneratePieceColors 3) initModel



-- UPDATE


randomRgb : Generator Color
randomRgb =
    Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)


randomRgbs : Int -> Generator (Array Color)
randomRgbs numColors =
    Random.Array.array numColors randomRgb


generatePieceColors : Int -> Int -> Generator (Array (Maybe Color))
generatePieceColors numColors numPieces =
    let
        randColors =
            randomRgbs numColors

        seed0 =
            Random.initialSeed 0

        ( colors, _ ) =
            Random.step randColors seed0
    in
        Random.Array.array numPieces (Random.Array.sample colors)


type Msg
    = UpdateRows String
    | UpdateColumns String
    | GeneratePieceColors Int
    | GeneratedPieceColors (Array (Maybe Color))


maybeColorToPiece : Maybe Color -> Piece
maybeColorToPiece maybeColor =
    case maybeColor of
        Just color ->
            Piece color

        Nothing ->
            Piece (Color.rgb 255 0 0)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ board } as model) =
    case msg of
        UpdateRows numRows ->
            let
                bd =
                    String.toInt numRows
                        |> Result.map (clamp 1 100)
                        |> Result.map Board.Rows
                        |> Result.map (Board.updateRows board)
                        |> Result.withDefault board
            in
                update (GeneratePieceColors 3) { model | board = bd }

        UpdateColumns numCols ->
            let
                bd =
                    String.toInt numCols
                        |> Result.map (clamp 1 100)
                        |> Result.map Board.Columns
                        |> Result.map (Board.updateColumns board)
                        |> Result.withDefault board
            in
                update (GeneratePieceColors 3) { model | board = bd }

        GeneratePieceColors numColors ->
            let
                numPieces =
                    Board.numPieces board
            in
                ( model, Random.generate GeneratedPieceColors (generatePieceColors 3 numPieces) )

        GeneratedPieceColors colors ->
            let
                pieces =
                    colors
                        |> Array.map maybeColorToPiece
                        |> Array.map Just
                        |> Array.indexedMap (,)
                        |> Array.map (\( i, p ) -> ( Board.Index i, p ))
            in
                ( { model | pieces = pieces }, Cmd.none )



-- VIEW


drawPiece : Board -> ( Board.Index, Piece ) -> Svg Msg
drawPiece ({ pieceLength } as board) ( index, piece ) =
    let
        { xPos, yPos } =
            Board.piecePos board index

        hexColor =
            colorToHex piece.color

        (Board.Piece.Length len) =
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


drawPieces : Board -> Array ( Board.Index, Maybe Piece ) -> List (Svg Msg)
drawPieces board piecesWithIndex =
    piecesWithIndex
        |> Array.toList
        |> List.filterMap
            (\( i, mp ) ->
                case mp of
                    Just p ->
                        Just ( i, p )

                    Nothing ->
                        Nothing
            )
        |> List.map (drawPiece board)


view : Model -> Html Msg
view { board, pieces } =
    let
        { rows, columns, colors } =
            board

        col =
            Array.get 0 colors
                |> Maybe.withDefault Color.red

        (Board.Rows bdRows) =
            rows

        (Board.Columns bdCols) =
            columns

        ( Board.Width bWidth, Board.Height bHeight ) =
            Board.dimensions board

        drawnPieces =
            drawPieces board pieces
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
