module Main exposing (..)

import Array exposing (Array)
import Maybe exposing (Maybe)
import Html exposing (Html, h1, text, div, input, label)
import Html.Attributes exposing (id, class, for, type_, value)
import Html.Events exposing (onInput)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)
import Svg.Events exposing (onClick)
import Board exposing (Board, Piece)
import Bootstrap exposing (formGroup)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Random exposing (Generator)
import Random.Array


-- MODEL


type alias Model =
    { board : Board
    }


initModel : Model
initModel =
    { board = Board.default
    }


type NumColors
    = NumColors Int


type NumPieces
    = NumPieces Int



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        numPieces =
            Board.numPieces initModel.board

        numColors =
            Board.numColorsValue initModel.board
    in
        initModel ! [ Random.generate GeneratedPieceColors (genColorsThenPieces (NumColors numColors) (NumPieces numPieces)) ]



-- UPDATE


{-| Generate a random color
-}
genRandomColor : Generator Color
genRandomColor =
    Random.map3 Color.rgb (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)


{-| Generate a specified # of random colors
-}
genRandomColors : Int -> Generator (Array Color)
genRandomColors numColors =
    Random.Array.array numColors genRandomColor


{-| Generate an array of pieces that have a random color from the specified list of colors
-}
genPieceColors : Int -> Array Color -> Generator (Array (Maybe Color))
genPieceColors numPieces colors =
    Random.Array.array numPieces (Random.Array.sample colors)


{-| Generate an array of colors and then array of pieces that sample from those random colors.
-}
genColorsThenPieces : NumColors -> NumPieces -> Generator (Array (Maybe Color))
genColorsThenPieces (NumColors numColors) (NumPieces numPieces) =
    genRandomColors numColors
        |> Random.andThen (genPieceColors numPieces)


updateBoard : Model -> Board -> Model
updateBoard model board =
    { model | board = board }


type Msg
    = UpdateRows String
    | UpdateColumns String
    | UpdateNumColors String
    | GeneratePieceColors Int
    | GeneratedPieceColors (Array (Maybe Color))
    | GeneratedPieces (Array (Maybe Color))
    | ClickPiece Board.Index


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ board } as model) =
    case msg of
        UpdateRows numRows ->
            Board.updateNumRowsFromString board numRows
                |> Result.withDefault board
                |> updateBoard model
                |> update (GeneratePieceColors (Board.numColorsValue board))

        UpdateColumns numCols ->
            Board.updateNumColumnsFromString board numCols
                |> Result.withDefault board
                |> updateBoard model
                |> update (GeneratePieceColors (Board.numColorsValue board))

        GeneratePieceColors numColors ->
            let
                cmd =
                    board
                        |> Board.numPieces
                        |> NumPieces
                        |> (genColorsThenPieces (NumColors numColors))
                        |> Random.generate GeneratedPieceColors
            in
                ( model, cmd )

        GeneratedPieceColors colors ->
            let
                updatedModel =
                    colors
                        |> Board.maybeColorsToPieces board
                        |> updateBoard model
            in
                ( updatedModel, Cmd.none )

        GeneratedPieces colors ->
            ( model, Cmd.none )

        UpdateNumColors rawNumColors ->
            let
                updatedBoard =
                    Board.updateNumColorsFromString board rawNumColors
                        |> Result.withDefault board

                numColors =
                    Board.numColorsValue updatedBoard

                cmd =
                    updatedBoard
                        |> Board.numPieces
                        |> NumPieces
                        |> (genColorsThenPieces (NumColors numColors))
                        |> Random.generate GeneratedPieceColors
            in
                { model | board = updatedBoard } ! [ cmd ]

        ClickPiece idx ->
            let
                updatedModel =
                    Board.removeBlockAt model.board idx
                        |> updateBoard model
            in
                -- Board.removePieceAtIndex board index
                ( updatedModel, Cmd.none )



-- VIEW


drawPiece : Board -> ( Board.Index, Piece ) -> Svg Msg
drawPiece board ( index, piece ) =
    let
        { xCoord, yCoord } =
            Board.pieceCoordinates board index

        len =
            Board.pieceLengthValue board
    in
        rect
            [ x (toString xCoord)
            , y (toString yCoord)
            , width (toString len)
            , height (toString len)
            , fill (colorToHex piece.color)
            , stroke "#ddd"
            , onClick (ClickPiece index)
            ]
            []


drawPieces : Board -> List (Svg Msg)
drawPieces board =
    board.pieces
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
view { board } =
    let
        bdRows =
            Board.numRowsValue board
                |> toString

        bdCols =
            Board.numColumnsValue board
                |> toString

        ( bWidth, bHeight ) =
            Board.dimensionsValue board

        drawnPieces =
            drawPieces board

        numColors =
            Board.numColorsValue board
                |> toString
    in
        div []
            [ div [ class "d-flex flex-row" ]
                [ div [ class "p-2" ]
                    [ formGroup
                        [ label [ for "boardRows" ] [ (text "Rows") ]
                        , input
                            [ type_ "number"
                            , value bdRows
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
                            , value bdCols
                            , class "form-control"
                            , id "boardColumns"
                            , onInput UpdateColumns
                            ]
                            []
                        ]
                    ]
                , div [ class "p-2" ]
                    [ formGroup
                        [ label [ for "numColors" ] [ (text "# of Colors") ]
                        , input
                            [ type_ "number"
                            , value numColors
                            , class "form-control"
                            , id "numColors"
                            , onInput UpdateNumColors
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
