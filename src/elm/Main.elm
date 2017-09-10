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
    let
        numPieces =
            Board.numPieces initModel.board
    in
        initModel ! [ Random.generate GeneratedPieceColors (genColorsThenPieces numPieces 3) ]



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


genColorsThenPieces : Int -> Int -> Generator (Array (Maybe Color))
genColorsThenPieces numPieces numColors =
    genRandomColors numColors
        |> Random.andThen (genPieceColors numPieces)


type Msg
    = UpdateRows String
    | UpdateColumns String
    | GeneratePieceColors Int
    | GeneratedPieceColors (Array (Maybe Color))
    | GeneratedPieces (Array (Maybe Color))


maybeColorToPiece : Maybe Color -> Piece
maybeColorToPiece maybeColor =
    case maybeColor of
        Just color ->
            Piece color

        Nothing ->
            Piece (Color.rgb 255 0 0)


updateBoard : Model -> Board -> Model
updateBoard model board =
    { model | board = board }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ board } as model) =
    case msg of
        UpdateRows numRows ->
            board
                |> Board.updateRowsFromString numRows
                |> Result.withDefault board
                |> updateBoard model
                |> update (GeneratePieceColors 3)

        UpdateColumns numCols ->
            board
                |> Board.updateColumnsFromString numCols
                |> Result.withDefault board
                |> updateBoard model
                |> update (GeneratePieceColors 3)

        GeneratePieceColors numColors ->
            let
                numPieces =
                    Board.numPieces board
            in
                ( model, Random.generate GeneratedPieceColors (genColorsThenPieces numPieces 3) )

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

        GeneratedPieces colors ->
            ( model, Cmd.none )



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
