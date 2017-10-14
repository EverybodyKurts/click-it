module Main exposing (..)

import Array exposing (Array)
import Maybe exposing (Maybe)
import Html exposing (Html, h1, text, div, input, label)
import Html.Attributes exposing (id, class, for, type_, value)
import Html.Events exposing (onInput)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)
import Svg.Events exposing (onClick)


-- import Board exposing (Board, Piece)

import Bd exposing (Board(..), Rows(..))
import Board.Properties exposing (Properties)
import Bootstrap exposing (formGroup)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Random exposing (Generator)
import Random.Array


-- MODEL


type alias Model =
    { properties : Properties
    , board : Maybe Board
    }


initModel : Model
initModel =
    { board = Bd.default
    }


type NumColors
    = NumColors Int


type NumPieces
    = NumPieces Int



-- INIT


init : ( Model, Cmd Msg )
init =
    initModel ! [ generateBoard Bd.default ]



-- UPDATE


generateBoard board =
    Random.generate GeneratedBoard board


updateBoard : Model -> Board -> Model
updateBoard model board =
    { model | board = board }


updateProperties : Model -> Properties -> Model
updateProperties model properties =
    { model | properties = properties }


updatePropertiesAndBoard model properties board =
    (updateProperties model properties) ! [ generateBoard board ]


type Msg
    = GeneratedBoard Board
    | UpdateNumRows String
    | UpdateNumColumns String
    | UpdateNumColors String
    | GeneratePieceColors Int
    | GeneratedPieces (Array (Maybe Color))



-- | ClickPiece Board.Index


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ properties, board } as model) =
    case msg of
        GeneratedBoard board ->
            let
                updatedModel =
                    updateBoard model (Just board)
            in
                ( updatedModel, Cmd.none )

        UpdateNumRows numRows ->
            let
                updatedProperties =
                    Board.Properties.updateNumRowsOrDefault properties numRows

                updatedBoard =
                    Bd.init updatedProperties
            in
                updatePropertiesAndBoard model updatedProperties updatedBoard

        UpdateNumColumns numCols ->
            let
                updatedProperties =
                    Board.Properties.updateNumColumnsOrDefault properties numCols

                updatedBoard =
                    Bd.init updatedProperties
            in
                updatePropertiesAndBoard model updatedProperties updatedBoard

        UpdateNumColors rawNumColors ->
            let
                updatedProperties =
                    Board.Properties.updateNumColorsOrDefault board rawNumColors

                updatedBoard =
                    Bd.init updatedProperties
            in
                updatePropertiesAndBoard model updatedProperties updatedBoard



-- ClickPiece idx ->
--     let
--         updatedModel =
--             Board.removeBlockAt model.board idx
--                 |> updateBoard model
--     in
--         ( updatedModel, Cmd.none )
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

            -- , onClick (ClickPiece index)
            ]
            []


drawPieces : Board -> List (Svg Msg)
drawPieces (Board (Rows rows)) =
    rows
        |> List.indexedMap (,)



-- board.pieces
--     |> Array.toList
--     |> List.filterMap
--         (\( i, mp ) ->
--             case mp of
--                 Just p ->
--                     Just ( i, p )
--                 Nothing ->
--                     Nothing
--         )
--     |> List.map (drawPiece board)


view : Model -> Html Msg
view { board, properties } =
    let
        ( numRows, numColumns, numColors, _ ) =
            Board.Properties.raw properties

        boardWidth =
            Board.Properties.width properties

        boardHeight =
            Board.Properties.height properties

        drawnPieces =
            drawPieces board
    in
        div []
            [ div [ class "d-flex flex-row" ]
                [ div [ class "p-2" ]
                    [ formGroup
                        [ label [ for "boardRows" ] [ (text "Rows") ]
                        , input
                            [ type_ "number"
                            , value (toString numRows)
                            , class "form-control"
                            , id "boardRows"
                            , onInput UpdateNumRows
                            ]
                            []
                        ]
                    ]
                , div [ class "p-2" ]
                    [ formGroup
                        [ label [ for "boardColumns" ] [ (text "Columns") ]
                        , input
                            [ type_ "number"
                            , value (toString numColumns)
                            , class "form-control"
                            , id "boardColumns"
                            , onInput UpdateNumColumns
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
                        [ width (toString boardWidth), height (toString boardHeight) ]
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
