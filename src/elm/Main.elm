module Main exposing (..)

import Maybe exposing (Maybe)
import Html exposing (Html, h1, text, div, input, label)
import Html.Attributes exposing (id, class, for, type_, value)
import Html.Events exposing (onInput)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)
import Svg.Events exposing (onClick)
import Board exposing (Board(..), Rows(..), Row(..), XCoord(..), YCoord(..))
import Board.Properties exposing (Properties, PieceLength(..))
import Board.Position exposing (RowIndex(..), ColumnIndex(..), Position(..))
import Bootstrap exposing (formGroup)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Random exposing (Generator)


-- MODEL


type alias Model =
    { properties : Properties
    , board : Maybe Board
    }


initModel : Model
initModel =
    Model Board.Properties.default Nothing


type NumColors
    = NumColors Int


type NumPieces
    = NumPieces Int



-- INIT


init : ( Model, Cmd Msg )
init =
    initModel ! [ generateBoard Board.default ]



-- UPDATE


generateBoard : Generator Board -> Cmd Msg
generateBoard board =
    Random.generate GeneratedBoard board


updateBoard : Model -> Maybe Board -> Model
updateBoard model board =
    { model | board = board }


updateProperties : Model -> Properties -> Model
updateProperties model properties =
    { model | properties = properties }


updatePropertiesAndBoard : Model -> Properties -> Generator Board -> ( Model, Cmd Msg )
updatePropertiesAndBoard model properties board =
    (updateProperties model properties) ! [ generateBoard board ]


type Msg
    = GeneratedBoard Board
    | UpdateNumRows String
    | UpdateNumColumns String
    | UpdateNumColors String
    | ClickPiece Position



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
                    Board.init updatedProperties
            in
                updatePropertiesAndBoard model updatedProperties updatedBoard

        UpdateNumColumns numCols ->
            let
                updatedProperties =
                    Board.Properties.updateNumColumnsOrDefault properties numCols

                updatedBoard =
                    Board.init updatedProperties
            in
                updatePropertiesAndBoard model updatedProperties updatedBoard

        UpdateNumColors rawNumColors ->
            let
                updatedProperties =
                    Board.Properties.updateNumColorsOrDefault properties rawNumColors

                updatedBoard =
                    Board.init updatedProperties
            in
                updatePropertiesAndBoard model updatedProperties updatedBoard

        ClickPiece position ->
            let
                _ =
                    case board of
                        Just b ->
                            Debug.log "colorblock" (Board.findBlockAt b position)

                        _ ->
                            []
            in
                ( model, Cmd.none )



-- VIEW


drawPiece : PieceLength -> RowIndex -> ( ColumnIndex, Color ) -> Svg Msg
drawPiece pieceLength rowIndex ( columnIndex, color ) =
    let
        (PieceLength length) =
            pieceLength

        (XCoord xCoord) =
            Board.xCoord pieceLength columnIndex

        (YCoord yCoord) =
            Board.yCoord pieceLength rowIndex
    in
        rect
            [ x (toString xCoord)
            , y (toString yCoord)
            , width (toString length)
            , height (toString length)
            , fill (colorToHex color)
            , stroke "#ddd"
            , onClick (ClickPiece (Position ( rowIndex, columnIndex )))
            ]
            []


drawRow : PieceLength -> ( RowIndex, Row ) -> List (Svg Msg)
drawRow pieceLength ( rowIndex, Row row ) =
    let
        keepExistingIndexedColors : ( a, Maybe b ) -> Maybe ( a, b )
        keepExistingIndexedColors ( index, maybeColor ) =
            case maybeColor of
                Just color ->
                    Just ( index, color )

                _ ->
                    Nothing

        columnIndexTuple : Int -> a -> ( ColumnIndex, a )
        columnIndexTuple rawIndex a =
            ( ColumnIndex rawIndex, a )

        indexedPieces =
            row
                |> List.indexedMap columnIndexTuple
                |> List.filterMap keepExistingIndexedColors
                |> List.map (drawPiece pieceLength rowIndex)
    in
        indexedPieces


drawRows : PieceLength -> Maybe Board -> List (Svg Msg)
drawRows pieceLength maybeBoard =
    case maybeBoard of
        Just board ->
            let
                rowIndexTuple : Int -> a -> ( RowIndex, a )
                rowIndexTuple rawIndex a =
                    ( RowIndex rawIndex, a )

                (Board (Rows rows)) =
                    board
            in
                rows
                    |> List.indexedMap rowIndexTuple
                    |> List.concatMap (drawRow pieceLength)

        Nothing ->
            []


view : Model -> Html Msg
view { board, properties } =
    let
        ( numRows, numColumns, numColors, _ ) =
            Board.Properties.raw properties

        boardWidth =
            Board.Properties.width properties

        boardHeight =
            Board.Properties.height properties

        drawnRows =
            drawRows properties.pieceLength board
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
                            , value (toString numColors)
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
                        drawnRows
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
