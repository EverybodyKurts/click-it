module Main exposing (..)

import Maybe exposing (Maybe)
import Html exposing (Html, h1, text, div, input, label)
import Html.Attributes exposing (id, class, for, type_, value)
import Html.Events exposing (onInput)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)
import Svg.Events exposing (onClick)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Random exposing (Generator)


-- User modules

import Bootstrap exposing (formGroup)
import Board exposing (Board(..))
import Board.Properties exposing (Properties, PieceLength(..), NumRows(..), NumColumns(..), NumColors(..))
import Board.Position exposing (Position)
import Board.Position.RowIndex as RowIndex exposing (RowIndex)
import Board.Position.ColumnIndex as ColumnIndex exposing (ColumnIndex)
import Board.Row as Row exposing (Row(..))
import Board.Rows as Rows
import Util.Tuple as Tuple


-- MODEL


type Model
    = Prestart Properties
    | Started Properties Board


initModel : Model
initModel =
    Prestart Board.Properties.default



-- INIT


init : ( Model, Cmd Msg )
init =
    initModel ! [ generateBoard Board.default ]



-- UPDATE


type Msg
    = GeneratedBoard Board
    | UpdateNumRows String
    | UpdateNumColumns String
    | UpdateNumColors String
    | ClickPiece Position


generateBoard : Generator Board -> Cmd Msg
generateBoard =
    Random.generate GeneratedBoard


updateBoard : Model -> Board -> Model
updateBoard model board =
    case model of
        Prestart properties ->
            Started properties board

        Started properties _ ->
            Started properties board


{-| Whenever a board's properties are updated, reset the game.
-}
updateProperties : Model -> Properties -> Model
updateProperties model updatedProperties =
    case model of
        Prestart _ ->
            Prestart updatedProperties

        Started _ _ ->
            Prestart updatedProperties


{-| Update the board's properties, generate a new board based on those properties, and reset the game.
-}
updatePropertiesAndBoard : Model -> Properties -> Generator Board -> ( Model, Cmd Msg )
updatePropertiesAndBoard model properties board =
    (updateProperties model properties) ! [ generateBoard board ]


updateNumRows : Model -> Properties -> String -> ( Model, Cmd Msg )
updateNumRows model properties numRows =
    let
        updatedProperties =
            Board.Properties.updateNumRowsOrDefault properties numRows

        updatedBoard =
            Board.init updatedProperties
    in
        updatePropertiesAndBoard model updatedProperties updatedBoard


updateNumColumns : Model -> Properties -> String -> ( Model, Cmd Msg )
updateNumColumns model properties numColumns =
    let
        updatedProperties =
            Board.Properties.updateNumColumnsOrDefault properties numColumns

        updatedBoard =
            Board.init updatedProperties
    in
        updatePropertiesAndBoard model updatedProperties updatedBoard


updateNumColors : Model -> Properties -> String -> ( Model, Cmd Msg )
updateNumColors model properties numColors =
    let
        updatedProperties =
            Board.Properties.updateNumColorsOrDefault properties numColors

        updatedBoard =
            Board.init updatedProperties
    in
        updatePropertiesAndBoard model updatedProperties updatedBoard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Prestart properties ->
            case msg of
                GeneratedBoard board ->
                    let
                        updatedModel =
                            updateBoard model board
                    in
                        ( updatedModel, Cmd.none )

                UpdateNumRows numRows ->
                    updateNumRows model properties numRows

                UpdateNumColumns numColumns ->
                    updateNumColumns model properties numColumns

                UpdateNumColors numColors ->
                    updateNumColors model properties numColors

                ClickPiece _ ->
                    ( model, Cmd.none )

        Started properties board ->
            case msg of
                GeneratedBoard board ->
                    let
                        updatedModel =
                            updateBoard model board
                    in
                        ( updatedModel, Cmd.none )

                UpdateNumRows numRows ->
                    updateNumRows model properties numRows

                UpdateNumColumns numColumns ->
                    updateNumColumns model properties numColumns

                UpdateNumColors numColors ->
                    updateNumColors model properties numColors

                ClickPiece position ->
                    let
                        updatedModel =
                            Board.removeBlockAt board position
                                |> updateBoard model
                    in
                        ( updatedModel, Cmd.none )



-- VIEW


drawPiece : PieceLength -> RowIndex -> ( ColumnIndex, Color ) -> Svg Msg
drawPiece pieceLength rowIndex ( columnIndex, color ) =
    let
        (PieceLength length) =
            pieceLength

        xCoord =
            Board.rawXCoord pieceLength columnIndex

        yCoord =
            Board.rawYCoord pieceLength rowIndex

        pos =
            Board.Position.fromIndices rowIndex columnIndex
    in
        rect
            [ x (toString xCoord)
            , y (toString yCoord)
            , width (toString length)
            , height (toString length)
            , fill (colorToHex color)
            , stroke "#ddd"
            , onClick (ClickPiece pos)
            ]
            []


drawRow : PieceLength -> ( RowIndex, Row ) -> List (Svg Msg)
drawRow pieceLength ( rowIndex, Row row ) =
    let
        keepExistingIndexedColors : ( a, Maybe b ) -> Maybe ( a, b )
        keepExistingIndexedColors ( index, maybeColor ) =
            maybeColor
                |> Maybe.map (Tuple.create index)

        indexedColumn : Int -> a -> ( ColumnIndex, a )
        indexedColumn index a =
            ( ColumnIndex.fromInt index, a )
    in
        row
            |> List.indexedMap indexedColumn
            |> List.filterMap keepExistingIndexedColors
            |> List.map (drawPiece pieceLength rowIndex)


drawRows : PieceLength -> Board -> List (Svg Msg)
drawRows pieceLength =
    Board.indexRows
        >> List.concatMap (drawRow pieceLength)


boardRowsFormGroup : NumRows -> List (Html Msg)
boardRowsFormGroup (NumRows numRows) =
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


boardColumnsFormGroup : NumColumns -> List (Html Msg)
boardColumnsFormGroup (NumColumns numColumns) =
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


boardColorsFormGroup : NumColors -> List (Html Msg)
boardColorsFormGroup (NumColors numColors) =
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


boardPropertiesView : NumRows -> NumColumns -> NumColors -> Html Msg
boardPropertiesView numRows numColumns numColors =
    div [ class "row justify-content-md-center" ]
        [ div [ class "col-md-3" ]
            (boardRowsFormGroup numRows)
        , div [ class "col-md-3" ]
            (boardColumnsFormGroup numColumns)
        , div [ class "col-md-3" ]
            (boardColorsFormGroup numColors)
        ]


appView : Properties -> List (Svg Msg) -> Html Msg
appView ({ numRows, numColumns, numColors } as properties) boardSvg =
    div [ class "container" ]
        [ (boardPropertiesView numRows numColumns numColors)
        , div []
            [ div [ class "row justify-content-md-center" ]
                [ div [ class "col-md-9" ] boardSvg
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Prestart properties ->
            appView properties []

        Started ({ numRows, numColumns, numColors } as properties) board ->
            let
                boardWidth =
                    Board.Properties.width properties

                boardHeight =
                    Board.Properties.height properties

                drawnRows =
                    drawRows properties.pieceLength board

                boardSvg =
                    [ svg
                        [ width (toString boardWidth), height (toString boardHeight) ]
                        drawnRows
                    ]
            in
                appView properties boardSvg



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
