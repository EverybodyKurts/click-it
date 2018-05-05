module Main exposing (..)

import Html exposing (Html, h1, text, div, input, label)
import Html.Attributes exposing (id, class, for, type_, value)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, ry, fill, stroke)
import Random exposing (Generator)


-- User modules

import Board exposing (Board)
import Board.Properties as Properties exposing (Properties)
import Board.Position as Position exposing (Position)


-- MODEL


type Model
    = Prestart Properties
    | Started Properties Board


initModel : Model
initModel =
    Prestart Properties.default



-- INIT


init : ( Model, Cmd Msg )
init =
    initModel ! [ Board.generate GeneratedBoard Board.default ]



-- UPDATE


type Msg
    = GeneratedBoard Board
    | UpdateNumRows String
    | UpdateNumColumns String
    | UpdateNumColors String
    | ClickPiece Position


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
    (updateProperties model properties) ! [ Board.generate GeneratedBoard board ]


updateNumRows : Model -> Properties -> String -> ( Model, Cmd Msg )
updateNumRows model properties numRows =
    let
        updatedProperties =
            Properties.updateNumRowsOrDefault properties numRows

        updatedBoard =
            Board.init updatedProperties
    in
        updatePropertiesAndBoard model updatedProperties updatedBoard


updateNumColumns : Model -> Properties -> String -> ( Model, Cmd Msg )
updateNumColumns model properties numColumns =
    let
        updatedProperties =
            Properties.updateNumColumnsOrDefault properties numColumns

        updatedBoard =
            Board.init updatedProperties
    in
        updatePropertiesAndBoard model updatedProperties updatedBoard


updateNumColors : Model -> Properties -> String -> ( Model, Cmd Msg )
updateNumColors model properties numColors =
    let
        updatedProperties =
            Properties.updateNumColorsOrDefault properties numColors

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


appView : Properties -> List (Svg Msg) -> Html Msg
appView properties boardSvg =
    div [ class "container" ]
        [ (Properties.view UpdateNumRows UpdateNumColumns UpdateNumColors properties)
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
                    Properties.width properties

                boardHeight =
                    Properties.height properties

                drawnBoard =
                    board
                        |> Board.draw properties.pieceLength ClickPiece

                boardSvg =
                    [ svg
                        [ width (toString boardWidth), height (toString boardHeight) ]
                        drawnBoard
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
