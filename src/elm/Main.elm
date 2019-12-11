module Main exposing (..)

import Board exposing (Board)
import Board.Position exposing (Position)
import Board.Properties as Properties exposing (Properties)
import Bootstrap exposing (container, row)
import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Random exposing (Generator)
import Svg exposing (Svg)



-- MODEL


type Model
    = Prestart Properties
    | Started Properties Board


initModel : Model
initModel =
    Prestart Properties.default



-- INIT


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Random.generate GeneratedBoard Board.default
    )



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
    ( updateProperties model properties
    , Random.generate GeneratedBoard board
    )


updateNumRows : Model -> Properties -> String -> ( Model, Cmd Msg )
updateNumRows model properties numRows =
    let
        updatedProperties =
            properties |> Properties.updateRows numRows

        updatedBoard =
            Board.generate updatedProperties
    in
    updatePropertiesAndBoard model updatedProperties updatedBoard


updateNumColumns : Model -> Properties -> String -> ( Model, Cmd Msg )
updateNumColumns model properties numColumns =
    let
        updatedProperties =
            properties |> Properties.updateColumns numColumns

        updatedBoard =
            Board.generate updatedProperties
    in
    updatePropertiesAndBoard model updatedProperties updatedBoard


updateNumColors : Model -> Properties -> String -> ( Model, Cmd Msg )
updateNumColors model properties numColors =
    let
        updatedProperties =
            properties |> Properties.updateColors numColors

        updatedBoard =
            Board.generate updatedProperties
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
                GeneratedBoard bd ->
                    let
                        updatedModel =
                            updateBoard model bd
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
                            board 
                                |> Board.removeBlockAt position
                                |> updateBoard model
                    in
                    ( updatedModel, Cmd.none )



-- VIEW


appView : Properties -> List (Svg Msg) -> Html Msg
appView properties boardSvg =
    container
        [ Properties.view UpdateNumRows UpdateNumColumns UpdateNumColors properties
        , div []
            [ row [ class "justify-content-md-center" ]
                [ div [ class "col-md-9" ] boardSvg
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Prestart properties ->
            appView properties []

        Started properties board ->
            board
                |> Board.view ClickPiece properties
                |> appView properties



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


type alias Flags =
    {}


main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
