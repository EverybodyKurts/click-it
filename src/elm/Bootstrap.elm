module Bootstrap exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


formGroup : List (Html msg) -> Html msg
formGroup =
    div [ class "form-group" ]


container : List (Html msg) -> Html msg
container =
    div [ class "container" ]


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attrs =
    let
        rowAttrs =
            [ class "row" ] ++ attrs
    in
    div rowAttrs
