module Bootstrap exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


formGroup : List (Html msg) -> Html msg
formGroup =
    div [ class "form-group" ]
