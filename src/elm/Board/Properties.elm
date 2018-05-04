module Board.Properties exposing (..)

import Html exposing (Html, label, text, input, div)
import Html.Attributes exposing (for, type_, value, class, id)
import Html.Events exposing (onInput)
import Bootstrap exposing (formGroup)


type NumRows
    = NumRows Int


type NumColumns
    = NumColumns Int


type NumColors
    = NumColors Int


type PieceLength
    = PieceLength Int


type alias Properties =
    { numRows : NumRows
    , numColumns : NumColumns
    , numColors : NumColors
    , pieceLength : PieceLength
    }



-- INITIALIZING BOARD PROPERTIES


{-| The board's default # of rows, columns, colors, and piece length.
-}
default : Properties
default =
    Properties (NumRows 15) (NumColumns 10) (NumColors 3) (PieceLength 50)



-- ACCESSING PROPERTIES


{-| Return the board's properties as a tuple of integers.
-}
raw : Properties -> ( Int, Int, Int, Int )
raw { numRows, numColumns, numColors, pieceLength } =
    let
        (NumRows rows) =
            numRows

        (NumColumns columns) =
            numColumns

        (NumColors colors) =
            numColors

        (PieceLength pl) =
            pieceLength
    in
        ( rows, columns, colors, pl )


{-| Get the board's total # of pieces, based on the # of its rows & columns.
-}
numPieces : Properties -> Int
numPieces { numRows, numColumns } =
    let
        (NumRows r) =
            numRows

        (NumColumns c) =
            numColumns
    in
        r * c



-- UPDATING PROPERTIES


{-| Update the number of rows.
-}
updateNumRows : Properties -> NumRows -> Properties
updateNumRows properties numRows =
    { properties | numRows = numRows }


{-| Update the number of rows from a string, most likely user input.
-}
updateNumRowsFromString : Properties -> String -> Result String Properties
updateNumRowsFromString properties =
    String.toInt
        >> Result.map (clamp 1 100)
        >> Result.map NumRows
        >> Result.map (updateNumRows properties)


{-| Update # of rows from user input or provide default # of rows.
-}
updateNumRowsOrDefault : Properties -> String -> Properties
updateNumRowsOrDefault properties =
    (updateNumRowsFromString properties)
        >> Result.withDefault default


updateNumColumns : Properties -> NumColumns -> Properties
updateNumColumns properties numColumns =
    { properties | numColumns = numColumns }


{-| Update the board's # of columns from a string, most likely user input.
-}
updateNumColumnsFromString : Properties -> String -> Result String Properties
updateNumColumnsFromString properties =
    String.toInt
        >> Result.map (clamp 1 100)
        >> Result.map NumColumns
        >> Result.map (updateNumColumns properties)


{-| Update the board's # of columns based on user input or provide default # of columns
-}
updateNumColumnsOrDefault : Properties -> String -> Properties
updateNumColumnsOrDefault properties =
    (updateNumColumnsFromString properties)
        >> Result.withDefault default


{-| Update the board's number of colors.
-}
updateNumColors : Properties -> NumColors -> Properties
updateNumColors properties numColors =
    { properties | numColors = numColors }


{-| Update the board's # of colors from a string, most likely user input.
-}
updateNumColorsFromString : Properties -> String -> Result String Properties
updateNumColorsFromString properties =
    String.toInt
        >> Result.map (clamp 1 100)
        >> Result.map NumColors
        >> Result.map (updateNumColors properties)


{-| Update the board's # of colors based on user input or provide default # of colors
-}
updateNumColorsOrDefault : Properties -> String -> Properties
updateNumColorsOrDefault properties =
    (updateNumColorsFromString properties)
        >> Result.withDefault default



-- DRAWING THE BOARD


{-| The board's drawn width
-}
width : Properties -> Int
width { numColumns, pieceLength } =
    let
        (PieceLength l) =
            pieceLength

        (NumColumns c) =
            numColumns
    in
        (l * c)


{-| The board's drawn height
-}
height : Properties -> Int
height { numRows, pieceLength } =
    let
        (PieceLength l) =
            pieceLength

        (NumRows r) =
            numRows
    in
        (l * r)


unwrapPieceLength : PieceLength -> Int
unwrapPieceLength (PieceLength pl) =
    pl



-- VIEW --


rowsFormGroup : (String -> msg) -> NumRows -> List (Html msg)
rowsFormGroup updateNumRowsMsg (NumRows numRows) =
    [ formGroup
        [ label [ for "boardRows" ] [ (text "Rows") ]
        , input
            [ type_ "number"
            , value (toString numRows)
            , class "form-control"
            , id "boardRows"
            , onInput updateNumRowsMsg
            ]
            []
        ]
    ]


columnsFormGroup : (String -> msg) -> NumColumns -> List (Html msg)
columnsFormGroup updateNumColumnsMsg (NumColumns numColumns) =
    [ formGroup
        [ label [ for "boardColumns" ] [ (text "Columns") ]
        , input
            [ type_ "number"
            , value (toString numColumns)
            , class "form-control"
            , id "boardColumns"
            , onInput updateNumColumnsMsg
            ]
            []
        ]
    ]


colorsFormGroup : (String -> msg) -> NumColors -> List (Html msg)
colorsFormGroup updateNumColorsMsg (NumColors numColors) =
    [ formGroup
        [ label [ for "numColors" ] [ (text "# of Colors") ]
        , input
            [ type_ "number"
            , value (toString numColors)
            , class "form-control"
            , id "numColors"
            , onInput updateNumColorsMsg
            ]
            []
        ]
    ]

view : (String -> msg) -> (String -> msg) -> (String -> msg) -> Properties -> Html msg
view updateNumRowsMsg updateNumColumnsMsg updateNumColorsMsg { numRows, numColumns, numColors } =
    div [ class "row justify-content-md-center" ]
        [ div [ class "col-md-3" ]
            (rowsFormGroup updateNumRowsMsg numRows)
        , div [ class "col-md-3" ]
            (columnsFormGroup updateNumColumnsMsg numColumns)
        , div [ class "col-md-3" ]
            (colorsFormGroup updateNumColorsMsg numColors)
        ]
