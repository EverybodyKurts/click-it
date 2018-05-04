module Board.Properties exposing (..)


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
