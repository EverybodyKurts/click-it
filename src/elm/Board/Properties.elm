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


type RowIndex
    = RowIndex Int


type ColumnIndex
    = ColumnIndex Int


type XCoord
    = XCoord Int


type YCoord
    = YCoord Int



-- INITIALIZING BOARD PROPERTIES


default : Properties
default =
    Properties (NumRows 10) (NumColumns 5) (NumColors 3) (PieceLength 25)



-- ACCESSING PROPERTIES


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


{-| Update the number of rows
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


updateNumRowsOrDefault : Properties -> String -> Properties
updateNumRowsOrDefault properties =
    (updateNumRowsFromString properties)
        >> Result.withDefault default


updateNumColumns : Properties -> NumColumns -> Properties
updateNumColumns properties numColumns =
    { properties | numColumns = numColumns }


updateNumColumnsFromString : Properties -> String -> Result String Properties
updateNumColumnsFromString properties =
    String.toInt
        >> Result.map (clamp 1 100)
        >> Result.map NumColumns
        >> Result.map (updateNumColumns properties)


updateNumColumnsOrDefault : Properties -> String -> Properties
updateNumColumnsOrDefault properties =
    (updateNumColumnsFromString properties)
        >> Result.withDefault default


updateNumColors : Properties -> NumColors -> Properties
updateNumColors properties numColors =
    { properties | numColors = numColors }


updateNumColorsFromString : Properties -> String -> Result String Properties
updateNumColorsFromString properties =
    String.toInt
        >> Result.map (clamp 1 100)
        >> Result.map NumColors
        >> Result.map (updateNumColors properties)


updateNumColorsOrDefault : Properties -> String -> Properties
updateNumColorsOrDefault properties =
    (updateNumColorsFromString properties)
        >> Result.withDefault default



-- DRAWING THE BOARD


width : Properties -> Int
width { numColumns, pieceLength } =
    let
        (PieceLength l) =
            pieceLength

        (NumColumns c) =
            numColumns
    in
        (l * c)


height : Properties -> Int
height { numRows, pieceLength } =
    let
        (PieceLength l) =
            pieceLength

        (NumRows r) =
            numRows
    in
        (l * r)


xCoord : PieceLength -> ColumnIndex -> XCoord
xCoord (PieceLength pieceLength) (ColumnIndex columnIndex) =
    XCoord (pieceLength * columnIndex)


yCoord : PieceLength -> RowIndex -> YCoord
yCoord (PieceLength pieceLength) (RowIndex rowIndex) =
    YCoord (pieceLength * rowIndex)
