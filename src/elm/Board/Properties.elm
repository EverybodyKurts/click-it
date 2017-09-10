module Board.Properties exposing (..)


type NumRows
    = NumRows Int


type NumColumns
    = NumColumns Int


type NumColors
    = NumColors Int


type PieceLength
    = PieceLength Int


type BoardWidth
    = BoardWidth Int


type BoardHeight
    = BoardHeight Int


type alias Properties =
    { numRows : NumRows
    , numColumns : NumColumns
    , numColors : NumColors
    , pieceLength : PieceLength
    }


default : Properties
default =
    Properties (NumRows 10) (NumColumns 5) (NumColors 3) (PieceLength 25)


{-| Get raw value of a board's # of columns
-}
numColumnsValue : Properties -> Int
numColumnsValue { numColumns } =
    let
        (NumColumns c) =
            numColumns
    in
        c


numRowsValue : Properties -> Int
numRowsValue { numRows } =
    let
        (NumRows r) =
            numRows
    in
        r


numColorsValue : Properties -> Int
numColorsValue { numColors } =
    let
        (NumColors c) =
            numColors
    in
        c


{-| Get raw value of a board's piece length
-}
pieceLengthValue : Properties -> Int
pieceLengthValue { pieceLength } =
    let
        (PieceLength pl) =
            pieceLength
    in
        pl


numPieces : Properties -> Int
numPieces properties =
    let
        r =
            numRowsValue properties

        c =
            numColumnsValue properties
    in
        r * c


rawIndices : Properties -> List Int
rawIndices properties =
    let
        r =
            numRowsValue properties

        c =
            numColumnsValue properties
    in
        List.range 0 ((r * c) - 1)


boardWidth : Properties -> BoardWidth
boardWidth { pieceLength, numColumns } =
    let
        (PieceLength l) =
            pieceLength

        (NumColumns c) =
            numColumns
    in
        BoardWidth (l * c)


boardHeight : Properties -> BoardHeight
boardHeight { pieceLength, numRows } =
    let
        (PieceLength l) =
            pieceLength

        (NumRows r) =
            numRows
    in
        BoardHeight (l * r)


dimensions : Properties -> ( BoardWidth, BoardHeight )
dimensions properties =
    ( boardWidth properties, boardHeight properties )


dimensionsValue : Properties -> ( Int, Int )
dimensionsValue properties =
    let
        ( BoardWidth bw, BoardHeight bh ) =
            dimensions properties
    in
        ( bw, bh )


updateNumColumns : Properties -> NumColumns -> Properties
updateNumColumns properties numColumns =
    { properties | numColumns = numColumns }


updateNumRows : Properties -> NumRows -> Properties
updateNumRows properties numRows =
    { properties | numRows = numRows }


updateNumRowsFromString : Properties -> String -> Result String Properties
updateNumRowsFromString properties rawNumRows =
    String.toInt rawNumRows
        |> Result.map (clamp 1 100)
        |> Result.map NumRows
        |> Result.map (updateNumRows properties)


updateNumColumnsFromString : Properties -> String -> Result String Properties
updateNumColumnsFromString properties rawNumColumns =
    String.toInt rawNumColumns
        |> Result.map (clamp 1 100)
        |> Result.map NumColumns
        |> Result.map (updateNumColumns properties)
