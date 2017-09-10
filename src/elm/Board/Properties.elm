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


{-| Get raw value of a board's piece length
-}
pieceLengthValue : Properties -> Int
pieceLengthValue { pieceLength } =
    let
        (PieceLength pl) =
            pieceLength
    in
        pl
