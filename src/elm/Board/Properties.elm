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
