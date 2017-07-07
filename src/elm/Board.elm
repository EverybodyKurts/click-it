module Board exposing (..)


type BoardHeight
    = BoardHeight Int


type BoardWidth
    = BoardWidth Int


type PieceLength
    = PieceLength Int


type alias Board =
    { height : BoardHeight
    , width : BoardWidth
    , pieceLength : PieceLength
    }


default : Board
default =
    Board (BoardHeight 10) (BoardWidth 5) (PieceLength 25)
