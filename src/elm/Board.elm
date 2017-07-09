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


type PixelHeight
    = PixelHeight Int


type PixelWidth
    = PixelWidth Int


default : Board
default =
    Board (BoardHeight 10) (BoardWidth 5) (PieceLength 25)


pixelWidth : Board -> PixelWidth
pixelWidth { pieceLength, width } =
    let
        (PieceLength pLength) =
            pieceLength

        (BoardWidth bdWidth) =
            width
    in
        PixelWidth (pLength * bdWidth)


pixelHeight : Board -> PixelHeight
pixelHeight { pieceLength, height } =
    let
        (PieceLength pLength) =
            pieceLength

        (BoardHeight bdHeight) =
            height
    in
        PixelHeight (pLength * bdHeight)


dimensions : Board -> ( PixelHeight, PixelWidth )
dimensions board =
    ( pixelHeight board, pixelWidth board )
