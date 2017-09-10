module Board exposing (..)

import Board.Piece as Piece exposing (Position)
import Color exposing (Color)
import Array exposing (Array)


-- MODEL --


type Rows
    = Rows Int


type Columns
    = Columns Int


type alias Board =
    { rows : Rows
    , columns : Columns
    , pieceLength : Piece.Length
    , colors : Array Color
    }


type Height
    = Height Int


type Width
    = Width Int


type Index
    = Index Int


default : Board
default =
    Board (Rows 10) (Columns 5) (Piece.Length 25) (Array.fromList [])


pieceYPos : Piece.Length -> Columns -> Index -> Int
pieceYPos (Piece.Length l) (Columns c) (Index i) =
    (i // c)
        |> (*) l


pieceXPos : Piece.Length -> Columns -> Index -> Int
pieceXPos (Piece.Length l) (Columns c) (Index idx) =
    (idx % c)
        |> (*) l


piecePos : Board -> Index -> Piece.Position
piecePos { pieceLength, columns } idx =
    Piece.Position (pieceXPos pieceLength columns idx) (pieceYPos pieceLength columns idx)


numPieces : Board -> Int
numPieces { rows, columns } =
    let
        ( Rows r, Columns c ) =
            ( rows, columns )
    in
        r * c


indices : Board -> List Index
indices { rows, columns } =
    let
        (Rows r) =
            rows

        (Columns c) =
            columns
    in
        List.range 0 ((r * c) - 1)
            |> List.map Index


width : Board -> Width
width { pieceLength, columns } =
    let
        (Piece.Length l) =
            pieceLength

        (Columns c) =
            columns
    in
        Width (l * c)


height : Board -> Height
height { pieceLength, rows } =
    let
        (Piece.Length l) =
            pieceLength

        (Rows r) =
            rows
    in
        Height (l * r)


dimensions : Board -> ( Width, Height )
dimensions board =
    ( width board, height board )


updateColumns : Board -> Columns -> Board
updateColumns board columns =
    { board | columns = columns }


updateRows : Board -> Rows -> Board
updateRows board rows =
    { board | rows = rows }


updateRowsFromString : String -> Board -> Result String Board
updateRowsFromString rawNumRows board =
    String.toInt rawNumRows
        |> Result.map (clamp 1 100)
        |> Result.map Rows
        |> Result.map (updateRows board)
