module Board exposing (..)

import Board.Piece exposing (Piece, Position, Color(..))
import Svg exposing (Svg, svg)
import Array exposing (Array)


-- MODEL --


type Rows
    = Rows Int


type Columns
    = Columns Int


type alias Board =
    { rows : Rows
    , columns : Columns
    , pieceLength : Board.Piece.Length
    }


type Height
    = Height Int


type Width
    = Width Int


type Index
    = Index Int


default : Board
default =
    Board (Rows 10) (Columns 5) (Board.Piece.Length 25)


pieceYPos : Board.Piece.Length -> Columns -> Index -> Int
pieceYPos (Board.Piece.Length l) (Columns c) (Index i) =
    ((toFloat i) / (toFloat c))
        |> floor
        |> (*) l


pieceXPos : Board.Piece.Length -> Columns -> Index -> Int
pieceXPos (Board.Piece.Length l) (Columns c) (Index idx) =
    (idx % c)
        |> (*) l


piecePos : Board.Piece.Length -> Columns -> Index -> Board.Piece.Position
piecePos len cols idx =
    Board.Piece.Position (pieceXPos len cols idx) (pieceYPos len cols idx)


initialize : Board -> Array Piece
initialize { rows, columns, pieceLength } =
    let
        (Rows r) =
            rows

        (Columns c) =
            columns

        indices =
            List.range 0 ((r * c) - 1)
    in
        Array.fromList []


width : Board -> Width
width { pieceLength, columns } =
    let
        (Board.Piece.Length l) =
            pieceLength

        (Columns c) =
            columns
    in
        Width (l * c)


height : Board -> Height
height { pieceLength, rows } =
    let
        (Board.Piece.Length l) =
            pieceLength

        (Rows r) =
            rows
    in
        Height (l * r)


dimensions : Board -> ( Width, Height )
dimensions board =
    ( width board, height board )
