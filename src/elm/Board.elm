module Board exposing (..)

import Board.Piece exposing (Piece, Position, Color(..), PieceLength(..))
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
    , pieceLength : PieceLength
    }


type Height
    = Height Int


type Width
    = Width Int


type Index
    = Index Int


default : Board
default =
    Board (Rows 10) (Columns 5) (PieceLength 25)



-- yPos : BoardWidth -> PieceLength -> BoardIndex -> Int
-- yPos (BoardWidth w) (PieceLength l) (BoardIndex i) =
--     ((toFloat i) / (toFloat w))
--         |> floor
--         |> (*) l


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
        (PieceLength l) =
            pieceLength

        (Columns c) =
            columns
    in
        Width (l * c)


height : Board -> Height
height { pieceLength, rows } =
    let
        (PieceLength l) =
            pieceLength

        (Rows r) =
            rows
    in
        Height (l * r)


dimensions : Board -> ( Height, Width )
dimensions board =
    ( height board, width board )
