module Board exposing (..)

import Board.Piece exposing (Position, Piece)
import Board.Properties exposing (NumRows, NumColumns, NumColors, PieceLength, Properties)
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
    , pieceLength : Board.Piece.Length
    , colorPalette : Array Color
    }


type Height
    = Height Int


type Width
    = Width Int


type Index
    = Index Int


default : Board
default =
    Board (Rows 10) (Columns 5) (Board.Piece.Length 25) (Array.fromList [])


pieceYPos : Properties -> Index -> Int
pieceYPos properties (Index idx) =
    let
        c =
            Board.Properties.numColumnsValue properties

        l =
            Board.Properties.pieceLengthValue properties
    in
        (idx // c)
            |> (*) l


pieceXPos : Properties -> Index -> Int
pieceXPos properties (Index idx) =
    let
        c =
            Board.Properties.numColumnsValue properties

        l =
            Board.Properties.pieceLengthValue properties
    in
        (idx % c)
            |> (*) l


piecePos : Properties -> Index -> Board.Piece.Position
piecePos properties idx =
    Board.Piece.Position (pieceXPos properties idx) (pieceYPos properties idx)


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


updateColumnsFromString : String -> Board -> Result String Board
updateColumnsFromString rawNumCols board =
    String.toInt rawNumCols
        |> Result.map (clamp 1 100)
        |> Result.map Columns
        |> Result.map (updateColumns board)


rowValue : Board -> Int
rowValue { rows } =
    let
        (Rows rowsInt) =
            rows
    in
        rowsInt


columnValue : Board -> Int
columnValue { columns } =
    let
        (Columns colsInt) =
            columns
    in
        colsInt


numColors : Board -> Int
numColors { colorPalette } =
    Array.length colorPalette


createIndexedPiece : Int -> Maybe Piece -> ( Index, Maybe Piece )
createIndexedPiece index maybePiece =
    ( Index index, maybePiece )
