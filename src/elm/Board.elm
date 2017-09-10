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


type Index
    = Index Int


type alias Board =
    { properties : Properties
    , colorPalette : Array Color
    , pieces : Array ( Index, Maybe Piece )
    }


type Height
    = Height Int


type Width
    = Width Int


default : Board
default =
    Board Board.Properties.default (Array.fromList []) (Array.fromList [])


pieceYPos : Board -> Index -> Int
pieceYPos { properties } (Index idx) =
    let
        c =
            Board.Properties.numColumnsValue properties

        l =
            Board.Properties.pieceLengthValue properties
    in
        (idx // c)
            |> (*) l


pieceXPos : Board -> Index -> Int
pieceXPos { properties } (Index idx) =
    let
        c =
            Board.Properties.numColumnsValue properties

        l =
            Board.Properties.pieceLengthValue properties
    in
        (idx % c)
            |> (*) l


piecePos : Board -> Index -> Board.Piece.Position
piecePos board idx =
    Board.Piece.Position (pieceXPos board idx) (pieceYPos board idx)


indices : Properties -> List Index
indices =
    Board.Properties.rawIndices
        >> List.map Index


createIndexedPiece : Int -> Maybe Piece -> ( Index, Maybe Piece )
createIndexedPiece index maybePiece =
    ( Index index, maybePiece )


numPieces : Board -> Int
numPieces { properties } =
    Board.Properties.numPieces properties


updateProperties : Board -> Properties -> Board
updateProperties board properties =
    { board | properties = properties }


updateNumRowsFromString : Board -> String -> Result String Board
updateNumRowsFromString ({ properties } as board) =
    Board.Properties.updateNumRowsFromString properties
        >> Result.map (updateProperties board)


updateNumColumnsFromString : Board -> String -> Result String Board
updateNumColumnsFromString ({ properties } as board) =
    Board.Properties.updateNumColumnsFromString properties
        >> Result.map (updateProperties board)


updatePieces : Board -> Array ( Index, Maybe Piece ) -> Board
updatePieces board pieces =
    { board | pieces = pieces }


toIndexedPiece : Int -> Maybe Piece -> ( Index, Maybe Piece )
toIndexedPiece index piece =
    ( Index index, piece )
