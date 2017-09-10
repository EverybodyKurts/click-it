module Board exposing (..)

import Board.Piece exposing (Position, Piece)
import Color exposing (Color)
import Array exposing (Array)


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


defaultProperties : Properties
defaultProperties =
    Properties (NumRows 10) (NumColumns 5) (NumColors 3) (PieceLength 25)


{-| Get raw value of a board's # of columns
-}
numColumnsValue : Board -> Int
numColumnsValue { properties } =
    let
        (NumColumns c) =
            properties.numColumns
    in
        c


numRowsValue : Board -> Int
numRowsValue { properties } =
    let
        (NumRows r) =
            properties.numRows
    in
        r


numColorsValue : Board -> Int
numColorsValue { properties } =
    let
        (NumColors c) =
            properties.numColors
    in
        c


{-| Get raw value of a board's piece length
-}
pieceLengthValue : Board -> Int
pieceLengthValue { properties } =
    let
        (PieceLength pl) =
            properties.pieceLength
    in
        pl


numPieces : Board -> Int
numPieces board =
    let
        r =
            numRowsValue board

        c =
            numColumnsValue board
    in
        r * c


boardWidth : Board -> BoardWidth
boardWidth { properties } =
    let
        (PieceLength l) =
            properties.pieceLength

        (NumColumns c) =
            properties.numColumns
    in
        BoardWidth (l * c)


boardHeight : Board -> BoardHeight
boardHeight { properties } =
    let
        (PieceLength l) =
            properties.pieceLength

        (NumRows r) =
            properties.numRows
    in
        BoardHeight (l * r)


dimensions : Board -> ( BoardWidth, BoardHeight )
dimensions board =
    ( boardWidth board, boardHeight board )


dimensionsValue : Board -> ( Int, Int )
dimensionsValue board =
    let
        ( BoardWidth bw, BoardHeight bh ) =
            dimensions board
    in
        ( bw, bh )


default : Board
default =
    Board defaultProperties (Array.fromList []) (Array.fromList [])


pieceYPos : Board -> Index -> Int
pieceYPos board (Index idx) =
    let
        c =
            numColumnsValue board

        l =
            pieceLengthValue board
    in
        (idx // c)
            |> (*) l


pieceXPos : Board -> Index -> Int
pieceXPos board (Index idx) =
    let
        c =
            numColumnsValue board

        l =
            pieceLengthValue board
    in
        (idx % c)
            |> (*) l


piecePos : Board -> Index -> Board.Piece.Position
piecePos board idx =
    Board.Piece.Position (pieceXPos board idx) (pieceYPos board idx)


rawIndices : Board -> List Int
rawIndices board =
    let
        r =
            numRowsValue board

        c =
            numColumnsValue board
    in
        List.range 0 ((r * c) - 1)


indices : Board -> List Index
indices =
    rawIndices
        >> List.map Index


createIndexedPiece : Int -> Maybe Piece -> ( Index, Maybe Piece )
createIndexedPiece index maybePiece =
    ( Index index, maybePiece )


updateProperties : Board -> Properties -> Board
updateProperties board properties =
    { board | properties = properties }


updateNumRows : Board -> NumRows -> Board
updateNumRows ({ properties } as board) numRows =
    { properties | numRows = numRows }
        |> updateProperties board


updateNumRowsFromString board rawNumRows =
    String.toInt rawNumRows
        |> Result.map (clamp 1 100)
        |> Result.map NumRows
        |> Result.map (updateNumRows board)


updateNumColumns : Board -> NumColumns -> Board
updateNumColumns ({ properties } as board) numColumns =
    { properties | numColumns = numColumns }
        |> updateProperties board


updateNumColumnsFromString : Board -> String -> Result String Board
updateNumColumnsFromString board rawNumColumns =
    String.toInt rawNumColumns
        |> Result.map (clamp 1 100)
        |> Result.map NumColumns
        |> Result.map (updateNumColumns board)


updateNumColors : Board -> NumColors -> Board
updateNumColors ({ properties } as board) numColors =
    { properties | numColors = numColors }
        |> updateProperties board


updateNumColorsFromString : Board -> String -> Result String Board
updateNumColorsFromString board rawNumColors =
    String.toInt rawNumColors
        |> Result.map (clamp 1 100)
        |> Result.map NumColors
        |> Result.map (updateNumColors board)


updatePieces : Board -> Array ( Index, Maybe Piece ) -> Board
updatePieces board pieces =
    { board | pieces = pieces }


toIndexedPiece : Int -> Maybe Piece -> ( Index, Maybe Piece )
toIndexedPiece index piece =
    ( Index index, piece )


maybeColorsToPieces : Board -> Array (Maybe Color) -> Board
maybeColorsToPieces board =
    Array.map (Maybe.map Piece)
        >> Array.indexedMap toIndexedPiece
        >> (updatePieces board)
