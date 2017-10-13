module Board
    exposing
        ( Board
        , Piece
        , Index
        , default
        , numPieces
        , numColorsValue
        , updateNumRowsFromString
        , updateNumColorsFromString
        , updateNumColumnsFromString
        , maybeColorsToPieces
        , pieceCoordinates
        , pieceLengthValue
        , numColumnsValue
        , numRowsValue
        , dimensionsValue
        , removeBlockAt
        )

import Color exposing (Color)
import Array exposing (Array)
import List.Extra as Lextra


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


type alias Piece =
    { color : Color
    }


type alias Board =
    { properties : Properties
    , colorPalette : Array Color
    , pieces : Array ( Index, Maybe Piece )
    }


type Height
    = Height Int


type Width
    = Width Int


type alias Coordinates =
    { xCoord : Int
    , yCoord : Int
    }


type alias Position =
    { row : Int
    , column : Int
    }


type ColorBlockIndices
    = ColorBlockIndices (List Index)


type DestinationIndices
    = DestinationIndices (List Index)


defaultProperties : Properties
defaultProperties =
    Properties (NumRows 10) (NumColumns 5) (NumColors 3) (PieceLength 25)


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


{-| The row the piece is located
-}
pieceRow : Board -> Index -> Int
pieceRow board (Index idx) =
    let
        c =
            numColumnsValue board
    in
        (idx // c)


{-| The piece's y-coord
-}
pieceYPos : Board -> Index -> Int
pieceYPos board index =
    let
        l =
            pieceLengthValue board
    in
        pieceRow board index
            |> (*) l


{-| The column the piece is located
-}
pieceColumn : Board -> Index -> Int
pieceColumn board (Index idx) =
    let
        c =
            numColumnsValue board
    in
        (idx % c)


{-| The piece's x-coord
-}
pieceXPos : Board -> Index -> Int
pieceXPos board index =
    let
        l =
            pieceLengthValue board
    in
        pieceColumn board index
            |> (*) l


piecePosition : Board -> Index -> Position
piecePosition board idx =
    Position (pieceRow board idx) (pieceColumn board idx)


positionToIndex : Board -> Position -> Index
positionToIndex board { row, column } =
    let
        numCols =
            numColumnsValue board
    in
        Index (numCols * row + column)


neighborIndices : Board -> Index -> List Index
neighborIndices board =
    (piecePosition board)
        >> (neighborPositionsInbound board)
        >> List.map (positionToIndex board)


pieceIsColor : Color -> Piece -> Bool
pieceIsColor color piece =
    color == piece.color


colorNeighborToIndex : Color -> List Index -> ( Index, Maybe Piece ) -> Maybe Index
colorNeighborToIndex color neighborIndices ( idx, maybePiece ) =
    let
        isColor =
            maybePiece
                |> Maybe.map (pieceIsColor color)
                |> Maybe.withDefault False

        isNeighbor =
            List.member idx neighborIndices
    in
        if isColor && isNeighbor then
            Just idx
        else
            Nothing


colorNeighborIndices : Board -> Color -> Index -> List Index
colorNeighborIndices board color index =
    let
        nIndices =
            neighborIndices board index
    in
        board.pieces
            |> Array.toList
            |> List.filterMap (colorNeighborToIndex color nIndices)


findIndexedPiece : Index -> Array ( Index, Maybe Piece ) -> Maybe ( Index, Maybe Piece )
findIndexedPiece idx =
    Array.toList
        >> Lextra.find (\( i, maybePiece ) -> i == idx)


indexedPieceToColor : ( Index, Maybe Piece ) -> Maybe Color
indexedPieceToColor ( idx, maybePiece ) =
    maybePiece
        |> Maybe.map .color


findIndexedColor : Index -> Array ( Index, Maybe Piece ) -> Maybe Color
findIndexedColor idx =
    findIndexedPiece idx
        >> Maybe.andThen indexedPieceToColor


fcb : Color -> Board -> ColorBlockIndices -> DestinationIndices -> List Index
fcb color board (ColorBlockIndices colorBlock) (DestinationIndices destinations) =
    case Lextra.uncons destinations of
        Just ( blockIndex, rest ) ->
            let
                updatedColorBlock =
                    blockIndex :: colorBlock
            in
                blockIndex
                    |> (colorNeighborIndices board color)
                    |> Lextra.filterNot (\idx -> List.member idx updatedColorBlock)
                    |> List.append rest
                    |> DestinationIndices
                    |> (fcb color board (ColorBlockIndices updatedColorBlock))

        Nothing ->
            colorBlock


findColorBlock : Board -> Index -> List Index
findColorBlock board idx =
    let
        color =
            findIndexedColor idx board.pieces
    in
        case color of
            Just col ->
                let
                    toVisit =
                        colorNeighborIndices board col idx

                    colorBlock =
                        [ idx ]
                in
                    fcb col board (ColorBlockIndices colorBlock) (DestinationIndices toVisit)

            Nothing ->
                []


positionInbound : Board -> Position -> Bool
positionInbound board { row, column } =
    let
        numColumns =
            numColumnsValue board

        numRows =
            numRowsValue board
    in
        case ( column >= 0, column < numColumns, row >= 0, row < numRows ) of
            ( True, True, True, True ) ->
                True

            _ ->
                False


neighborPositions : Position -> List Position
neighborPositions { row, column } =
    [ (Position row (column - 1)) -- north
    , (Position row (column + 1)) -- south
    , (Position (row - 1) column) -- east
    , (Position (row + 1) column) -- west
    ]


neighborPositionsInbound : Board -> Position -> List Position
neighborPositionsInbound board position =
    neighborPositions position
        |> List.filter (positionInbound board)


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


updateNumColumns : Board -> NumColumns -> Board
updateNumColumns ({ properties } as board) numColumns =
    { properties | numColumns = numColumns }
        |> updateProperties board


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



-- Public API


{-| Remove a block of pieces at the specified index
-}
removeBlockAt : Board -> Index -> Board
removeBlockAt board index =
    let
        colorBlockIndices =
            findColorBlock board index
    in
        if (List.length colorBlockIndices) >= 3 then
            board.pieces
                |> Array.toList
                |> Lextra.filterNot (\( i, mp ) -> List.member i colorBlockIndices)
                |> Array.fromList
                |> (updatePieces board)
        else
            board


{-| Create a default board
-}
default : Board
default =
    Board defaultProperties (Array.fromList []) (Array.fromList [])


{-| Get the total # of possible pieces on the board.
-}
numPieces : Board -> Int
numPieces board =
    let
        r =
            numRowsValue board

        c =
            numColumnsValue board
    in
        r * c


{-| The board's # of colors.
-}
numColorsValue : Board -> Int
numColorsValue { properties } =
    let
        (NumColors c) =
            properties.numColors
    in
        c


{-| Update the # of board rows from a user inputted string.
-}
updateNumRowsFromString : Board -> String -> Result String Board
updateNumRowsFromString board rawNumRows =
    String.toInt rawNumRows
        |> Result.map (clamp 1 100)
        |> Result.map NumRows
        |> Result.map (updateNumRows board)


{-| Update the # of board columns from a user inputted string.
-}
updateNumColumnsFromString : Board -> String -> Result String Board
updateNumColumnsFromString board rawNumColumns =
    String.toInt rawNumColumns
        |> Result.map (clamp 1 100)
        |> Result.map NumColumns
        |> Result.map (updateNumColumns board)


{-| Convert colors to pieces
-}
maybeColorsToPieces : Board -> Array (Maybe Color) -> Board
maybeColorsToPieces board =
    Array.map (Maybe.map Piece)
        >> Array.indexedMap toIndexedPiece
        >> (updatePieces board)


{-| The piece's coordinates
-}
pieceCoordinates : Board -> Index -> Coordinates
pieceCoordinates board idx =
    Coordinates (pieceXPos board idx) (pieceYPos board idx)


{-| Return the length of a board piece
-}
pieceLengthValue : Board -> Int
pieceLengthValue { properties } =
    let
        (PieceLength pl) =
            properties.pieceLength
    in
        pl


{-| Return the # of board columns
-}
numColumnsValue : Board -> Int
numColumnsValue { properties } =
    let
        (NumColumns c) =
            properties.numColumns
    in
        c


{-| Return the # of board rows
-}
numRowsValue : Board -> Int
numRowsValue { properties } =
    let
        (NumRows r) =
            properties.numRows
    in
        r


{-| Return the width & height of the board.
-}
dimensionsValue : Board -> ( Int, Int )
dimensionsValue board =
    let
        ( BoardWidth bw, BoardHeight bh ) =
            dimensions board
    in
        ( bw, bh )
