
module Game where
import Deck
import Error
import Data.List (sortBy)


{- Commands and instructions, representing moves to be made -}
type StackIndex = Int
type Count = Int
type FromStack = StackIndex
type ToStack = Int

-- An instruction is a top-level command.
data Instruction = Quit | Undo | GameCommand Command
    deriving (Show)

-- A Command is a move to be played in the game.
data Command = Move Count FromStack ToStack
             | MoveStack FromStack ToStack
             | Draw
             | MoveFromDiscard StackIndex
             | MoveToPillar CardSource
             | MoveFromPillar Suit StackIndex
             | Solve
        deriving (Show)

data CardSource = FromStack StackIndex | FromDiscard
    deriving (Show)

{- Board representation -}

-- A column is a list of (Card, Bool) pairs, where the Bool
-- represents visibility: true for visible, false for hidden
type Column = [(Card, Bool)]

-- The pillars / foundation stacks are represented as Maybe Card
-- values, where Nothing represents an empty pillar and Just c 
-- denotes that card 'c' is at the top of the pillar.
-- Note that we don't need to store all cards since they are stored
-- in ascending order.
data Pillars = MkPillars {
        spades :: Maybe Value,
        clubs :: Maybe Value,
        hearts :: Maybe Value,
        diamonds :: Maybe Value
  }
  deriving (Show, Eq)

emptyPillars :: Pillars
emptyPillars = MkPillars {
        spades = Nothing,
        clubs = Nothing,
        hearts = Nothing,
        diamonds = Nothing
    }

-- The board consists of a deck, discard pile, pillars, and 7 columns.
data Board = MkBoard {
    boardDeck :: [Card],
    boardDiscard :: [Card],
    boardPillars :: Pillars,
    boardColumns :: [Column]
} deriving (Eq)


{- EXERCISE 3: Show instance for the board -}
{- We recommend writing helper functions. -}
instance Show Board where
    show b = unlines
        [ showDeck b
        , showDiscard b
        , showPillars b
        , showColumns b
        ]

showDeck :: Board -> String
showDeck board = "\n Deck size: " ++ show (length (boardDeck board)) ++ " cards"

showDiscard :: Board -> String
showDiscard board = "Discard: " ++ if null (boardDiscard board)
    then "Empty board"
    else showDiscarded (boardDiscard board)
    where
        showDiscarded :: [Card] -> String
        showDiscarded [] = "Empty"
        showDiscarded (x:xs) = show x ++ " (+" ++ show (length xs) ++ ")"

showPillars :: Board -> String
showPillars board = "Pillars:\n" ++ concatMap (showPillar (boardPillars board)) [Spades, Clubs, Hearts, Diamonds]

showPillar :: Pillars -> Suit -> String
showPillar pillars suit = case getPillar pillars suit of
    Nothing -> "  " ++ suitName suit ++ ": <empty>\n"
    Just value -> "  " ++ suitName suit ++ ": [" ++ show value ++ "]\n"

suitName :: Suit -> String
suitName Spades   = "Spades"
suitName Clubs    = "Clubs"
suitName Hearts   = "Hearts"
suitName Diamonds = "Diamonds"


-- This function displays the board
padRight :: Int -> String -> String
padRight n str = str ++ replicate (n - length str) ' '

showColumns :: Board -> String
showColumns board =
    let
        columns = sortBy (\c1 c2 -> compare (length c1) (length c2)) (boardColumns board)  -- Sort columns by height
        maxHeight = maximum (map length columns)  -- Find the height of the tallest column
        headers = concatMap (\i -> padRight 7 $ "[" ++ show i ++ "]") [0 .. length columns - 1]  -- Column headers
        rows = [generateRow r columns maxHeight | r <- [0 .. maxHeight - 1]]  -- Generate rows, starting from top to bottom
    in
        headers ++ "\n" ++ unlines rows

-- Generate a single row of the display
generateRow :: Int -> [Column] -> Int -> String
generateRow rowIndex columns maxHeight =
    concatMap (\col -> let visibleRowIndex = length col - 1 - rowIndex
                in if visibleRowIndex >= 0
                    then padRight 7 (showCard (col !! visibleRowIndex)) 
                    else padRight 7 " ") columns

-- Show a card or placeholder for hidden cards
showCard :: (Card, Bool) -> String
showCard (card, visible) = if visible then show card else " ??? "










{- EXERCISE 4: Board Setup -}
setup :: Deck -> Board
setup deck = MkBoard {
    boardDeck = remaniningDeck,
    boardDiscard = [],
    boardPillars = emptyPillars,
    boardColumns = initialColumns
    }
    where
    (columnCards, remaniningDeck) = splitAt 28 deck
    initialColumns = createColumns [0..6] columnCards

createColumns :: [Int] -> [Card] -> [Column]
createColumns [] _ = []
createColumns (n:ns) cards =
    let (colCards, rest) = splitAt n cards
        column = reverse (zip colCards (replicate (n - 1) False ++ [True]))
    in column : createColumns ns rest



{- EXERCISE 5: Win checking -}
isWon :: Board -> Bool
isWon board = all isPillarFull [Spades, Clubs, Hearts, Diamonds]
    where
        pillars = boardPillars board
        isPillarFull suit = getPillar pillars suit == Just King


{- Pillar helper functions -}
-- Gets the pillar for a given suit.
getPillar :: Pillars -> Suit -> Maybe Value
getPillar ps Spades = spades ps
getPillar ps Clubs = clubs ps
getPillar ps Hearts = hearts ps
getPillar ps Diamonds = diamonds ps

-- Decrements a pillar. 
decValue :: Maybe Value -> Maybe Value
decValue Nothing = Nothing
decValue (Just Ace) = Nothing
decValue (Just x) = Just (pred x)

-- Increments a pillar.
incValue :: Maybe Value -> Maybe Value
incValue Nothing = Just Ace
incValue (Just x) = Just (succ x)

-- Increments the pillar for a given suit.
incPillar :: Pillars -> Suit -> Pillars
incPillar ps Spades = ps { spades = incValue (spades ps) }
incPillar ps Clubs = ps { clubs = incValue (clubs ps) }
incPillar ps Hearts = ps { hearts = incValue (hearts ps) }
incPillar ps Diamonds = ps { diamonds = incValue (diamonds ps) }

-- Decrements the pillar for a given suit.
decPillar :: Pillars -> Suit -> Pillars
decPillar ps Spades = ps { spades = decValue $ spades ps }
decPillar ps Clubs = ps { clubs = decValue $ clubs ps }
decPillar ps Hearts = ps { hearts = decValue $ hearts ps }
decPillar ps Diamonds = ps { diamonds = decValue $ diamonds ps }

{- EXERCISE 6: Helper Functions -}

--Flips the top card of all columns, if not already flipped
flipCards :: Board -> Board
flipCards board = board { boardColumns = map flipTopCard (boardColumns board) }
    where
        flipTopCard :: Column -> Column
        flipTopCard [] = []
        flipTopCard ((card, isVisible):rest)
            | isVisible = (card, isVisible) : rest
            | otherwise = (card, True) : rest



-- Checks whether it's possible to stack the first card onto the second.
canStack :: Card -> Card -> Bool
canStack card onto =
    (isRed card /= isRed onto) &&
    (fromEnum (cardValue card) == fromEnum (cardValue onto)-1)



-- Updates a column at the given index
updateColumn :: Int -> Column -> [Column] -> [Column]
updateColumn index newColumn columns = take index columns ++ [newColumn] ++ drop (index + 1) columns



-- Checks whether it's possible to place a card onto a pillar.
canStackOnPillar :: Card -> Maybe Value -> Bool
canStackOnPillar card Nothing = cardValue card == Ace
canStackOnPillar card (Just topValue) = fromEnum (cardValue card) == fromEnum topValue + 1




{- EXERCISE 7: Draw -}
draw :: Board -> Either Error Board
draw board
    | null (boardDeck board) && null (boardDiscard board) = Left DeckEmpty
    | null (boardDeck board) = Right board { boardDeck = reverse (boardDiscard board), boardDiscard = [] }
    | (x:xs) <- boardDeck board = Right board {boardDiscard = x : boardDiscard board, boardDeck = xs}
    | otherwise = Right board { boardDiscard = head (boardDeck board) : boardDiscard board, boardDeck = tail (boardDeck board) }



{- EXERCISE 8: Move -}
move :: Int -> Int -> Int -> Board -> Either Error Board
move count fromIndex toIndex board
    | fromIndex < 1 || fromIndex > length columns = Left InvalidStack
    | toIndex < 1 || toIndex > length columns = Left InvalidStack
    | length fromColumn < count = Left DeckEmpty
    | not (isValidStack movingCardsCards) = Left InvalidCommand
    | not (null toColumn) && not (canStack (last movingCardsCards) (fst (head toColumn))) = Left InvalidCommand
    | null toColumn && not (isKing (head movingCardsCards)) = Left InvalidCommand
    | otherwise = Right board { boardColumns = updatedColumns }
    where
        columns = boardColumns board
        fromColumn = columns !! (fromIndex - 1)
        toColumn = columns !! (toIndex - 1)
        movingCards = take count fromColumn
        movingCardsCards = map fst movingCards
        remainingCards = drop count fromColumn
        updatedFromColumn = flipTopCardIfHidden remainingCards
        updatedToColumn = movingCards ++ toColumn
        updatedColumns = updateColumn (fromIndex -1) updatedFromColumn (updateColumn (toIndex -1) updatedToColumn columns)

isValidStack :: [Card] -> Bool
isValidStack [] = True
isValidStack [_] = True
isValidStack (col1:col2:cs) = canStack col1 col2 && isValidStack (col2:cs)

isKing :: Card -> Bool
isKing card = cardValue card == King

flipTopCardIfHidden :: Column -> Column
flipTopCardIfHidden [] = []
flipTopCardIfHidden ((card, isVisible):rest)
    | isVisible = (card, isVisible):rest
    | otherwise = (card, True):rest





{- EXERCISE 9: Move Stack -}
moveStack :: Int -> Int -> Board -> Either Error Board
moveStack fromIndex toIndex board
    | fromIndex < 1 || fromIndex > length columns = Left InvalidStack
    | toIndex < 1 || toIndex > length columns = Left InvalidStack
    | null visibleCards = Left ColumnEmpty
    | not (null visibleCards) && not (null toColumn) && not (canStack (fst (head visibleCards)) (fst (head toColumn))) = Left InvalidCommand
    | otherwise = Right board { boardColumns = updatedColumns }
    where
        columns = boardColumns board
        fromColumn = columns !! (fromIndex - 1)
        toColumn = columns !! (toIndex - 1)
        visibleCards = takeWhile snd fromColumn
        remainingFromColumn = drop (length visibleCards) fromColumn
        updatedFromColumn = remainingFromColumn
        updatedToColumn = visibleCards ++ toColumn
        updatedColumns = updateColumn (fromIndex - 1) updatedFromColumn (updateColumn (toIndex - 1) updatedToColumn columns)





{- EXERCISE 10: Move from Discard -}
moveFromDiscard :: Int -> Board -> Either Error Board
moveFromDiscard idx board
    | null (boardDiscard board) = Left DiscardEmpty
    | idx < 1 || idx > length (boardColumns board) = Left InvalidStack
    | not (null targetColumn) && not (canStack topDiscard (fst (head targetColumn))) = Left WrongOrder
    | otherwise = Right updatedBoard
    where
        topDiscard = head (boardDiscard board)
        targetColumn = boardColumns board !! (idx - 1)
        updatedColumns = updateColumn (idx - 1) ((topDiscard, True) : targetColumn) (boardColumns board)
        updatedBoard = board { boardDiscard = tail (boardDiscard board), boardColumns = updatedColumns }




{- EXERCISE 11: Move to Pillar -}
moveToPillar :: CardSource -> Board -> Either Error Board
moveToPillar (FromStack index) board
    | index < 1 || index > length (boardColumns board) = Left InvalidStack
    | null fromColumn = Left ColumnEmpty
    | not (canStackOnPillar cardToMove currentPillarValue) = Left WrongPillarOrder
    | otherwise = Right updatedBoard
    where
        columns = boardColumns board
        fromColumn = columns !! (index - 1)
        (cardToMove, _) = head fromColumn
        suitOf (MkCard suit _) = suit
        currentPillarValue = getPillar (boardPillars board) (suitOf cardToMove)
        updatedPillars = incPillar (boardPillars board) (suitOf cardToMove)
        remainingFromColumn = tail fromColumn
        remainingFromColumn' = flipTopCardIfHidden remainingFromColumn
        updatedFromColumn = remainingFromColumn'
        updatedColumns = updateColumn (index - 1) updatedFromColumn columns
        updatedBoard = board { boardColumns = updatedColumns, boardPillars = updatedPillars }

moveToPillar FromDiscard board
    | null (boardDiscard board) = Left DiscardEmpty
    | not (canStackOnPillar cardToMove currentPillarValue) = Left WrongPillarOrder
    | otherwise = Right updatedBoard
    where
        cardToMove = head (boardDiscard board)
        suitOf (MkCard suit _) = suit
        currentPillarValue = getPillar (boardPillars board) (suitOf cardToMove)
        updatedPillars = incPillar (boardPillars board) (suitOf cardToMove)
        updatedDiscard = tail (boardDiscard board)
        updatedBoard = board { boardDiscard = updatedDiscard, boardPillars = updatedPillars }



{- EXERCISE 12: Move from Pillar -}
moveFromPillar :: Suit -> Int -> Board -> Either Error Board
moveFromPillar suit idx board
    | idx < 1 || idx > length (boardColumns board) = Left InvalidStack
    | currentPillarValue == Nothing = Left PillarEmpty
    | not (canPlaceInColumn cardToMove destinationColumn) = Left InvalidCommand
    | otherwise = Right updatedBoard
    where
        currentPillarValue = getPillar (boardPillars board) suit
        cardToMove = case currentPillarValue of
            Just value -> MkCard suit value
            Nothing -> error "Redundant case"
        destinationColumn = boardColumns board !! (idx - 1)

        canPlaceInColumn :: Card -> Column -> Bool
        canPlaceInColumn _ [] = True
        canPlaceInColumn card column = canStack card (fst (head column))
        updatedColumn = (cardToMove, True) : destinationColumn
        updatedColumns = updateColumn (idx - 1) updatedColumn (boardColumns board)
        updatedPillars = decPillar (boardPillars board) suit
        updatedBoard = board { boardColumns = updatedColumns, boardPillars = updatedPillars }


{- EXERCISE 13: Solve -}
solve :: Board -> Board
solve = solveHelper
    where
        solveHelper b = maybe b solveHelper (nextMoveToPillar b)
        nextMoveToPillar :: Board -> Maybe Board
        nextMoveToPillar b =
            case moveFromColumnsToPillars b of
                Just updatedBoard -> Just updatedBoard
                Nothing -> moveFromColumnsToPillars b

        moveFromColumnsToPillars :: Board -> Maybe Board
        moveFromColumnsToPillars b =
            foldl tryMove Nothing [0..(length (boardColumns b) - 1)]
            where
                tryMove :: Maybe Board -> Int -> Maybe Board
                tryMove ( Just updatedBoard) _ = Just updatedBoard
                tryMove Nothing idx =
                    case moveToPillar (FromStack idx) b of
                        Right updatedBoard -> Just updatedBoard
                        Left _ -> Nothing




{- Scaffolding: This checks input indexes and calls the relevant functions -}
checkStackIndex :: Int -> Either Error ()
checkStackIndex x | x >= 0 && x <= 6 = return ()
                  | otherwise = Left InvalidStack


makeMove' :: Command -> Board -> Either Error Board
makeMove' (Move count from to) b = do
    checkStackIndex from
    checkStackIndex to
    move count from to b
makeMove' (MoveStack from to) b = do
    checkStackIndex from
    checkStackIndex to
    moveStack from to b
-- If deck nonempty, move a card from the top of the deck to the top of the discard pile
-- If deck empty, reverse discard pile and put it back as deck
makeMove' Draw b = draw b
makeMove' (MoveFromDiscard idx) b = checkStackIndex idx >> moveFromDiscard idx b
-- Take the top card from the given stack and move to pillar -- if possible
makeMove' (MoveToPillar source) b =
    case source of
        FromDiscard -> moveToPillar source b
        FromStack idx -> checkStackIndex idx >> moveToPillar source b
makeMove' (MoveFromPillar suit idx) b = checkStackIndex idx >> moveFromPillar suit idx b
makeMove' Solve b = Right $ solve b

makeMove :: Command -> Board -> Either Error Board
makeMove cmd b = fmap flipCards (makeMove' cmd b)
