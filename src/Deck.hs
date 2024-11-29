module Deck where
import System.Random
import Data.List (sort)
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types

{- Card definitions and instances -}
data Suit = Spades | Clubs | Diamonds | Hearts
    deriving (Eq, Ord) -- Arbitrary ordering, doesn't really matter

data Value = 
      Ace | Two | Three | Four | Five | Six | Seven
    | Eight | Nine | Ten | Jack | Queen | King
    deriving (Eq, Enum, Bounded)

instance Ord Value where
    compare c1 c2 = compare (fromEnum c1) (fromEnum c2)


{- Card representation, creation, and helper functions -}
data Card = MkCard { cardSuit :: Suit, cardValue :: Value }
    deriving (Eq, Ord)

mkCard :: Suit -> Value -> Card
mkCard suit value = MkCard { cardSuit = suit, cardValue = value }

isRed :: Card -> Bool
isRed c = cardSuit c == Hearts || cardSuit c == Diamonds

isBlack :: Card -> Bool
isBlack = not . isRed


{- Card Show instances -}
redStr :: String -> String
redStr str = (setSGRCode [SetColor Foreground Vivid Red]) ++ str ++ (setSGRCode [])

instance Show Suit where
    show Spades   = "♠"
    show Clubs    = "♣"
    show Diamonds = redStr "♦"
    show Hearts   = redStr "♥"

instance Show Value where
    show Ace = "A"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show other = show (fromEnum other + 1)

instance Show Card where
    show card = padding ++ str
        where 
            padding = replicate (2 - length (show (cardValue card))) ' '
            str = show (cardSuit card) ++ show (cardValue card)

{- Deck construction -}

type Deck = [Card]

{- EXERCISE 1: Deck Creation -}
deckOf52 :: [Card]
deckOf52 = [MkCard suit value | suit <- [Spades, Clubs, Diamonds, Hearts], value <- [Ace ..King]]

{- You can use this to check whether your shuffled deck contains the correct
 - cards -}
deckCorrect :: Deck -> Bool
deckCorrect deck = sort deck == sort deckOf52

{- Shuffling -}

{- EXERCISE 2: Fisher-Yates Shuffle -}
shuffle :: StdGen -> [a] -> [a]
shuffle randomNumberGenerator deck = shuffleCards randomNumberGenerator deck (length deck - 1)

shuffleCards :: StdGen -> [a] -> Int -> [a]
shuffleCards _ deck 0 = deck
shuffleCards randomNumberGenerator deck currentIndex =
    let (randomIndex, anotherRandomNumberGenerator) = randomR (0, currentIndex) randomNumberGenerator
        shuffledDeck = swapCardsInDeck deck currentIndex randomIndex
    in shuffleCards anotherRandomNumberGenerator shuffledDeck (currentIndex - 1)

swapCardsInDeck :: [a] -> Int -> Int -> [a]
swapCardsInDeck deckOfCards swapPosition1 swapPosition2
    | swapPosition1 == swapPosition2 = deckOfCards
    | otherwise = 
        let cardAtPosition1 = deckOfCards !! swapPosition1
            cardAtPosition2 = deckOfCards !! swapPosition2
            shuffledDeckOfCards = 
                [ if positionInDeck == swapPosition1 then cardAtPosition2
                  else if positionInDeck == swapPosition2 then cardAtPosition1
                  else currentCard
                | (currentCard, positionInDeck) <- zip deckOfCards [0..] ]
        in shuffledDeckOfCards

{- shuffleDeck is called by Main.hs when setting up -}
shuffleDeck :: IO Deck
shuffleDeck = do
    gen <- initStdGen
    return $ shuffle gen deckOf52