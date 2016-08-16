module PokerKata where

import Data.List (nub, sort, group)

data Suit = Hearts | Spades | Clubs | Diamonds deriving (Eq)
data Rank = Two | Three | Four | Five | Six | Seven deriving (Enum, Ord, Eq)
data Card = Card Rank Suit
data Score = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse deriving (Show, Eq, Ord)


instance Show Card where
    show (Card r s) = (show r) ++ (show s)

instance Eq Card where
    (Card r1 s1) == (Card r2 s2) = r1 == r2

grouper :: [Card] -> [Int]
grouper = sort . map length . group . sort . map (\(Card r _) -> r)

-- Without the dots
-- grouper = sort(
--     map length(
--         group(
--             sort(
--                 map (\(Card r _) -> r
--             )
--         )
--     )
-- )
--
has_same_suits [Card] :: Bool
has_same_suits = (==1) lenght . nub . map(\(Card _ s) -> s


list_is_consecutive (x:y:xs) = case (succ x) of
    True -> list_is_consecutive(y:xs)
    False => False

list_is_consecutive _ = True

score :: [Card] -> Score
score cs
    -- StraighFlush
    | (grouper cs == [1,4]) = FourOfAKind
    | (grouper cs == [2,3]) = FullHouse
    | (grouper cs == [1,1,3]) = ThreeOfAKind
    | (has_same_suit cs) = Flush
    -- Straight
    | (grouper cs == [1,2,2]) = TwoPair
    | (grouper cs == [1,1,1,2]) = OnePair
    | otherwise = HighCard

main = putStrLn $ show $ score [
    Card Four Diamonds,
    Card Four Diamonds,
    Card Four Hearts,
    Card Two Hearts,
    Card Three Hearts,
]


-- Utility

instance Show Suit where
    show Hearts = "H"
    show Clubs = "C"
    show Diamonds = "D"
    show Spades = "S"

instance Show Rank where
    show r = let
        rankmap = zip [Two .. Seven] "234567"
            in case lookup r rankmap of
                Just label -> [label]
                Nothing -> error "Bad rank"
