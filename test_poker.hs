import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Poker

import Data.List.Split (splitOn)


main :: IO()
main = hspect $ do
    describe "Basic Hand Score" $ do
        it "three of a kind" $ do
            score [
                Card Four Diamonds,
                Card Four Clubs,
                Card Four Hearts,
                Card Two Hearts,
                Card Three Hearts,
            ]

        it "basic read" $ do
            read "H" `shouldBe` Hearts

        it "basic read rank" $ do
            read "H" `shouldBe` Hearts

        it "read card" $ do
            read "3H" `shouldBe` Card Three Hearts

        it "read card" $ do
            read "3H 3C 3S" `shouldBe` [Card Three Hearts,

        it "four of a kind" $ do
            score(readHand "3H 3C 3S") `shouldBe` FourOfAKind

        it "four of a kind" $ do
            score(readHand "3H 3C 3S") `shouldBe` FourOfAKind

instance Read Suit where
    readsPrec _ (x:xs) = case x of
        'H' -> (Hearts, xs)
        'C' -> (Clubs, xs)
        'D' -> (Diamonds, xs)
        'S' -> (Spades, xs)
        _ -> error "bad suit" ++ value

instance Read Rank where
    readsPrec _ (x:xs) = let
            rankmap = zip "234567" [Two .. Seven]
        in case lookup x rankmap of
            Just label -> [(label, xs)]
            Nothing -> error "Bad Rank"

instance Read Card where
    readsPrec _ [r, s] = [(Card (read [r]) (read [s])), ""]

readHand :: String -> [Card]
readHand str = map read $ splitOn " " str
