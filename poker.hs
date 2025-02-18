module Poker where

import Data.List
import Data.Maybe

-- Cards --
data Suit = Club | Diamond | Heart | Spade
  deriving (Read, Show, Enum, Eq, Ord)
data CardValue = NoValue | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Read, Show, Enum, Eq, Ord)

instance Ord Card
  where compare (Card v1 _) (Card v2 _) = compare v1 v2
        compare (Joker v1)  (Joker v2)  = compare v1 v2
        compare (Card v1 _) (Joker v2)  = if v1 == v2 then GT else compare v1 v2
        compare (Joker v1)  (Card v2 _) = if v1 == v2 then LT else compare v1 v2

data Card = Card CardValue Suit | Joker CardValue
  deriving (Read, Show, Eq)

type Hand = [Card]

data HandType = HighCard | Pair | TwoPair | ThreeofaKind | Straight | Flush |
                FullHouse | FourofaKind | StraightFlush | FiveofaKind |
                FlushHouse | FlushFive | TwoTriplet | FullHouse4_2 | ThreePair |
                SixofaKind | FlushSix
  deriving (Read, Show, Enum, Eq, Ord)  

-- Helper functions -- 
compare_tuple :: [Card] -> [Card] -> Ordering
compare_tuple [] _ = LT
compare_tuple _ [] = GT
compare_tuple (card1:_) (card2:_) = compare card1 card2

compare_hand :: [Card] -> [Card] -> Ordering
compare_hand [] _ = LT
compare_hand _ [] = GT
compare_hand (card1:r1) (card2:r2) = case compare card1 card2 of
  EQ -> compare_hand r1 r2
  other -> other

is_joker :: Card -> Bool
is_joker (Joker _) = True
is_joker _ = False

count_jokers :: [Card] -> Int
count_jokers = length . filter is_joker

remove_jokers :: [Card] -> [Card]
remove_jokers  = filter (not . is_joker)

fill_ntuple_with_jokers :: Int -> [Card] -> [Card]
fill_ntuple_with_jokers n [] = replicate n (Joker Ace)
fill_ntuple_with_jokers n (cards@((Card v _):_)) =
  cards ++ (replicate (n - (length cards)) (Joker v))
fill_ntuple_with_jokers _ _ = error "ntuple had jokers"

subtract_hand :: Hand -> Int -> [Card] -> (Int, [Card])
subtract_hand used njokers cards = (njokers - (count_jokers used),
                                    cards \\ (remove_jokers used))

valid_hands :: Int -> [Hand] -> [Hand]
valid_hands handsize = filter ((>= handsize) . length)

has_value :: CardValue -> Card -> Bool
has_value value (Card v _) = value == v
has_value value (Joker v) = value == v

has_suit :: Suit -> Card -> Bool
has_suit suit (Card _ s) = suit == s
has_suit _ (Joker _) = True

take_card_by_value :: CardValue -> [Card] -> ([Card], Maybe Card)
take_card_by_value v cards = case find (has_value v) cards of
  Just card -> (delete card cards, Just card)
  Nothing -> (cards, Nothing)

split_suits :: [Card] -> [[Card]]
split_suits cards = [filter (has_suit suit) cards | suit <- [Club, Diamond, Heart, Spade]]

best_hand :: [Hand] -> Maybe Hand
best_hand [] = Nothing
best_hand cards = Just (maximumBy compare_hand cards)

first_hand :: [Hand] -> Maybe Hand
first_hand [] = Nothing
first_hand (hand:_) = Just hand

insert_jokers_in_order :: Int -> [Card] -> [Card]
insert_jokers_in_order n cards =
  sortBy (flip compare) $ cards ++ (take n (repeat (Joker Ace)))

insert_joker_tuple :: [[Card]] -> [[Card]]
insert_joker_tuple [] = []
insert_joker_tuple cards@(ntuple:rest) = case ntuple of
  []               -> error "shouldn't have empty tuples yet"
  ((Joker _):_)    -> error "shouldn't have joker tuples yet"
  ((Card Ace _):_) -> [ntuple, []] ++ rest
  ((Card _ _):_)   -> ([]:cards)

append_to_maybe :: Hand -> Maybe Hand -> Maybe Hand
append_to_maybe h1 (Just h2) = Just (h1 ++ h2)
append_to_maybe _ Nothing = Nothing

-- Hand identification --
-- All functions assume cards are sorted high to low.

add_kickers :: [Card] -> Int -> Int -> [Card] -> Hand
add_kickers used njokers handsize cards =
  used ++ (take num_cards $ insert_jokers_in_order njokers' cards')
  where (njokers', cards') = subtract_hand used njokers cards
        num_cards = handsize - (length used)

all_ntuples :: Int -> Int -> Int -> [Card] -> [[Card]]
all_ntuples n njokers _ =
    map (fill_ntuple_with_jokers n . take n)
  . filter ((>= n - njokers) . length)
  . insert_joker_tuple
  . groupBy (<=)

get_high_card_hand :: Int -> Int -> [Card] -> Maybe Hand
get_high_card_hand njokers handsize cards
  | length cards < handsize = Nothing
  | otherwise = Just (add_kickers [] njokers handsize cards)

get_ntuple_hand :: Int -> Int -> Int -> [Card] -> Maybe Hand
get_ntuple_hand n njokers handsize cards = case all_ntuples n njokers handsize cards of
    [] -> Nothing
    (hand:_) -> Just (add_kickers hand njokers handsize cards)

get_2_ntuple_hand :: Int -> Int -> Int -> Int -> [Card] -> Maybe Hand
get_2_ntuple_hand n1 n2 njokers handsize cards = first_hand
  $ mapMaybe (\(c1, (j2, c2)) -> append_to_maybe c1 (get_ntuple_hand n2 j2 (handsize-n1) c2))
  $ map (\c -> (c, subtract_hand c njokers cards))
  $ all_ntuples n1 njokers handsize cards

get_3_ntuple_hand :: Int -> Int -> Int -> Int -> Int -> [Card] -> Maybe Hand
get_3_ntuple_hand n1 n2 n3 njokers handsize cards = first_hand
  $ mapMaybe (\(c1, (j2, c2)) -> append_to_maybe c1 (get_2_ntuple_hand n2 n3 j2 (handsize-n1) c2))
  $ map (\c -> (c, subtract_hand c njokers cards))
  $ all_ntuples n1 njokers handsize cards

match_straight :: Int -> Int -> [Card] -> [CardValue] -> Maybe Hand
match_straight _ 0 _ _ = Just []
match_straight _ _ _ [] = Nothing
match_straight njokers handsize cards (m:match) = case take_card_by_value m cards of
  (_, Nothing) -> if njokers > 0
                  then append_to_maybe [Joker m] (match_straight (njokers-1) (handsize-1) cards match)
                  else Nothing
  (cards', Just card) -> append_to_maybe [card] (match_straight njokers (handsize-1) cards' match)

get_straight :: Int -> Int -> [Card] -> Maybe Hand
get_straight njokers handsize cards =
    first_hand $ mapMaybe (match_straight njokers handsize cards)
    [[handtype, pred handtype .. Two] ++ [Ace] | handtype <- [Ace, King .. Two]]

get_flush :: Int -> Int -> [Card] -> Maybe Hand
get_flush njokers handsize =
    best_hand
  . mapMaybe (get_high_card_hand njokers handsize)
  . split_suits

get_flush_straight :: Int -> Int -> [Card] -> Maybe Hand
get_flush_straight njokers handsize =
    best_hand
  . valid_hands handsize
  . mapMaybe (get_straight njokers handsize)
  . split_suits

-- these should potentially return kickers? currently handsize is unused
get_flush_ntuple :: Int -> Int -> Int -> [Card] -> Maybe Hand
get_flush_ntuple n njokers _ =
    best_hand
  . mapMaybe (get_ntuple_hand n njokers n)
  . split_suits

get_flush_2_ntuple :: Int -> Int -> Int -> Int -> [Card] -> Maybe Hand
get_flush_2_ntuple n1 n2 njokers _ =
    best_hand
  . mapMaybe (get_2_ntuple_hand n1 n2 njokers (n1+n2))
  . split_suits

get_flush_3_ntuple :: Int -> Int -> Int -> Int -> Int -> [Card] -> Maybe Hand
get_flush_3_ntuple n1 n2 n3 njokers _ =
    best_hand
  . mapMaybe (get_3_ntuple_hand n1 n2 n3 njokers (n1+n2+n3))
  . split_suits
