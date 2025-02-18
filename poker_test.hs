import Data.List
import Test.HUnit

import Poker

test_values_only vs = return (sortBy (flip compare) (map (\v -> Card v Heart) vs))
test_cards cs = return (sortBy (flip compare) cs)
 
test_all_ntuples = TestCase ( 
  do c1 <- test_values_only [Three, Two, Two] 
     assertEqual "NormalPair" (all_ntuples 2 0 5 c1) [[Card Two Heart, Card Two Heart]] 
     assertEqual "HighJokerPair" (all_ntuples 2 2 5 c1) 
       [[Joker Ace, Joker Ace], [Card Three Heart, Joker Three], [Card Two Heart, Card Two Heart]] 
     c2 <- test_values_only [Ace, Jack, Jack] 
     assertEqual "OneJokerPair" (all_ntuples 2 3 5 c2) 
       [[Card Ace Heart, Joker Ace], [Joker Ace, Joker Ace], [Card Jack Heart, Card Jack Heart]] 
     c3 <- test_values_only [Four, Four, Three, Three, Two, Two] 
     assertEqual "NormalPair2" (all_ntuples 2 1 5 c3) 
       [[Card Four Heart, Card Four Heart], [Card Three Heart, Card Three Heart], 
        [Card Two Heart, Card Two Heart]] 
     c4 <- test_values_only [Ace, Ace] 
     assertEqual "LowJokerPair" (all_ntuples 2 2 5 c4)
       [[Card Ace Heart, Card Ace Heart], [Joker Ace, Joker Ace]] 
     c5 <- test_values_only [Ace, King, Seven, Six, Three] 
     assertEqual "NoPair" (all_ntuples 2 0 5 c5) []) 
 
test_get_pair = TestCase ( 
  do c1 <- test_cards [Card Five Heart, Card Four Heart, Card Three Heart, Card Two Heart, Card Two Heart] 
     assertEqual "NormalPair" (get_ntuple_hand 2 0 5 c1) 
       (Just [Card Two Heart, Card Two Heart, Card Five Heart, Card Four Heart, Card Three Heart])
     assertEqual "HighJokerPair" (get_ntuple_hand 2 2 5 c1) 
       (Just [Joker Ace, Joker Ace, Card Five Heart, Card Four Heart, Card Three Heart])
     c2 <- test_values_only [Ace, Jack, Jack, Ten, Five, Three, Two] 
     assertEqual "OneJokerPair" (get_ntuple_hand 2 4 5 c2) 
       (Just [Card Ace Heart, Joker Ace, Joker Ace, Joker Ace, Joker Ace])
     c3 <- test_values_only [Four, Four, Three, Three, Two, Two]
     assertEqual "NormalPair2" (get_ntuple_hand 2 1 5 c3) 
       (Just [Card Four Heart, Card Four Heart, Joker Ace, Card Three Heart, Card Three Heart])
     c4 <- test_values_only [Ace, Ace, Nine] 
     assertEqual "LowJokerPair" (get_ntuple_hand 2 2 5 c4) 
       (Just [Card Ace Heart, Card Ace Heart, Joker Ace, Joker Ace, Card Nine Heart])
     c5 <- test_values_only [Three, Ace, King, Six, Seven] 
     assertEqual "NoPair" (get_ntuple_hand 2 0 5 c5) Nothing
     c6 <- test_cards [Card King Diamond,Card Jack Heart,Card Ten Heart,Card Ten Diamond,Card Nine Spade]
     assertEqual "PairNormal2" (get_ntuple_hand 2 0 5 c6)
       (Just [Card Ten Heart, Card Ten Diamond, Card King Diamond, Card Jack Heart, Card Nine Spade]))

test_two_pair = TestCase ( 
  do c1 <- test_cards [Card Four Diamond, Card Four Club, Card Three Spade, Card Three Spade, Card Two Heart, Card Two Heart] 
     assertEqual "NormalTwoPair" (get_2_ntuple_hand 2 2 0 5 c1)
       (Just [Card Four Diamond, Card Four Club, Card Three Spade, Card Three Spade, Card Two Heart])
     assertEqual "JokerOnlyTwoPair" (get_2_ntuple_hand 2 2 5 5 c1)
       (Just [Joker Ace, Joker Ace, Joker Ace, Joker Ace, Joker Ace])
     assertEqual "JokerPairAndHighCardTwoPair" (get_2_ntuple_hand 2 2 3 5 c1)
       (Just [Joker Ace, Joker Ace, Card Four Diamond, Card Four Club, Joker Ace])
     c2 <- test_values_only [Ace, Ten, Three]
     assertEqual "JokerPairsTwoPair" (get_2_ntuple_hand 2 2 2 5 c2)
       (Just [Card Ace Heart, Joker Ace, Card Ten Heart, Joker Ten, Card Three Heart])
     assertEqual "NotEnoughCardsTwoPair" (get_2_ntuple_hand 2 2 1 5 c2) Nothing)

test_three_of_a_kind = TestCase ( 
  do c1 <- test_values_only [Five, Four, Three, Two, Two] 
     assertEqual "NoTriplet" (get_ntuple_hand 3 0 5 c1) Nothing
     assertEqual "OneJokerTriplet" (get_ntuple_hand 3 1 5 c1) 
       (Just [Card Two Heart, Card Two Heart, Joker Two, Card Five Heart, Card Four Heart])
     assertEqual "HighJokerTriplet" (get_ntuple_hand 3 4 5 c1) 
       (Just [Joker Ace, Joker Ace, Joker Ace, Joker Ace, Card Five Heart])
     c2 <- test_values_only [Ace, Ace, Jack] 
     assertEqual "LowJokerTriplet" (get_ntuple_hand 3 4 5 c2) 
       (Just [Card Ace Heart, Card Ace Heart, Joker Ace, Joker Ace, Joker Ace])
     c3 <- test_values_only [Three, Three, Three, Three]
     assertEqual "FourOfAKindTriplet" (get_ntuple_hand 3 1 5 c3) 
       (Just [Card Three Heart, Card Three Heart, Card Three Heart, Joker Ace, Card Three Heart]))

test_three_pair = TestCase ( 
  do c1 <- test_values_only [Four, Four, Three, Three, Two, Two]
     assertEqual "NormalThreePair" (get_3_ntuple_hand 2 2 2 0 6 c1)
       (Just [Card Four Heart, Card Four Heart, Card Three Heart, Card Three Heart, Card Two Heart, Card Two Heart])
     assertEqual "JokerOnlyThreePair" (get_3_ntuple_hand 2 2 2 6 6 c1)
       (Just [Joker Ace, Joker Ace, Joker Ace, Joker Ace, Joker Ace, Joker Ace])
     assertEqual "JokerHighPairThreePair" (get_3_ntuple_hand 2 2 2 3 6 c1)
       (Just [Joker Ace, Joker Ace, Card Four Heart, Card Four Heart, Card Three Heart, Card Three Heart])
     c2 <- test_values_only [Ace, Ten, Three]
     assertEqual "JokerPairsThreePair" (get_3_ntuple_hand 2 2 2 3 6 c2)
       (Just [Card Ace Heart, Joker Ace, Card Ten Heart, Joker Ten, Card Three Heart, Joker Three])
     assertEqual "NotEnoughCardsTwoPair" (get_3_ntuple_hand 2 2 2 1 6 c2) Nothing)

test_straight = TestCase ( 
  do c1 <- test_values_only [Ace, Ten, Five, Four, Four, Three, Two] 
     assertEqual "StraightNormal1" (get_straight 0 5 c1)
       (Just [Card Five Heart, Card Four Heart, Card Three Heart, Card Two Heart, Card Ace Heart])
     assertEqual "StraightInsertHighJoker" (get_straight 1 5 c1)
       (Just [Joker Six, Card Five Heart, Card Four Heart, Card Three Heart, Card Two Heart])
     c2 <- test_values_only [Four, Three, Ace, Ten, Four, Jack, Queen, Two, Five, Nine, Eight] 
     assertEqual "StraightBetterStraight" (get_straight 0 5 c2)
       (Just [Card Queen Heart, Card Jack Heart, Card Ten Heart, Card Nine Heart, Card Eight Heart])
     assertEqual "StraightInsertMiddleJoker" (get_straight 2 5 c2)
       (Just [Card Ace Heart, Joker King, Card Queen Heart, Card Jack Heart, Card Ten Heart])
     c3 <- test_values_only [Four, Eight, Five]
     assertEqual "StraightInsertTwoJokers" (get_straight 2 5 c3)
       (Just [Card Eight Heart, Joker Seven, Joker Six, Card Five Heart, Card Four Heart])
     assertEqual "StraightAllJokers" (get_straight 5 5 c3)
       (Just [Joker Ace, Joker King, Joker Queen, Joker Jack, Joker Ten])
     assertEqual "StraightNotEnoughCards" (get_straight 1 5 c3) Nothing)

test_flush = TestCase(
  do c1 <- test_cards [Card Two Heart, Card Three Heart, Card Four Heart, Card Five Heart, Card Six Heart]
     assertEqual "FlushNormal1" (get_flush 0 5 c1)
       (Just [Card Six Heart, Card Five Heart, Card Four Heart, Card Three Heart, Card Two Heart])
     c2 <- test_cards [Card Six Heart, Card Five Spade, Card Four Heart, Card Three Heart, Card Two Heart]
     assertEqual "FlushNone1" (get_flush 0 5 c2) Nothing
     assertEqual "FlushAllJokers" (get_flush 5 5 c2)
       (Just [Joker Ace, Joker Ace, Joker Ace, Joker Ace, Joker Ace])
     assertEqual "FlushNone2" (get_flush 5 6 []) Nothing
     assertEqual "FlushWithJokers" (get_flush 1 5 c2)
       (Just [Joker Ace, Card Six Heart, Card Four Heart, Card Three Heart, Card Two Heart])
     c3 <- test_cards [Card Three Spade, Card Two Heart, Card Four Spade, Card Five Spade, Card Four Club, Card Two Heart, Card Six Heart, Card King Diamond, Card Ten Club]
     c3 <- test_cards [Card King Diamond, Card Ten Club, Card Six Heart, Card Five Spade, Card Four Spade, Card Four Club, Card Three Spade, Card Two Heart, Card Two Heart]
     assertEqual "FlushComparison" (get_flush 0 3 c3)
       (Just [Card Six Heart, Card Two Heart, Card Two Heart]))

test_full_house = TestCase(
  do c1 <- test_cards [Card Five Diamond, Card Five Spade, Card Two Heart, Card Two Heart, Card Two Club]
     assertEqual "FullHouseNormal1" (get_2_ntuple_hand 3 2 0 5 c1) 
       (Just [Card Two Heart, Card Two Heart, Card Two Club, Card Five Diamond, Card Five Spade])
     assertEqual "FullHouseJoker1" (get_2_ntuple_hand 3 2 1 5 c1) 
       (Just [Card Five Diamond, Card Five Spade, Joker Five, Card Two Heart, Card Two Heart])
     c2 <- test_cards [Card Five Club, Card Two Heart, Card Two Diamond, Card Two Spade, Card Two Heart]
     assertEqual "FullHouseNone1" (get_2_ntuple_hand 3 2 0 5 c2) Nothing)

test_flush_five = TestCase(
  do c1 <- test_cards [Card Ace Heart, Card Ace Heart, Card Ace Heart, Card Ace Heart, Card Ace Heart]
     assertEqual "FlushFiveMatch1" (get_flush_ntuple 5 0 5 c1)
       (Just [Card Ace Heart, Card Ace Heart, Card Ace Heart, Card Ace Heart, Card Ace Heart])
     assertEqual "FlushFiveNoUseJoker" (get_flush_ntuple 5 5 5 c1)
       (Just [Card Ace Heart, Card Ace Heart, Card Ace Heart, Card Ace Heart, Card Ace Heart])
     c2 <- test_cards [Card Ace Heart, Card Ace Club, Card Ace Diamond, Card Ace Heart, Card Ace Heart]
     assertEqual "FlushFiveNone" (get_flush_ntuple 5 0 5 c2) Nothing
     assertEqual "FlushFiveUseJokers" (get_flush_ntuple 5 2 5 c2)
       (Just [Card Ace Heart, Card Ace Heart, Card Ace Heart, Joker Ace, Joker Ace]))

main = do
  runTestTT (test[
    test_all_ntuples,
    test_get_pair,
    test_two_pair,
    test_three_of_a_kind,
    test_three_pair,
    test_straight,
    test_flush,
    test_full_house,
    test_flush_five])

