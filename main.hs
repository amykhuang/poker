import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import Options.Applicative
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')
import Text.Printf

import Poker

-- Flags --
data Flags = Flags
  { flag_num_decks   :: Int
  , flag_num_jokers  :: Int
  , flag_hand_size   :: Int
  , flag_cards_table :: Int
  , flag_cards_hand  :: Int
  , flag_num_runs    :: Int}
  deriving (Show)

print_flags :: Flags -> String
print_flags (Flags d j s t h r) = printf
    ("Number of runs:   %d\n"
  ++ "Number of decks:  %d\n"
  ++ "Number of jokers: %d\n"
  ++ "Hand size:        %d\n"
  ++ "Cards on table:   %d\n"
  ++ "Cards in hand:    %d\n\n") r d j s t h
    
flags :: Parser Flags
flags = Flags
     <$> option auto
         ( long "num_decks"
        <> short 'd'
        <> metavar "INT"
        <> help "total number of decks"
        <> value 1 )
     <*> option auto
         ( long "num_jokers"
        <> short 'j'
        <> metavar "INT"
        <> help "number of jokers"
        <> value 0 )
     <*> option auto
         ( long "hand_size"
        <> short 's'
        <> metavar "INT"
        <> help "size of poker hand"
        <> value 5 )
     <*> option auto
         ( long "cards_table"
        <> short 't'
        <> metavar "INT"
        <> help "number of cards on the table"
        <> value 5 )
     <*> option auto
         ( long "cards_hand"
        <> short 'h'
        <> metavar "INT"
        <> help "number of cards in your hand"
        <> value 2 )
     <*> option auto
         ( long "num_runs"
        <> short 'r'
        <> metavar "INT"
        <> help "number of runs for the simulation"
        <> value 1000000 )


-- Simulation --
all_hands :: Int -> [Card] -> [HandType]
all_hands handsize all_cards =
  -- map (\(a, b) -> (a, fromJust b)) $
  map fst
  $ filter (isJust . snd)
  $ map (\(t, fn) -> (t, fn njokers handsize cards)) all_fns
  where all_fns = [(HighCard,      get_high_card_hand),
                   (Pair,          get_ntuple_hand 2),
                   (TwoPair,       get_2_ntuple_hand 2 2), 
                   (ThreeofaKind,  get_ntuple_hand 3),
                   (Straight,      get_straight),
                   (Flush,         get_flush),
                   (FullHouse,     get_2_ntuple_hand 3 2),
                   (FourofaKind,   get_ntuple_hand 4),
                   (StraightFlush, get_flush_straight),
                   (FiveofaKind,   get_ntuple_hand 5),
                   (FlushHouse,    get_flush_2_ntuple 3 2),
                   (FlushFive,     get_flush_ntuple 5),
                   (FullHouse4_2,  get_2_ntuple_hand 4 2),
                   (TwoTriplet,    get_2_ntuple_hand 3 3),
                   (ThreePair,     get_3_ntuple_hand 2 2 2),
                   (SixofaKind,    get_ntuple_hand 6),
                   (FlushSix,      get_flush_ntuple 6)]
        njokers = count_jokers all_cards
        cards = sortBy (flip compare) $ remove_jokers all_cards

simulate_one_hand :: Int -> Int -> [Card] -> Int -> [HandType]
simulate_one_hand handsize ndraw decks seed = all_hands handsize $ take ndraw shuffled
  where shuffled = shuffle' decks (length decks) (mkStdGen seed)

simulate_n_hands :: Int -> Int -> Int -> [Card] -> [HandType]
simulate_n_hands nruns handsize ndraw decks =
  concatMap (simulate_one_hand handsize ndraw decks) (take nruns seeds)
  where seeds = [0..]

--simulate_n_hands_parallel :: Int -> Int -> Int -> [Card] -> [HandType]
--simulate_n_hands_parallel nruns handsize ndraw decks =
--  concat $ (parMap rseq (simulate_one_hand handsize ndraw decks) (take nruns seeds))
--  where seeds = [0..]

to_float :: Int -> Float
to_float i = fromIntegral i :: Float

print_histogram :: Int -> [HandType] -> String
print_histogram nruns = unlines
  . map (\(count, t) -> printf "%s: %.4f"
                        (show t) (100 * (to_float count) / (to_float nruns)))
  . reverse . sort . map (\ls -> (length ls, head ls)) . group . sort

main :: IO ()
main = do
  all_flags <- execParser (info (flags <**> helper) (fullDesc <> header "poker"))
  let Flags ndecks njokers hand_size ncards_in_hand ncards_on_table runs = all_flags
  putStr $ print_flags all_flags

  -- run
  let deck = [Card v s | v <- [Two ..], s <- [Club ..]]
  let decks = (concat(take ndecks (repeat deck))) ++ (take njokers $ repeat (Joker NoValue))
  let all_runs = simulate_n_hands runs hand_size (ncards_in_hand + ncards_on_table) decks
  putStr $ print_histogram runs $ all_runs
