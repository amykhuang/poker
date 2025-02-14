from collections import defaultdict
import enum
import multiprocessing
import random

JOKER = 100
JACK = 11
QUEEN = 12
KING = 13
ACE = 14

ORDERING = [2, 3, 4, 5, 6, 7, 8, 9, 10, JACK, QUEEN, KING, ACE]
SUITS = ['heart', 'spade', 'club', 'diamond']
DECK = []
for n in ORDERING:
  for suit in SUITS:
    DECK.append((n, suit))

class Hand(enum.Enum):
  """Hand types."""
  HIGH_CARD = 1
  PAIR = 2
  TWO_PAIR = 3
  THREE_OF_A_KIND = 4
  STRAIGHT = 5
  FLUSH = 6
  FULL_HOUSE = 7
  FOUR_OF_A_KIND = 8
  STRAIGHT_FLUSH = 9
  FIVE_OF_A_KIND = 10
  FLUSH_HOUSE = 11
  FLUSH_FIVE = 12
  FULL_HOUSE_4_2 = 13
  FULL_HOUSE_3_3 = 14
  THREE_PAIR = 15
  SIX_OF_A_KIND = 16
  FLUSH_SIX = 17
  ROYAL_FLUSH = 18

  def __str__(self) -> str:
    if self == Hand.HIGH_CARD:
      return 'High Card'
    elif self == Hand.PAIR:
      return 'Pair'
    elif self == Hand.TWO_PAIR:
      return 'Two Pair'
    elif self == Hand.THREE_OF_A_KIND:
      return 'Three of a Kind'
    elif self == Hand.STRAIGHT:
      return 'Straight'
    elif self == Hand.FLUSH:
      return 'Flush'
    elif self == Hand.FULL_HOUSE:
      return 'Full House'
    elif self == Hand.FOUR_OF_A_KIND:
      return 'Four of a Kind'
    elif self == Hand.STRAIGHT_FLUSH:
      return 'Straight Flush'
    elif self == Hand.ROYAL_FLUSH:
      return 'Royal Flush'
    elif self == Hand.FIVE_OF_A_KIND:
      return 'Five of a Kind'
    elif self == Hand.FLUSH_HOUSE:
      return 'Flush House'
    elif self == Hand.FLUSH_FIVE:
      return 'Flush Five'
    elif self == Hand.FULL_HOUSE_4_2:
      return 'Full mansion'
    elif self == Hand.FULL_HOUSE_3_3:
      return 'Two triplet'
    elif self == Hand.THREE_PAIR:
      return 'Three pair'
    elif self == Hand.SIX_OF_A_KIND:
      return 'Six of a kind'
    elif self == Hand.FLUSH_SIX:
      return 'Flush six'
    else:
      return 'Unknown Hand'


### HELPER FUNCTIONS ###
def valid(hand):
  return hand != []

def sort_values_hi_to_lo(hand_values):
  return sorted(hand_values, reverse=True)

def filter_by_suit(hand, suit):
  return [v for (v, s) in hand if s == suit]

### RANDOM DEAL ###
def deal(deck, num_cards_in_hand, num_cards_on_table):
  random.shuffle(deck)
  table = deck[0:num_cards_on_table]
  hand = deck[num_cards_on_table:num_cards_on_table+num_cards_in_hand]
  return (table, hand)

### HAND MATCHING HELPERS ###
def ntuple_value(ntuple):
  if len(ntuple) == 0:
    return 0
  return ACE if all([c == JOKER for c in ntuple]) else [c for c in ntuple if c != JOKER][0]

def get_ntuple(n, hand_values, num_jokers=0, hand_size=5):
  '''Return best ntuple in this hand. (n=2 for pair, n=3 for 3 of a kind, etc.
     Return [] if there is none.
  '''
  best_hand = []
  hi_to_lo = sort_values_hi_to_lo(list(set(hand_values)))
  if num_jokers >= n and (len(hi_to_lo) == 0 or hi_to_lo[0] != ACE):
    return [JOKER] * n

  for value in hi_to_lo:
    match = hand_values.count(value)
    if match >= n:
      return [value] * n
    jokers_needed = n - match
    if num_jokers >= jokers_needed:
      return [value] * match + [JOKER] * jokers_needed
      break
  return []

def get_all_ntuples(n, hand_values, num_jokers):
  all_hands = []

  for value in sort_values_hi_to_lo(list(set(hand_values))):
    match = hand_values.count(value)
    if match >= n:
      all_hands.append([value] * n)
    elif match > 0:
      jokers_needed = n - match
      if num_jokers >= jokers_needed:
        all_hands.append([value]*match + [JOKER]*jokers_needed)

  if num_jokers >= n:
    all_hands.append([JOKER]*n)

  all_hands.sort(reverse=True, key=lambda x: ntuple_value(x))

  return all_hands

def straight_hi_card(hand_values):
  if hand_values == []:
    return 0
  if all([c == JOKER for c in hand_values]):
    return ACE
  if hand_values[-1] == ACE:
    hand_values[-1] = 1
  for i in range(len(hand_values)):
    v = hand_values[i]
    if v == JOKER: continue
    return v + i
  raise Exception('straight hi card failed')

def match_straight(hand_values, num_jokers, straight_to_match):
  straight = []
  for v in straight_to_match:
    if v in hand_values:
      straight.append(v)
      hand_values[hand_values.index(v)] = 0
    elif num_jokers > 0:
      straight.append(JOKER)
      num_jokers -= 1
    else:
      return []
  return straight

def get_straight(k, hand_values, num_jokers=0):
  possible_straights = list(reversed(
    [[ACE] + ORDERING[:k-1]] + [ORDERING[i:i+k] for i in range(len(ORDERING)-k+1)]))
  for match in possible_straights:
    straight = match_straight([x for x in hand_values], num_jokers, match)
    if valid(straight):
      return list(reversed(straight))
  return []

def better_hand(flush1, flush2):
  if flush1 == []:
    return False
  if flush2 == []:
    return True

  if len(flush1) != len(flush2):
    raise Exception('different length flush')

  for i in range(len(flush1)):
    # joker tiebreaking
    if flush1[i] == JOKER:
      if flush2[i] == ACE:
        return False
      if flush2[i] == JOKER:
        continue
      else:
        return True
    if flush2[i] == JOKER:
      if flush1[i] == ACE:
        return True
      else:
        return False

    if flush1[i] < flush2[i]:
      return False
    if flush1[i] > flush2[i]:
      return True
  return False

def get_flush(hand_size, hand, num_jokers=0):
  best_flush = []

  for suit in SUITS:
    match = filter_by_suit(hand, suit)
    if len(match) + num_jokers < hand_size:
      continue
    match.sort(reverse=True)
    # insert jokers
    i = match.count(ACE)
    match = match[:i] + [JOKER] * num_jokers + match[i:]
    match = match[:hand_size]

    if better_hand(match, best_flush):
      best_flush = match

  return best_flush

def get_2_ntuple(n1, n2, hand_values, num_jokers, hand_size=5):
  if hand_size < n1 + n2:
    return []
  all_triplets = get_all_ntuples(n1, hand_values, num_jokers)
  if len(all_triplets) == 0:
    return []

  best_full_house_3 = []
  best_full_house_2 = []

  for triple in all_triplets:
    hi_card = ntuple_value(triple)
    if hi_card <= ntuple_value(best_full_house_3):
      continue

    remaining_jokers = num_jokers - triple.count(JOKER)
    remaining_hand = [x for x in hand_values]
    for v in [x for x in triple if x != JOKER]:
      remaining_hand.remove(v)
    all_pairs = get_all_ntuples(n2, remaining_hand, remaining_jokers)
    if len(all_pairs) == 0:
      continue

    hi_pair = []
    for pair in all_pairs:
      if ntuple_value(pair) > ntuple_value(hi_pair):
        hi_pair = pair

    best_full_house_3 = triple
    best_full_house_2 = hi_pair

  return best_full_house_3 + best_full_house_2

def get_3_ntuple(n1, n2, n3, hand_values, num_jokers, hand_size):
  if hand_size < n1 + n2 + n3:
    return []

  all_n1 = get_all_ntuples(n1, hand_values, num_jokers)
  if len(all_n1) == 0:
    return []

  best1 = []
  best2 = []
  best3 = []

  for group1 in all_n1:
    hi_card = ntuple_value(group1)
    if hi_card <= ntuple_value(best1):
      continue

    best1 = group1
    remaining_jokers = num_jokers - group1.count(JOKER)
    remaining_hand = [x for x in hand_values]
    for v in [x for x in group1 if x != JOKER]:
      remaining_hand.remove(v)

    all_n2 = get_all_ntuples(n2, remaining_hand, remaining_jokers)
    if len(all_n2) == 0:
      continue

    best2 = []
    for group2 in all_n2:
      # if ntuple_value(group2) > ntuple_value(best2):
      best2 = group2

      remaining_jokers_2 = remaining_jokers - best2.count(JOKER)
      remaining_hand_2 = [x for x in remaining_hand]
      for v in [x for x in group2 if x != JOKER]:
        remaining_hand_2.remove(v)

      all_n3 = get_all_ntuples(n3, remaining_hand_2, remaining_jokers_2)
      if len(all_n3) == 0:
        continue

      best3 = []
      for group3 in all_n3:
        if ntuple_value(group3) > ntuple_value(best3):
          return best1 + best2 + group3

  return []

def get_straight_flush(k, hand, num_jokers=0):
  best_straight = []
  for suit in SUITS:
    filtered_hand = filter_by_suit(hand, suit)
    next_straight = get_straight(k, filtered_hand, num_jokers)
    if valid(next_straight) and (
        straight_hi_card(next_straight) > straight_hi_card(best_straight)):
      best_straight = next_straight
  return best_straight

def get_royal_flush(k, hand, num_jokers=0):
  best_straight_flush = get_straight_flush(k, hand, num_jokers)
  if straight_hi_card(best_straight_flush) == ACE:
    return best_straight_flush
  return []

def get_flush_house(hand, num_jokers=0):
  for suit in SUITS:
    filtered_hand = filter_by_suit(hand, suit)
    next_flush_house = get_2_ntuple(3, 2, filtered_hand, num_jokers, hand_size=5)
    if valid(next_flush_house):
      # TODO get best flush house
      return next_flush_house
  return []

def get_nflush(hand_size, hand, num_jokers=0):
  best_nflush = []
  for suit in SUITS:
    filtered_hand = filter_by_suit(hand, suit)
    next_nflush = get_ntuple(hand_size, filtered_hand, num_jokers, hand_size)
    if valid(next_nflush) and ntuple_value(next_nflush) > ntuple_value(best_nflush):
      best_nflush = next_nflush
  return best_nflush

### RUN SIMULATION ###
def all_hands(hand, hand_size):
  hands = []
  hand_values = [v for (v, _) in hand if v != JOKER]
  num_jokers = sum([1 for (v, _) in hand if v == JOKER] )

  hands.append(Hand.HIGH_CARD)
  if valid(get_ntuple(2, hand_values, num_jokers, hand_size)):
    hands.append(Hand.PAIR)
  if valid(get_2_ntuple(2, 2, hand_values, num_jokers, hand_size)):
    hands.append(Hand.TWO_PAIR)
  if valid(get_ntuple(3, hand_values, num_jokers, hand_size)):
    hands.append(Hand.THREE_OF_A_KIND)
  if valid(get_straight(hand_size, hand_values, num_jokers)):
    hands.append(Hand.STRAIGHT)
  if valid(get_2_ntuple(3, 2, hand_values, num_jokers, hand_size)):
    hands.append(Hand.FULL_HOUSE)
  if valid(get_flush(hand_size, hand, num_jokers)):
    hands.append(Hand.FLUSH)
  if valid(get_ntuple(4, hand_values, num_jokers, hand_size)):
    hands.append(Hand.FOUR_OF_A_KIND)
  if valid(get_straight_flush(hand_size, hand, num_jokers)):
    hands.append(Hand.STRAIGHT_FLUSH)
#  if valid(get_royal_flush(hand_size, hand, num_jokers)):
#    hands.append(Hand.ROYAL_FLUSH)
  if valid(get_flush_house(hand, num_jokers)):
    hands.append(Hand.FLUSH_HOUSE)
  if valid(get_nflush(5, hand, num_jokers)):
    hands.append(Hand.FLUSH_FIVE)
  if valid(get_2_ntuple(4, 2, hand_values, num_jokers, hand_size)):
    hands.append(Hand.FULL_HOUSE_4_2)
  if valid(get_2_ntuple(3, 3, hand_values, num_jokers, hand_size)):
    hands.append(Hand.FULL_HOUSE_3_3)
  if valid(get_ntuple(6, hand_values, num_jokers, hand_size)):
    hands.append(Hand.SIX_OF_A_KIND)
  if valid(get_nflush(6, hand, num_jokers)):
    hands.append(Hand.FLUSH_SIX)
  if valid(get_nflush(5, hand, num_jokers)):
    hands.append(Hand.FLUSH_FIVE)
  if valid(get_ntuple(5, hand_values, num_jokers, hand_size)):
    hands.append(Hand.FIVE_OF_A_KIND)
  if valid(get_3_ntuple(2, 2, 2, hand_values, num_jokers, hand_size)):
    hands.append(Hand.THREE_PAIR)

  # get best hand
  hand_ordering_6 = [
      Hand.HIGH_CARD, Hand.PAIR, Hand.TWO_PAIR, Hand.THREE_PAIR, Hand.THREE_OF_A_KIND,
      Hand.FULL_HOUSE, Hand.STRAIGHT, Hand.FLUSH, Hand.FOUR_OF_A_KIND, Hand.FULL_HOUSE_3_3,
      Hand.FULL_HOUSE_4_2, Hand.FIVE_OF_A_KIND, Hand.FLUSH_HOUSE, Hand.STRAIGHT_FLUSH,
      Hand.FLUSH_FIVE, Hand.SIX_OF_A_KIND, Hand.FLUSH_SIX
  ]
  hands.sort(reverse=True, key=lambda x: hand_ordering_6.index(x))
  return hands, hands[0]

def compute_all_thread(
    sample_size, num_decks, num_cards_in_hand, num_cards_on_table, num_jokers, hand_size):
  histogram = {}
  decks = (DECK * num_decks) + ([(JOKER, '')] * num_jokers)
  for _ in range(sample_size):
    random.shuffle(decks)
    dealt_cards = decks[0:num_cards_on_table+num_cards_in_hand]
    (hands, best_hand) = all_hands(dealt_cards, hand_size)
    if best_hand not in histogram: histogram[best_hand] = [0,0]
    histogram[best_hand][1] += 1
    for hand in hands:
      if hand not in histogram: histogram[hand] = [0,0]
      histogram[hand][0] += 1
  return histogram

def compute_all(
    sample_size, num_decks, num_cards_in_hand, num_cards_on_table, num_jokers, hand_size):
  threads = 16
  histograms = []
  with multiprocessing.Pool(threads) as pool:
    for _ in range(threads):
      histograms.append(
          pool.apply_async(
              compute_all_thread,
              (
                  sample_size // threads,
                  num_decks,
                  num_cards_in_hand,
                  num_cards_on_table,
                  num_jokers,
                  hand_size,
              ),
          )
      )
    pool.close()
    pool.join()
  global_histogram = {}
  for histogram in histograms:
    for hand, count in histogram.get().items():
      if hand not in global_histogram: global_histogram[hand] = [0,0]
      global_histogram[hand][0] += count[0]
      global_histogram[hand][1] += count[1]

  final_histogram = {}
  for hnd in global_histogram:
    final_histogram[hnd] = [global_histogram[hnd][0] / sample_size, global_histogram[hnd][1] / sample_size]
  return final_histogram

def print_histogram(histogram):
  for hnd in sorted(list(histogram.items()), reverse=True, key=lambda x: x[1]):
    print("{}{:.5%}\t{:.5%}".format(str(hnd[0]).ljust(20), hnd[1][0], hnd[1][1]))

if __name__ == "__main__":
  sample_size = 1000000
  num_decks = 5
  num_cards_in_hand = 6
  num_cards_on_table = 6
  num_jokers = 10
  hand_size = 6

  print("Number of runs: {}".format(sample_size))
  print("Decks:          {}".format(num_decks))
  print("Cards in hand:  {}".format(num_cards_in_hand))
  print("Cards on table: {}".format(num_cards_on_table))
  print("Jokers:         {}".format(num_jokers))
  print("Hand size:      {}".format(hand_size))
  print("")
  print_histogram(
    compute_all(sample_size, num_decks, num_cards_in_hand, num_cards_on_table, num_jokers, hand_size))


