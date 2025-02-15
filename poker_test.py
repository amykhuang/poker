from poker import *
import unittest

class TestPoker(unittest.TestCase):
  def test_helpers(self):
    self.assertEqual(ntuple_value([3, 3]), 3)
    self.assertEqual(ntuple_value([JOKER]), ACE)
    self.assertEqual(ntuple_value([5,JOKER]), 5)
    self.assertEqual(straight_hi_card([ACE,KING,QUEEN,JACK,10]), ACE)
    self.assertEqual(straight_hi_card([JOKER,4,JOKER,JOKER,ACE]), 5)
    self.assertEqual(straight_hi_card([JOKER,5,JOKER,JOKER,JOKER,ACE]), 6)
    self.assertEqual(straight_hi_card([JOKER,JOKER,4,JOKER,2,JOKER]), 6)
    self.assertEqual(straight_hi_card([JOKER,JOKER,4,JOKER,JOKER]), 6)
    self.assertEqual(straight_hi_card([JOKER,JOKER,JOKER,JOKER,JOKER]), ACE)
    self.assertEqual(better_hicard([2,3,4,5,6], [2,3,4,5,6]), [2,3,4,5,6])
    self.assertEqual(better_hicard([3,3,4,5,6], [2,3,4,5,6]), [3,3,4,5,6])
    self.assertEqual(better_hicard([2,3,4,5,ACE], [2,3,4,5,6]), [2,3,4,5,ACE])
    self.assertEqual(better_hicard([2,3,4,JOKER,6], [2,3,4,5,6]), [2,3,4,JOKER,6])
    self.assertEqual(better_hicard([JOKER,3,4,5,6], [ACE,3,4,5,6]), [ACE,3,4,5,6])
    self.assertEqual(better_hicard([], []), [])
    self.assertEqual(better_hicard([2,3], []), [2,3])
    self.assertEqual(better_straight([5,4,3,2], [JOKER,4,3,2]), [5,4,3,2])

  def test_pair(self):
    self.assertEqual(get_ntuple(2, [2,2,3,3,4,4]), [4, 4])
    self.assertEqual(get_ntuple(2, [JACK,JACK,ACE], num_jokers=1), [ACE,JOKER])
    self.assertEqual(get_ntuple(2, [4,3], num_jokers=4), [JOKER,JOKER])
    self.assertEqual(get_ntuple(2, [ACE,3], num_jokers=4), [ACE,JOKER])
    self.assertEqual(get_ntuple(2, [2,3,4,5,6]), [])
    self.assertEqual(get_ntuple(2, [2,5,7,8,3,2]), [2, 2])
    self.assertEqual(get_ntuple(2, [4,3,2,14,1,0]), [])

  def test_three_of_a_kind(self):
    self.assertEqual(get_ntuple(3, [2,2,3,3,4,4]), [])
    self.assertEqual(get_ntuple(3, [JACK,JACK,ACE], num_jokers=1), [JACK,JACK,JOKER])
    self.assertEqual(get_ntuple(3, [4,4,4,3], num_jokers=4), [JOKER,JOKER,JOKER])
    self.assertEqual(get_ntuple(3, [2,2,2,2]), [2,2,2])
    self.assertEqual(get_ntuple(3, [2,5,3,5,5]), [5,5,5])

  def test_four_of_a_kind(self):
    self.assertEqual(get_ntuple(4, [2,2,2,3,4,4]), [])
    self.assertEqual(get_ntuple(4, [JACK,4,JACK,JACK], num_jokers=1), [JACK,JACK,JACK,JOKER])
    self.assertEqual(get_ntuple(4, [4,3], num_jokers=5), [JOKER,JOKER,JOKER,JOKER])
    self.assertEqual(get_ntuple(4, [4,4,4,4,3], num_jokers=3), [4,4,4,4])

  def test_two_pair(self):
    self.assertEqual(get_2_ntuple(2, 2, [2,2,3,3,4,4], num_jokers=0, hand_size=5), [4,4,3,3])
    self.assertEqual(get_2_ntuple(2, 2, [JACK,JACK,ACE], num_jokers=1, hand_size=5), [ACE,JOKER,JACK,JACK])
    self.assertEqual(get_2_ntuple(2, 2, [4,3], num_jokers=4, hand_size=5), [JOKER,JOKER,JOKER,JOKER])
    self.assertEqual(get_2_ntuple(2, 2, [2,3,4,5,6], num_jokers=0, hand_size=5), [])
    self.assertEqual(get_2_ntuple(2, 2, [2,2,3,3,3,3], num_jokers=0, hand_size=5), [3,3,3,3])

  def test_two_triplet(self):
    self.assertEqual(get_2_ntuple(3, 3, [2,2,3,3,2,4,4,4], num_jokers=0, hand_size=6), [4,4,4,2,2,2])
    self.assertEqual(get_2_ntuple(3, 3, [JACK,JACK,ACE], num_jokers=1, hand_size=6), [])
    self.assertEqual(get_2_ntuple(3, 3, [4,3], num_jokers=4, hand_size=6), [4,JOKER,JOKER,3,JOKER,JOKER])
    self.assertEqual(get_2_ntuple(3, 3, [2,2,3,3,3], num_jokers=3, hand_size=6), [JOKER,JOKER,JOKER,3,3,3])

  def test_three_pair(self):
    self.assertEqual(get_3_ntuple(2, 2, 2, [2,2,3,3,4,4], num_jokers=0, hand_size=6), [4,4,3,3,2,2])
    self.assertEqual(get_3_ntuple(2, 2, 2, [2,2,3,3,4,4], num_jokers=4, hand_size=6), [JOKER,JOKER,JOKER,JOKER,4,4])

  def test_straight(self):
    self.assertEqual(get_straight([2,4,3,6,5,ACE,KING], hand_size=5), [6,5,4,3,2])
    self.assertEqual(get_straight([2,3,7,5,6,ACE,KING,QUEEN], hand_size=5), [])
    self.assertEqual(get_straight([2,4,5,ACE,QUEEN,JACK], num_jokers=1, hand_size=5), [5,4,JOKER,2,ACE])
    self.assertEqual(get_straight([2,4,ACE,10,JACK], num_jokers=2, hand_size=5), [ACE,JOKER,JOKER,JACK,10])
    self.assertEqual(get_straight([2,3,4,5,6,7,8,9,10,JACK,QUEEN,KING,ACE], hand_size=5), [ACE,KING,QUEEN,JACK,10])
    self.assertEqual(get_straight([], num_jokers=5, hand_size=5), [JOKER,JOKER,JOKER,JOKER,JOKER])
    self.assertEqual(get_straight([2,4,3,6], hand_size=5), [])

  def test_flush(self):
    hand = [(2, 'heart'), (3, 'heart'), (4, 'heart'), (5, 'heart'), (6, 'heart')]
    self.assertEqual(get_flush(5, hand), [6, 5, 4, 3, 2])
    hand = [(2, 'heart'), (3, 'spade'), (4, 'heart'), (5, 'heart'), (6, 'heart')]
    self.assertEqual(get_flush(5, hand), [])
    self.assertEqual(get_flush(5, [], num_jokers=5), [JOKER,JOKER,JOKER,JOKER,JOKER])
    self.assertEqual(get_flush(6, [], num_jokers=5), [])
    self.assertEqual(get_flush(6, [(3, 'spade')], num_jokers=5), [JOKER,JOKER,JOKER,JOKER,JOKER,3])
    hand = [(3,'spade'), (3,'spade'), (4, 'heart'), (4, 'spade'), (4, 'club'), (5, 'heart'), (6, 'heart')]
    self.assertEqual(get_flush(3, hand), [6,5,4])

  def test_full_house(self):
    self.assertEqual(get_2_ntuple(3, 2, [2,5,5,2,2], num_jokers=0, hand_size=5), [2,2,2,5,5])
    self.assertEqual(get_2_ntuple(3, 2, [2,2,2,2,5], num_jokers=0, hand_size=5), [])
    self.assertEqual(get_2_ntuple(3, 2, [2,2,2,5,5], num_jokers=1, hand_size=5), [5,5,JOKER,2,2])
    self.assertEqual(get_2_ntuple(3, 2, [2,2,2,2,2,5,5], num_jokers=1, hand_size=5), [5,5,JOKER,2,2])
    self.assertEqual(get_2_ntuple(3, 2, [], num_jokers=8, hand_size=5), [JOKER,JOKER,JOKER,JOKER,JOKER])
    self.assertEqual(get_2_ntuple(3, 2, [4], num_jokers=4, hand_size=5), [JOKER,JOKER,JOKER,4,JOKER])
    self.assertEqual(get_2_ntuple(3, 2, [ACE,ACE,6,6], num_jokers=1, hand_size=5), [ACE,ACE,JOKER,6,6])
    self.assertEqual(get_2_ntuple(3, 2, [ACE,ACE,6,2,3], num_jokers=2, hand_size=5), [ACE,ACE,JOKER,6,JOKER])

  def test_straight_flush(self):
    hand = [(2, 'heart'), (3, 'heart'), (4, 'heart'), (5, 'heart'), (6, 'heart'), (ACE, 'heart')]
    self.assertEqual(get_straight_flush(hand, num_jokers=0, hand_size=5), [6,5,4,3,2])
    hand = [(2, 'diamond'), (3, 'heart'), (4, 'heart'), (5, 'heart'), (6, 'heart'), (ACE, 'heart')]
    self.assertEqual(get_straight_flush(hand, num_jokers=0, hand_size=5), [])
    hand = [(2, 'heart'), (3, 'heart'), (5, 'heart'), (6, 'heart'), (ACE, 'heart')]
    self.assertEqual(get_straight_flush(hand, num_jokers=1, hand_size=5), [6,5,JOKER,3,2]) # ambiguous joker placement

  def test_royal_flush(self):
    hand = [(2, 'heart'), (3, 'heart'), (4, 'heart'), (5, 'heart'), (6, 'heart'), (ACE, 'heart')]
    self.assertEqual(get_royal_flush(hand, num_jokers=0, hand_size=5), [])
    hand = [(ACE, 'heart'), (KING, 'heart'), (QUEEN, 'heart'), (JACK, 'heart'), (10, 'heart'), (ACE, 'diamond')]
    self.assertEqual(get_royal_flush(hand, num_jokers=0, hand_size=5), [ACE,KING,QUEEN,JACK,10])
    # self.assertEqual(get_royal_flush(hand, num_jokers=5, hand_size=5), [ACE,KING,QUEEN,JACK,10])
    self.assertEqual(get_royal_flush([], num_jokers=5, hand_size=5), [JOKER,JOKER,JOKER,JOKER,JOKER])

  def test_flush_five(self):
    hand = [(5, 'heart'), (5, 'heart'), (5, 'heart'), (5, 'heart'), (5, 'heart')]
    self.assertEqual(get_nflush(5, hand), [5, 5, 5, 5, 5])
    hand += [(6, 'spade'), (6, 'spade'), (6, 'spade'), (6, 'spade'), (6, 'spade')]
    self.assertEqual(get_nflush(5, hand), [6,6,6,6,6])
    self.assertEqual(get_nflush(5, [], num_jokers=5), [JOKER,JOKER,JOKER,JOKER,JOKER])
    self.assertEqual(get_nflush(6, [], num_jokers=5), [])
    self.assertEqual(get_nflush(6, [(3, 'spade')], num_jokers=5), [3,JOKER,JOKER,JOKER,JOKER,JOKER])

if __name__ == "__main__":
  unittest.main()


