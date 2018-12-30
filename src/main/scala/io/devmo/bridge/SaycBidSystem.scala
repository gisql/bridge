package io.devmo.bridge

import io.devmo.bridge.BidSuit.bidSuitOrder._
import io.devmo.bridge.BidSystem.Bidding

object SaycBidSystem extends BidSystem {
  private val HighRanks: Vector[Rank] = Vector(Jack, Queen, King, Ace)
  private val Balanced: Set[List[Int]] = Set(List(4, 4, 3, 2), List(4, 3, 3, 3), List(5, 3, 3, 2))

  def highCardPoints(hand: Vector[Card]): Int = hand.map(x => HighRanks.indexOf(x.rank) + 1).sum
  def shape(hand: Vector[Card]): Map[BidSuit, Int] = Map(Hearts -> 0, Spades -> 0, Diamonds -> 0, Clubs -> 0) ++ hand.groupBy(_.suit).mapValues(_.size)

  override def bid(hand: Vector[Card], bidding: Bidding): Bid = {
    val hcp = highCardPoints(hand)
    val noTrumpShaped = isNtShaped(hand)

    if (hcp >= 15 && hcp <= 18 && noTrumpShaped) ValueBid(1, NoTrump)
    else if (hcp >= 19 && hcp <= 21 && noTrumpShaped) ValueBid(2, NoTrump)
    else if (hcp >= 25 && hcp <= 27 && noTrumpShaped) ValueBid(3, NoTrump)
    else if (hcp >= 22 && noTrumpShaped) ValueBid(2, Clubs)
    else if (hcp >= 12 && hcp <= 21) openSuit(hand)
    else if (hcp >= 6 && hcp <= 11) weakHand(hand)
    else Pass
  }

  private def weakHand(hand: Vector[Card]): Bid = {
    val ls = longestSuit(hand)
    if (ls != Clubs && highCardPoints(hand.filter(_.suit == ls)) >= 6) ValueBid(2, ls)
    else Pass
  }

  private def major(suit: BidSuit) = suit > Diamonds

  private def isNtShaped(hand: Vector[Card]): Boolean = {
    val sh = shape(hand)
    val sc = sh.values.toList.sorted.reverse
    Balanced.contains(sc) && !sh.exists { case (s, v) => major(s) && v == 5 || v > 5 }
  }
  private def longestSuit(hand: Vector[Card]): BidSuit = shape(hand).toList.sortBy(x => (x._2, x._1)).reverse.head._1

  private def openSuit(hand: Vector[Card]): Bid = {
    val sh = shape(hand)
    if (sh.values.max >= 5) ValueBid(1, longestSuit(hand))
    else if (sh(Diamonds) <= 3 && sh(Clubs) >= 3) ValueBid(1, Clubs)
    else if (sh(Diamonds) >= 3 && sh(Diamonds) >= sh(Clubs)) ValueBid(1, Diamonds)
    else Pass
  }
}
