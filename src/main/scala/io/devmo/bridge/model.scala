package io.devmo.bridge

import io.devmo.bridge.BidSystem.Bidding

sealed class BidSuit(val shortName: String) {
  override def toString: String = shortName
}
case object Clubs extends BidSuit("♣")
case object Diamonds extends BidSuit("♦")
case object Hearts extends BidSuit("♥")
case object Spades extends BidSuit("♠")
case object NoTrump extends BidSuit("NT")
object BidSuit {
  val bidSuits: Vector[BidSuit] = Deck.suits :+ NoTrump
  implicit val bidSuitOrder: Ordering[BidSuit] = (x: BidSuit, y: BidSuit) => bidSuits.indexOf(x) compare bidSuits.indexOf(y)
}

sealed class Rank(val shortName: String) {
  override def toString: String = shortName
}
case object Two extends Rank("2")
case object Three extends Rank("3")
case object Four extends Rank("4")
case object Five extends Rank("5")
case object Six extends Rank("6")
case object Seven extends Rank("7")
case object Eight extends Rank("8")
case object Nine extends Rank("9")
case object Ten extends Rank("T")
case object Jack extends Rank("J")
case object Queen extends Rank("Q")
case object King extends Rank("K")
case object Ace extends Rank("A")

object Rank {
  val ranks: Vector[Rank] = Vector(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
  implicit val rankOrder: Ordering[Rank] = (x: Rank, y: Rank) => ranks.indexOf(x) compare ranks.indexOf(y)
}

case class Card private(rank: Rank, suit: BidSuit) {
  override def toString: String = rank.toString + suit.toString
}

object Deck {

  import Rank.rankOrder._

  val suits: Vector[BidSuit] = Vector(Clubs, Diamonds, Hearts, Spades)

  val full: Vector[Card] = for {
    r <- Rank.ranks
    s <- suits
  } yield Card(r, s)

  val fromSevens: Vector[Card] = full.filter(c => c.rank >= Seven)
  val fromNines: Vector[Card] = fromSevens.filter(c => c.rank >= Nine)
}

sealed trait Player
case object North extends Player
case object East extends Player
case object West extends Player
case object South extends Player
object Player {
  val all: Vector[Player] = Vector(North, East, West, South)
}

sealed trait Bid
case object Pass extends Bid
case class ValueBid(n: Int, suit: BidSuit) extends Bid

object Bid {
  implicit final class IntBid(val self: Int) extends AnyVal {
    def suited(suit: BidSuit): Bid = ValueBid(self, suit)
    def ♣ : Bid = suited(Clubs)
    def ♦ : Bid = suited(Diamonds)
    def ♥ : Bid = suited(Hearts)
    def ♠ : Bid = suited(Spades)
    def NT: Bid = suited(NoTrump)
  }
}

object BidSystem {
  case class Bidding(dealer: Player, bids: Vector[Bid])
}

trait BidSystem {
  def bid(hand: Vector[Card], bidding: Bidding): Bid
}
