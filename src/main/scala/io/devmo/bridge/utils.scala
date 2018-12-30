package io.devmo.bridge

import io.devmo.bridge.BidSuit.bidSuitOrder._
import io.devmo.bridge.BidSystem.Bidding

import scala.util.Random

object Dealer {
  private def shuffle[T](rnd: Random, in: Vector[T], acc: Vector[T]): Vector[T] = {
    if (in.isEmpty) acc
    else {
      val p = rnd.nextInt(in.size)
      val rest = in.drop(p)
      shuffle(rnd, in.take(p) ++ rest.tail, rest.head +: acc)
    }
  }

  def deal(seed: Long = Random.nextLong(), deck: Vector[Card] = Deck.full): Map[Player, Vector[Card]] = {
    val rnd = new Random(seed)
    Player.all.zip(shuffle(rnd, deck, Vector()).sliding(deck.length / 4).toVector).toMap
  }
}

object Judge {
  private def lastValueBid(bidding: Bidding): Option[ValueBid] = bidding.bids.reverse.collectFirst({ case vb: ValueBid => vb })
  private def validN(n: Int): Boolean = n > 0 && n < 8

  def isValid(bidding: Bidding, newBid: Bid): Boolean =
    (lastValueBid(bidding), newBid) match {
      case (_, Pass) => true
      case (None, ValueBid(n, _)) if validN(n) => true
      case (Some(ValueBid(nl, sl)), ValueBid(n, s)) if validN(n) => nl < n || nl == n && sl < s
      case _ => false
    }
}

