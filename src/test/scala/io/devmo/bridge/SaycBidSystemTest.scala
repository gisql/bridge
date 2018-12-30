package io.devmo.bridge

import io.devmo.bridge.BidSystem.Bidding
import io.devmo.bridge.Rank.rankOrder._
import io.devmo.bridge.SaycBidSystem.{highCardPoints, shape}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

class SaycBidSystemTest extends WordSpec with GeneratorDrivenPropertyChecks with Matchers {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfig(minSuccessful = 100, maxDiscarded = 500000, workers = 10)

  private val highCardGen: Gen[Card] =
    for {
      r <- Gen.oneOf(Rank.ranks.filter(_ >= Jack))
      s <- Gen.oneOf(Clubs, Diamonds, Hearts, Spades)
    } yield Card(r, s)
  private val lowCardGen: Gen[Card] =
    for {
      r <- Gen.oneOf(Rank.ranks.filter(_ < Jack))
      s <- Gen.oneOf(Clubs, Diamonds, Hearts, Spades)
    } yield Card(r, s)
  private val lousyHandGen: Gen[Vector[Card]] = Gen.listOfN(20, lowCardGen).map(_.toSet.toVector.take(13))

  private def handHcp(low: Int, high: Int): Gen[Vector[Card]] = {
    val n = (low + (high - low) / 2.0) / 2.5
    val rv = for {
      highs <- Gen.listOfN(n.toInt, highCardGen).map(_.toSet.toVector)
      lows <- lousyHandGen
    } yield (highs ++ lows).take(13).sortBy(c => (c.suit, c.rank)).reverse
    rv suchThat (h => highCardPoints(h) >= low && highCardPoints(h) <= high)
  }

  "HCP" should {
    List((Ace, 4), (King, 3), (Queen, 2), (Jack, 1)) foreach { case (rank, hcp) =>
      s"count $rank as $hcp" in {
        forAll(Gen.oneOf(BidSuit.bidSuits), lousyHandGen) { (s, hand) =>
          SaycBidSystem.highCardPoints(hand.tail :+ Card(rank, s)) shouldBe hcp
        }
      }
    }
    "count non-honour cards as 0" in {
      forAll(lousyHandGen) { hand =>
        highCardPoints(hand) shouldBe 0
      }
    }
  }

  "Opening bidder" should {
    val bidding = Bidding(South, Vector())
    "pass with HCP < 5" in {
      forAll(handHcp(0, 5)) { hand =>
        SaycBidSystem.bid(hand, bidding) shouldBe Pass
      }
    }
    "open with 1 highest 5+ suit for shape 5xxx and HCP between 12 and 21 when not suitable for NT" in {
      forAll(handHcp(12, 21) suchThat (h => shape(h).values.max >= 5)) { hand =>
        val longest = shape(hand).toList.sortBy(x => (x._2, x._1)).reverse.head._1
        val hcp = highCardPoints(hand)
        val ValueBid(_, got) = SaycBidSystem.bid(hand, bidding)
        if (hcp < 15) got should not be NoTrump
        if (got != NoTrump) got shouldBe longest
      }
    }
    List((15, 18, 1), (19, 21, 2), (25, 27, 3)) foreach { case (low, high, exp) =>
      s"open with ${exp}NT for HCP between $low and $high shape 4333 4432 5332 (no major 5)" in {
        val ntShapes = Set("4333", "4432", "5332")
        forAll(handHcp(low, high) suchThat (h => ntShapes.contains(shape(h).values.mkString))) { hand =>
          val s = shape(hand)
          val noMajor5 = s(Spades) < 5 && s(Hearts) < 5
          whenever(noMajor5) {
            val got = SaycBidSystem.bid(hand, bidding)
            got shouldBe ValueBid(exp, NoTrump)
          }
        }
      }
    }
    "open with artificial 2 Clubs for HCP 22+ (excluding 25-27 range) and shape 4333 4432 5332 (no major 5)" in {
      val ntShapes = Set("4333", "4432", "5332")
      forAll(handHcp(22, 40) suchThat (h => ntShapes.contains(shape(h).values.mkString))) { hand =>
        val s = shape(hand)
        val noMajor5 = s(Spades) < 5 && s(Hearts) < 5
        val hcp = highCardPoints(hand)
        whenever(noMajor5 && !(hcp >= 25 && hcp <= 27)) {
          val got = SaycBidSystem.bid(hand, bidding)
          got shouldBe ValueBid(2, Clubs)
        }
      }
    }

    "open 1 Club with 3- Diamonds, 3+ Clubs and no 5+ major and HCP 12 to 21 and wrong shape for NT" in {
      def shapeFor1C(hand: Vector[Card]): Boolean = {
        val s = shape(hand)
        s(Diamonds) <= 3 && s(Clubs) >= 3 && s(Hearts) < 5 && s(Spades) < 5
      }

      forAll(handHcp(12, 21) suchThat shapeFor1C) { hand =>
        val s = shape(hand)
        whenever(s(Spades) < 5 && s(Hearts) < 5) {
          val hcp = highCardPoints(hand)
          val ValueBid(_, got) = SaycBidSystem.bid(hand, bidding)
          if (hcp < 15) got should not be NoTrump
          if (got != NoTrump) got shouldBe Clubs
        }
      }
    }

    "open 1 Diamond when no major, minors (DC) 44 or diamonds (3+) > clubs and HCP 12 to 21 and wrong shape for NT" in {
      def shapeFor1D(hand: Vector[Card]): Boolean = {
        val s = shape(hand)
        (s(Diamonds) >= 4 && s(Diamonds) >= s(Clubs) || s(Diamonds) >= 3 && s(Clubs) < 3) && s(Hearts) < 5 && s(Spades) < 5
      }

      forAll(handHcp(12, 21) suchThat shapeFor1D) { hand =>
        val s = shape(hand)
        whenever(s(Spades) < 5 && s(Hearts) < 5) {
          val hcp = highCardPoints(hand)
          val ValueBid(_, got) = SaycBidSystem.bid(hand, bidding)
          if (hcp < 15) got should not be NoTrump
          if (got != NoTrump) got shouldBe Diamonds
        }
      }
    }

    "open with weak 2 for HCP 6 to 11, 6+ cards in suit and 6+ points concentrated in it" in {
      forAll(handHcp(6, 11) suchThat(h => shape(h).values.max >= 6)) { hand =>
        val s = shape(hand)
        val longest = s.toList.sortBy(x => (x._2, x._1)).reverse.head._1
        whenever(longest != Clubs && highCardPoints(hand.filter(_.suit == longest)) >= 6) {
          SaycBidSystem.bid(hand, bidding) shouldBe ValueBid(2, longest)
        }
      }
    }
  }

  "Responder to 1 Clubs" should {
    val bidding = Bidding(South, Vector(ValueBid(1, Clubs), Pass))
    "pass with HCP 0-5" in {
      forAll(handHcp(0, 5)) { hand =>
        SaycBidSystem.bid(hand, bidding) shouldBe Pass
      }
    }
    "respond with 1 ♦/♥/♠ with HCP 6-9, 4+ in suit, no longer or equal lower suit" in {

    }
  }
}
