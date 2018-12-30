package io.devmo.bridge

import io.devmo.bridge.BidSuit.bidSuitOrder._
import io.devmo.bridge.BidSystem.Bidding
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

class JudgeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  private val bidGen: Gen[Bid] =
    for {
      s <- Gen.oneOf(BidSuit.bidSuits)
      n <- Gen.chooseNum(1, 7)
      rv <- Gen.oneOf(Pass, ValueBid(n, s))
    } yield rv
  private val biddingGen: Gen[Bidding] =
    for {
      bids <- Gen.listOf(bidGen)
      dealer <- Gen.oneOf(Player.all)
    } yield Bidding(dealer, bids.toVector)

  private def lastValueBid(bidding: Bidding): Option[ValueBid] = bidding.bids.reverse.collectFirst({ case vb: ValueBid => vb })

  test("bids outside 1 to 7 range are invalid") {
    val invalidBidGen = for {
      n <- Gen.chooseNum(-10, 20) suchThat (n => n < 1 || n > 7)
      s <- Gen.oneOf(BidSuit.bidSuits)
    } yield ValueBid(n, s)
    forAll(biddingGen, invalidBidGen) { (start, bid) =>
      Judge.isValid(start, bid) shouldBe false
    }
  }

  test("pass is always valid") {
    forAll(biddingGen) { start =>
      Judge.isValid(start, Pass) shouldBe true
    }
  }

  test("bid with higher value is valid") {
    forAll(biddingGen suchThat (b => lastValueBid(b).exists(_.n < 7))) { start =>
      lastValueBid(start) match {
        case Some(vb) => Judge.isValid(start, vb.copy(n = vb.n + 1)) shouldBe true
        case None => fail("shouldn't be here")
      }
    }
  }

  test("bid with lower value is invalid") {
    forAll(biddingGen suchThat (b => lastValueBid(b).exists(_.n > 1))) { start =>
      lastValueBid(start) match {
        case Some(vb) => Judge.isValid(start, vb.copy(n = vb.n - 1)) shouldBe false
        case None => fail("shouldn't be here")
      }
    }
  }

  test("bid with the same value but higher suit is valid") {
    forAll(biddingGen suchThat (b => lastValueBid(b).exists(_.suit < NoTrump)), Gen.oneOf(BidSuit.bidSuits)) { (start, next) =>
      whenever(lastValueBid(start).exists(_.suit < next)) {
        lastValueBid(start) match {
          case Some(vb) => Judge.isValid(start, vb.copy(suit = next)) shouldBe true
          case None => fail("shouldn't be here")
        }
      }
    }
  }

  test("bid with the same value but not higher suit is invalid") {
    forAll(biddingGen, Gen.oneOf(BidSuit.bidSuits)) { (start, next) =>
      whenever(lastValueBid(start).exists(_.suit >= next)) {
        lastValueBid(start) match {
          case Some(vb) => Judge.isValid(start, vb.copy(suit = next)) shouldBe false
          case None => fail("shouldn't be here")
        }
      }
    }
  }
}
