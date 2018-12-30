package io.devmo.bridge

import org.scalatest.{FunSuite, Matchers}

class DealerTest extends FunSuite with Matchers {
  test("shuffle should return 52 cards in 4 groups of equal size") {
    Dealer.deal().values.foreach(hand => hand.size shouldBe 13)
  }

  test("shuffle should produce the same result for the same seed") {
    Dealer.deal(10) shouldBe Dealer.deal(10)
  }

  test("shuffle should return a different hand each time it's called") {
    Dealer.deal() should not be Dealer.deal()
  }
}
