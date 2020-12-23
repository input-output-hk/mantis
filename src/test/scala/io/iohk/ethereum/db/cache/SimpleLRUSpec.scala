package io.iohk.ethereum.db.cache

import org.scalatest.wordspec.AnyWordSpec

class SimpleLRUSpec extends AnyWordSpec {

  var time = 0L

  private object MockedLRU extends SimpleLRU[Int](10, 100) {
    override protected def currentTime: Long = SimpleLRUSpec.this.time
  }

  "It" should {

    "Respond false with all missing entries and preserve length" in {
      val results = (0 until 100).map { i => MockedLRU.checkAndRefreshEntry(i) }
      assert( results.forall( _ == false ) )
    }

    "Drop the records according to maxElements" in {
      val existing = (90 until 100).map { i => MockedLRU.checkAndRefreshEntry(i) }
      assert( existing.forall( _ == true ) )

      // maxElements guaranteed that we have no space for those
      val absent = (0 until 10).map { i => MockedLRU.checkAndRefreshEntry(i) }
      assert( absent.forall( _ == false ) )
    }

    "Obsolete the records according to ttlMillis" in {
      this.time = 0L
      var results = (0 until 5).map { i => MockedLRU.checkAndRefreshEntry(i) }
      assert( results.forall( _ == true ) )

      this.time = 50L
      results = (5 until 10).map { i => MockedLRU.checkAndRefreshEntry(i) }
      assert( results.forall( _ == true ) )

      this.time = 150L
      results = (0 until 5).map { i => MockedLRU.checkAndRefreshEntry(i) }
      assert( results.forall( _ == false ) )

      // those were added at 50ms and still valid
      results = (5 until 10).map { i => MockedLRU.checkAndRefreshEntry(i) }
      assert( results.forall( _ == true ) )

      this.time = 300L
      results = (0 until 10).map { i => MockedLRU.checkAndRefreshEntry(i) }
      assert( results.forall( _ == false ) )
    }

  }

}
