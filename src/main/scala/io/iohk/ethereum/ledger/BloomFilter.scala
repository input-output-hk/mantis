package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain.TxLogEntry
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.utils.ByteUtils.or

object BloomFilter {

  val BloomFilterByteSize: Int = 256
  private val BloomFilterBitSize: Int = BloomFilterByteSize * 8
  val EmptyBloomFilter: ByteString = ByteString(Array.fill(BloomFilterByteSize)(0.toByte))
  private val IntIndexesToAccess: Set[Int] = Set(0, 2, 4)

  /**
    * Given the logs of a receipt creates the bloom filter associated with them
    * as stated in section 4.4.1 of the YP
    *
    * @param logs from the receipt whose bloom filter will be created
    * @return bloom filter associated with the logs
    */
  def create(logs: Set[TxLogEntry]): ByteString = {
    val bloomFilters = logs.map(createBloomFilterForLogEntry)
    if(bloomFilters.isEmpty)
      EmptyBloomFilter
    else
      ByteString(or(bloomFilters.toSeq: _*))
  }

  //Bloom filter function that reduces a log to a single 256-byte hash based on equation 24 from the YP
  private def createBloomFilterForLogEntry(logEntry: TxLogEntry): Array[Byte] = {
    val dataForBloomFilter = logEntry.loggerAddress.bytes +: logEntry.logTopics
    val bloomFilters = dataForBloomFilter.map(bytes => bloomFilter(bytes.toArray))

    or(bloomFilters: _*)
  }

  //Bloom filter that sets 3 bits out of 2048 based on equations 25-28 from the YP
  private def bloomFilter(bytes: Array[Byte]): Array[Byte] = {
    val hashedBytes = kec256(bytes)
    val bitsToSet = IntIndexesToAccess.map{ i =>
      val index16bit = (hashedBytes(i + 1) & 0xFF) + ((hashedBytes(i) & 0xFF) << 8)
      index16bit % BloomFilterBitSize //Obtain only 11 bits from the index
    }
    bitsToSet.foldLeft(EmptyBloomFilter.toArray){ case (prevBloom, index) => setBit(prevBloom, index) }.reverse
  }

  private def setBit(bytes: Array[Byte], bitIndex: Int): Array[Byte] = {
    require(bitIndex / 8 < bytes.length, "Only bits between the bytes array should be set")

    val byteIndex = bitIndex / 8
    val newByte: Byte = (bytes(byteIndex) | 1 << (bitIndex % 8).toByte).toByte
    bytes.updated(byteIndex, newByte)
  }

}
