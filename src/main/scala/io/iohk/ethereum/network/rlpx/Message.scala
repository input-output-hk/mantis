package io.iohk.ethereum.network.rlpx



trait EncoderDecoder {
  type Version = Int
  def decode(`type`: Int, payload: Array[Byte], protocolVersion: Version): Message
}


trait Message {
  def code: Int
}
