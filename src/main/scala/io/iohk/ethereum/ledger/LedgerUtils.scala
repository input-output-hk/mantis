package io.iohk.ethereum.ledger

import scala.annotation.tailrec

object LedgerUtils {

  /** Function finds minimal value in some interval for which provided function do not return error
    * If searched value is not in provided interval, function returns maximum value of searched interval
    * @param min minimum of searched interval
    * @param max maximum of searched interval
    * @param f function which return error in case to little value provided
    * @return minimal value for which provided function do not return error
    */
  @tailrec
  private[ledger] def binaryChop[Error](min: BigInt, max: BigInt)(f: BigInt => Option[Error]): BigInt = {
    assert(min <= max)

    if (min == max)
      max
    else {
      val mid = min + (max - min) / 2
      val possibleError = f(mid)
      if (possibleError.isEmpty)
        binaryChop(min, mid)(f)
      else
        binaryChop(mid + 1, max)(f)
    }
  }
}
