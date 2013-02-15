package com.gilt.util


/**
 * Extractor object to be used in match statements. If used against
 * an Option it will match if the type of the Option's value is of type:
 *
 * - Int
 * - Long
 * - Float
 * - BigDecimal
 * - Double
 * - String
 */
object PrimitiveOption {
  def unapply(opt: Option[Any]): Option[Any] = opt match {
    case Some(o) => if (isPrimitiveType(o)) Some(o) else None
    case _ => None
  }

  def isPrimitiveType(obj: Any) = obj.isInstanceOf[Int] || obj.isInstanceOf[Long] || obj.isInstanceOf[Float] ||
                                  obj.isInstanceOf[BigDecimal] || obj.isInstanceOf[Double] || obj.isInstanceOf[String]
}
