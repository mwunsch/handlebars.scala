package com.gilt.util

import org.slf4j.{LoggerFactory, Logger}

/**
 * User: chicks
 * Date: 2/23/13
 * Time: 7:58 PM
 */
trait ProfilingUtils {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def time[R](label: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()

    logger.info("%s: %sms".format(label, (t1 - t0)))
    result
  }
}
