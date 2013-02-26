package com.gilt.util

import org.slf4j.{LoggerFactory, Logger}

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
