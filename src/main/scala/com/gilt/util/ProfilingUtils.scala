package com.gilt.util

import org.slf4j.{LoggerFactory, Logger}

object ProfilingUtils {
  private[ProfilingUtils] val logger: Logger = LoggerFactory.getLogger(getClass)
}

trait ProfilingUtils {
  import ProfilingUtils._

  def time[R](label: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()

    logger.info("%s: %s ms".format(label, (t1 - t0)))
    result
  }
}
