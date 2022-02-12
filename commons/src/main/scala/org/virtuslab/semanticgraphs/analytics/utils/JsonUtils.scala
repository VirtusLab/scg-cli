package org.virtuslab.semanticgraphs.analytics.utils

import java.io.File

object JsonUtils:
  def dumpJsonFile(
    outputFileName: String,
    json: String
  ): Unit =
    val f = new File(outputFileName)
    val printer = new java.io.PrintWriter(f)
    printer.write(json)
    printer.close()
