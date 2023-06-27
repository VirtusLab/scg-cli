package org.virtuslab.semanticgraphs.analytics.utils

import java.io.File

object FileUtils:

  def dumpFile(
    outputFileName: String,
    content: String
  ): Unit =
    val f = new File(outputFileName)
    val printer = new java.io.PrintWriter(f)
    printer.write(content)
    printer.close()
