package org.virtuslab.semanticgraphs.analytics.utils

import geny.ByteData

import java.io.PrintWriter

class MultiPrinter(printers: PrintWriter*):

  def println(msg: String): Unit =
    printers.foreach { printer =>
      printer.println(msg)
      printer.flush()
    }

  def printf(msg: String, i: Any*): Unit =
    printers.foreach { printer =>
      printer.printf(msg, i)
      printer.flush()
    }

  def println(byteDate: ByteData): Unit =
    printers.foreach { printer =>
      printer.println(byteDate.text())
      printer.flush()
    }
