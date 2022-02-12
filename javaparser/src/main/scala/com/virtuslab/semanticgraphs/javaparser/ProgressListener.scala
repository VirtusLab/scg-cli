package com.virtuslab.semanticgraphs.javaparser

import java.util.concurrent.atomic.AtomicInteger

trait ProgressListener {
  def update(progress: Double): Unit

  def setIndeterminate(value: Boolean): Unit
}

case class ProgressReporter(
  maybeProgressListener: Option[ProgressListener],
  totalNumberOfElements: Int
) {
  private val reportedElements = AtomicInteger(0)

  def reportNewElement(): Unit =
    maybeProgressListener.foreach(_.update(calculateProgress(reportedElements.incrementAndGet())))

  private def calculateProgress(numberOfReportedElements: Int): Double =
    numberOfReportedElements.toDouble / totalNumberOfElements
}
