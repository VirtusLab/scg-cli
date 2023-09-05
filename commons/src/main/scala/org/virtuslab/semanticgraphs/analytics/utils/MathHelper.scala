package org.virtuslab.semanticgraphs.analytics.utils

import org.virtuslab.semanticgraphs.analytics.scg.SemanticCodeGraph
import scala.jdk.CollectionConverters.*

object MathHelper {

  def calculateVariance(numbers: List[Int]): Double = {

    val mean = numbers.sum.toDouble / numbers.length
    val squaredDifferences = numbers.map(x => math.pow(x - mean, 2))
    val variance = squaredDifferences.sum / numbers.length

    variance
  }

  def calculateCOD(data: List[Int]): Double = {
    // Calculate the mean (average)
    val mean = data.sum.toDouble / data.length

    // Calculate the variance
    val variance = calculateVariance(data)

    // Calculate the index of dispersion (COD)
    val cod = variance / mean

    cod
  }

  def extractDistribution(scg: SemanticCodeGraph): Distribution = {
    val g = scg.graph
    val inDegrees = g.vertexSet().asScala.toList.map(v => g.inDegreeOf(v))
    val outDegrees = g.vertexSet().asScala.toList.map(v => g.outDegreeOf(v))
    val varianceID = MathHelper.calculateVariance(inDegrees)
    val varianceOD = MathHelper.calculateVariance(outDegrees)
    Distribution(
      in = inDegrees,
      varianceID = varianceID,
      deviationID = math.sqrt(varianceID),
      out = outDegrees,
      deviationOD = math.sqrt(varianceOD),
      varianceOD = varianceOD
    )
  }

  case class Distribution(
    in: List[Int],
    varianceID: Double,
    deviationID: Double,
    out: List[Int],
    varianceOD: Double,
    deviationOD: Double
  )

}
