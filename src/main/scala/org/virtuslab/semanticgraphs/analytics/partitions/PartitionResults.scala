package org.virtuslab.semanticgraphs.analytics.partitions

import com.virtuslab.semanticgraphs.proto.model.graphnode
import com.virtuslab.semanticgraphs.proto.model.graphnode.{GraphNode, Location}
import org.jgrapht.nio.{AttributeType, DefaultAttribute}
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO
import org.virtuslab.semanticgraphs.analytics.exporters.ExportToGml
import org.virtuslab.semanticgraphs.analytics.metrics.JGraphTMetrics
import org.virtuslab.semanticgraphs.analytics.scg.{ScgJGraphT, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.utils.MultiPrinter

case class DistributionResults(
  nodes: List[GraphNodeDTO],
  nparts: Int,
  nodeToPart: Map[String, Int],
  extractGroupId: PartialFunction[GraphNodeDTO, String]
):

  lazy val groupPartitionStats = computeDistribution(nodes, extractGroupId)
  lazy val arithmeticAverageAccuracy: Int = groupPartitionStats.map(_.accuracy).sum / groupPartitionStats.size
  lazy val weightedAverageAccuracy: Int =
    groupPartitionStats.map(stat => stat.accuracy * stat.distribution.sum).sum / groupPartitionStats
      .map(_.distribution.sum)
      .sum

  private def computeDistribution(
    nodes: List[GraphNodeDTO],
    extractGroupId: PartialFunction[GraphNodeDTO, String]
  ): List[GroupPartitionStats] =
    nodes
      .collect {
        case node if extractGroupId.isDefinedAt(node) =>
          (extractGroupId(node), nodeToPart(node.id))
      }
      .foldLeft(Map.empty[String, Array[Int]].withDefault(_ => Array.ofDim[Int](nparts))) {
        case (result, (groupId, part)) =>
          result.updated(
            groupId, {
              val array = result(groupId)
              array(part) = array(part) + 1
              array
            }
          )
      }
      .map { case (_package, distribution) =>
        GroupPartitionStats(partitionName = _package, distribution = distribution.toList)
      }
      .toList
      .sorted

end DistributionResults

case class PartitionResults(
  method: String,
  nodes: List[GraphNodeDTO],
  nparts: Int,
  nodeToPart: Map[String, Int],
  comment: String
):

  lazy val distributionArithmeticAverage = nodes.size / nparts
  lazy val globalNodesDistribution = nodes.foldLeft(Array.ofDim[Int](nparts)) { (result, s) =>
    val part = nodeToPart(s.id)
    result.updated(part, result(part) + 1)
  }
  lazy val distributionVariance = (globalNodesDistribution
    .map(x1 => Math.pow(x1 - distributionArithmeticAverage, 2) / nparts)
    .sum) / (distributionArithmeticAverage * distributionArithmeticAverage)

  val modularityRatio =
    val (inner, outer) = nodes
      .map { node =>
        val nodePartNumber = nodeToPart(node.id)
        val (inner, outer) = node.edges // .filter(_.`type` == "CALL")
          .filter(edge => nodeToPart.contains(edge.to))
          .partition(edge => nodeToPart(edge.to) == nodePartNumber)
        (inner.size, outer.size)
      }
      .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    inner.toDouble / outer.toDouble

  lazy val packageDistribution = DistributionResults(
    nodes,
    nparts,
    nodeToPart,
    {
      case node if node.`package`.isDefined => node.`package`.get
    }
  )

  lazy val fileDistribution = DistributionResults(
    nodes,
    nparts,
    nodeToPart,
    {
      case node if node.location.isDefined => node.location.get.uri
    }
  )

  lazy val clusteringCoefficient: Double = (1 to nparts).map { i =>
    val filtered = nodes.filter(node => nodeToPart(node.id) == i)
    JGraphTMetrics.averageClusteringCoefficient(ScgJGraphT.exportUndirected(filtered))
  }.sum / nparts

object PartitionResults:
  def print(multiPrinter: MultiPrinter, results: List[PartitionResults]): Unit =

    val longestPackageSize =
      results.head.packageDistribution.groupPartitionStats.maxBy(_.partitionName.length).partitionName.length
    results.foreach { result =>
      multiPrinter.println(s"NParts: ${result.nparts}")
      multiPrinter.println(s"|Method |Part|Accuracy|" + "Package".padTo(longestPackageSize, ' ') + "|Distribution")
      result.packageDistribution.groupPartitionStats.foreach { distribution =>
        multiPrinter.println(
          f"|${result.method.padTo(7, ' ')}|${distribution.partition}%4d|${distribution.accuracy}%7d%%|${distribution.partitionName
              .padTo(longestPackageSize, ' ')}|[${distribution.distribution.mkString(",")}] "
        )
      }
    }

    val longestFileSize =
      results.head.fileDistribution.groupPartitionStats.maxBy(_.partitionName.length).partitionName.length
    results.foreach { result =>
      multiPrinter.println(s"NParts: ${result.nparts}")
      multiPrinter.println(s"|Method |Part|Accuracy|" + "File".padTo(longestFileSize, ' ') + "|Distribution")
      result.fileDistribution.groupPartitionStats.foreach { distribution =>
        multiPrinter.println(
          f"|${result.method.padTo(7, ' ')}|${distribution.partition}%4d|${distribution.accuracy}%7d%%|${distribution.partitionName
              .padTo(longestFileSize, ' ')}|[${distribution.distribution.mkString(",")}] "
        )
      }
    }

    val nodesSize = results.head.nodes.size
    multiPrinter.println("Partitioned project comparison")
    multiPrinter.println(
      "________________________________________________________________________________________________"
    )
    multiPrinter.println(
      "|       |     |          |           |File             |Package          |        |            |"
    )
    multiPrinter.println(
      "|Method |NPart|Modularity|Coefficient|Weighted|Accuracy|Weighted|Accuracy|Variance|Distribution"
    )
    multiPrinter.println(
      "-----------------------------------------------------------------------------------------------|"
    )
    results.foreach(printDistributionResult)
    multiPrinter.println(
      "|----------------------------------------------------------------------------------------------|"
    )

    def printDistributionResult(result: PartitionResults) =
      val file = result.fileDistribution
      val _package = result.packageDistribution
      multiPrinter.println(
        f"|${result.method.padTo(7, ' ')}|${result.nparts}%5d|${result.modularityRatio}%10.3f|${result.clusteringCoefficient}%11.3f|" +
          f"${file.weightedAverageAccuracy}%7d%%|${file.arithmeticAverageAccuracy}%7d%%|${_package.weightedAverageAccuracy}%7d%%|${_package.arithmeticAverageAccuracy}%7d%%|" +
          f"${result.distributionVariance}%8.3f|[${result.globalNodesDistribution.map(i => i * 100 / nodesSize).mkString(",")}]%%"
      )
  end print

  def exportGML(scg: SemanticCodeGraph, results: List[PartitionResults]): Unit = {
    val attributes = scg.nodes.map { node =>
      val attributes: Map[String, DefaultAttribute[Int]] = results.map{ r =>
        s"npart-${r.nparts}-${r.method}" -> new DefaultAttribute(r.nodeToPart.getOrElse(node.id, -1), AttributeType.INT)
      }.toMap
      node.id -> attributes
    }.toMap

    ExportToGml.exportToGML(
      outputFileName = s"partition-${scg.projectAndVersion.projectName}.gml",
      semanticCodeGraph = scg,
      nodeAttributes = attributes
    )
  }
  

case class GroupPartitionStats(partitionName: String, distribution: List[Int]) extends Comparable[GroupPartitionStats]:

  val (maxValue, partition) = distribution.zipWithIndex.maxBy(_._1)
  val accuracy = maxValue * 100 / distribution.sum

  override def compareTo(that: GroupPartitionStats): Int =
    implicitly[Ordering[(Int, String)]].compare((partition, partitionName), (that.partition, that.partitionName))

  override def toString: String =
    s"$partition - $partitionName -> $distribution $accuracy%"
