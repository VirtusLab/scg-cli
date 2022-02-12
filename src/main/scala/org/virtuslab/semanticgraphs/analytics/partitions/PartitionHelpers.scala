package org.virtuslab.semanticgraphs.analytics.partitions

import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO
import org.virtuslab.semanticgraphs.analytics.metrics.JGraphTMetrics
import org.virtuslab.semanticgraphs.analytics.partitions.patoh.PatohClustering.workspace
import org.virtuslab.semanticgraphs.analytics.scg.SemanticCodeGraph
import org.virtuslab.semanticgraphs.analytics.utils.MultiPrinter

import java.io.{File, PrintWriter}
import java.nio.file.Path

object PartitionHelpers:

  def multiPrinter(projectName: String, method: String): (File, MultiPrinter) =
    val tmpFolder = new File(Path.of(s"/tmp/$method-$projectName").toUri)
    if !tmpFolder.exists() then tmpFolder.mkdir()
    val filePrinter = new PrintWriter(new File(s"${tmpFolder.getAbsolutePath}/$projectName-results.txt"))
    (tmpFolder, new MultiPrinter(filePrinter, new PrintWriter(System.out)))

  def takeBiggestComponentOnly(semanticCodeGraph: SemanticCodeGraph): List[GraphNode] =
    val stronglyConnectedComponents = JGraphTMetrics.computeConnectedComponents(semanticCodeGraph.graph)
    val nodes = semanticCodeGraph.nodes
    val biggestConnectedGroup = stronglyConnectedComponents.groupBy(_._2).maxBy(_._2.keys.size)._2
    nodes.filter(n => biggestConnectedGroup.isDefinedAt(n.id)).toList

  def dumpCsv(
    projectName: String,
    nodes: List[GraphNode],
    nodeToIndex: scala.collection.mutable.Map[String, Int]
  ): Unit =
    val csvLines = nodes
      .collect {
        case node if nodeToIndex.contains(node.id) =>
          val nodeNumber = nodeToIndex(node.id)
          (
            nodeNumber,
            s"$nodeNumber,${node.id},${node.displayName},${node.properties.get("package").filter(_.nonEmpty).getOrElse("__empty__")}"
          )
      }
      .sortBy(_._1)
      .map(_._2)

    val f = new File(s"$projectName.csv")
    val printer = new java.io.PrintWriter(f)
    csvLines.foreach { line =>
      printer.println(line)
    }
    printer.close()

  def exportAllToGDF(
    maxNParts: Int,
    nodes: List[GraphNodeDTO],
    outputFileName: String,
    partitionResults: List[PartitionResults]
  ): Unit =
    val f = new File(outputFileName)
    val printer = new java.io.PrintWriter(f)

    val results: Map[Int, Map[String, Int]] = partitionResults.map(x => (x.nparts, x.nodeToPart)).toMap

    printer.println(
      "nodedef> name VARCHAR, label VARCHAR, kind VARCHAR, uri VARCHAR, loc INTEGER, package VARCHAR, " + (2 to maxNParts)
        .map(i => s"nparts-$i")
        .mkString(" INTEGER,") + " INTEGER"
    )
    nodes.foreach { node =>
      val printResult = (2 to maxNParts).map(i => results(i)(node.id)).mkString(",")
      import node._
      printer.println(
        s"$id, $displayName, $kind, ${location.map(_.uri).getOrElse("")}, ${loc.getOrElse(0)}, ${`package`
            .getOrElse("__empty__")}, $printResult"
      )
    }
    printer.println(
      "edgedef> source VARCHAR, target VARCHAR, type VARCHAR, directed BOOLEAN, uri VARCHAR, label VARCHAR"
    )
    nodes.foreach { node =>
      node.edges.foreach { edge =>
        import edge._
        printer.println(
          s"${node.id}, $to, ${`type`}, true, ${location.map(_.uri).getOrElse("")}, ${`type`}"
        )
      }
    }

    printer.close()

  def exportToGdf(
    outputFileName: String,
    nodes: List[GraphNodeDTO],
    nodeToPartMap: Map[String, Int],
    connectedComponents: Map[String, Int]
  ): Unit =
    val f = new File(outputFileName)
    val printer = new java.io.PrintWriter(f)

    printer.println(
      "nodedef> name VARCHAR, label VARCHAR, kind VARCHAR, uri VARCHAR, loc INTEGER, part INTEGER, package VARCHAR, component INTEGER"
    )
    nodes.foreach { node =>
      import node._
      printer.println(
        s"$id, $displayName, $kind, ${location
            .map(_.uri)
            .getOrElse("")}, ${loc.getOrElse(0)}, ${nodeToPartMap(node.id)}, ${`package`
            .getOrElse("__empty__")}, ${connectedComponents(node.id)}"
      )
    }
    printer.println(
      "edgedef> source VARCHAR, target VARCHAR, type VARCHAR, directed BOOLEAN, uri VARCHAR, label VARCHAR"
    )
    nodes.foreach { node =>
      node.edges.foreach { edge =>
        import edge._
        printer.println(
          s"${node.id}, $to, ${`type`}, true, ${location.map(_.uri).getOrElse("")}, ${`type`}"
        )
      }
    }

    printer.close()
