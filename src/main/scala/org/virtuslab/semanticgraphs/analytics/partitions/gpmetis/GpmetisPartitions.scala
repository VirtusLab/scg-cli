package org.virtuslab.semanticgraphs.analytics.partitions.gpmetis

import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO
import org.virtuslab.semanticgraphs.analytics.partitions.{DockerDistribution, PartitionHelpers, PartitionResults}
import org.virtuslab.semanticgraphs.analytics.scg.{ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.utils.MultiPrinter
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO.toGraphNodeDto

import java.io.File
import scala.collection.mutable
import scala.io.Source

object GpmetisPartitionsApp extends App:
  val workspace = args(0)
  val nparts = args(1).toInt
  val projectName = workspace.split("/").last

  implicit val (tmpFolder: File, multiPrinter: MultiPrinter) = PartitionHelpers.multiPrinter(projectName, "gpmetis")

  val biggestComponentNodes =
    PartitionHelpers
      .takeBiggestComponentOnly(
        SemanticCodeGraph.readOnlyGlobalNodes(ProjectAndVersion(workspace, projectName, ""))
      )
      .map(_.toGraphNodeDto)

  val results = GpmetisPartitions.partition(biggestComponentNodes, projectName, nparts, false)

  results.foreach { result =>
    PartitionHelpers.exportToGdf(
      s"${tmpFolder.getAbsolutePath}/$projectName-${result.nparts}.gdf",
      result.nodes,
      result.nodeToPart,
      Map.empty.withDefault(_ => 1)
    )
  }

  PartitionResults.print(
    multiPrinter,
    results.sortBy(_.packageDistribution.weightedAverageAccuracy)(implicitly[Ordering[Int]].reverse)
  )

  PartitionHelpers.exportAllToGDF(
    nparts,
    biggestComponentNodes,
    s"${tmpFolder.getAbsolutePath}/$projectName-all.gdf",
    results
  )

object GpmetisPartitions:

  def partition(
    nodes: List[GraphNodeDTO],
    projectName: String,
    nparts: Int,
    useDocker: Boolean
  ): List[PartitionResults] =
    val indexes = SpectralGraphUtils.exportToSpectralGraph(projectName, nodes)
    val result = GpmetisPartitions.computePartitioning(nodes, indexes, nparts, projectName, useDocker)
    new File(s"$projectName.gpmetis").delete()
    result

  def computePartitioning(
    nodes: List[GraphNodeDTO],
    indexes: Array[String],
    nparts: Int,
    projectName: String,
    useDocker: Boolean
  ): List[PartitionResults] =
    if nparts > 1 then
      val computing =
        if useDocker then
          os.proc(
            "docker",
            "run",
            "--rm",
            "-v",
            os.pwd.toString + "/:/data",
            DockerDistribution.scgCliImage,
            "gpmetis",
            "-ptype=kway",
            "-contig",
            "-objtype=cut",
            "-ufactor=1000",
            s"$projectName.gpmetis",
            nparts
          ).call()
        else
          os.proc("gpmetis", "-ptype=kway", "-contig", "-objtype=cut", "-ufactor=1000", s"$projectName.gpmetis", nparts)
            .call()
        end if

      if computing.exitCode != 0 then throw new RuntimeException(s"Computation failed")

      val gpMetisPartFile = s"$projectName.gpmetis.part.$nparts"
      val gpMetisResults = readGPMetisResults(gpMetisPartFile, indexes)
      new File(gpMetisPartFile).delete()

      computePartitioning(nodes, indexes, nparts - 1, projectName, useDocker) :+ PartitionResults(
        method = "gpmetis",
        nodes = nodes,
        nparts = nparts,
        nodeToPart = gpMetisResults,
        comment = computing.out.text()
      )
    else Nil

  private def readGPMetisResults(fileName: String, indexes: Array[String]): Map[String, Int] =
    Source
      .fromFile(fileName)
      .getLines()
      .toList
      .zipWithIndex
      .map { case (part, index) => (indexes(index), part.toInt) }
      .toMap

object SpectralGraphUtils:

  def exportToSpectralGraph(projectName: String, nodes: List[GraphNodeDTO]): Array[String] =
    val (nodeToIndex, nodeAndEdges) = toNodeAndEdges(nodes)
    dumpGraph(projectName, nodeAndEdges)
    nodeToIndex.toList.sortBy(_._2).map(_._1).toArray

  def toNodeAndEdges(nodes: Iterable[GraphNodeDTO]): (mutable.Map[String, Int], mutable.Map[Int, mutable.Set[Int]]) =
    var counter = 0
    val nodeAndNumber = scala.collection.mutable.Map.empty[String, Int]
    def getNodeNumber(id: String): Int =
      nodeAndNumber.getOrElseUpdate(id, { counter = counter + 1; counter })

    val nodeAndEdges = scala.collection.mutable.Map.empty[Int, mutable.Set[Int]]
    def createEdges(node: Int, toNodes: Set[Int]): Unit =
      nodeAndEdges.update(node, nodeAndEdges.getOrElse(node, mutable.Set.empty[Int]) union toNodes)
    def createEdge(node: Int, toNode: Int): Unit =
      nodeAndEdges.update(node, nodeAndEdges.getOrElse(node, mutable.Set.empty[Int]) union mutable.Set(toNode))

    nodes.foreach { currentNode =>
      if currentNode.edges.nonEmpty then
        val nodeNumber = getNodeNumber(currentNode.id)
        val toNodes = currentNode.edges.map(edge => getNodeNumber(edge.to)).filterNot(_ == nodeNumber).toSet
        createEdges(nodeNumber, toNodes)
        toNodes.foreach { toNode =>
          createEdge(toNode, nodeNumber) // create an undirected graph
        }
    }

    (nodeAndNumber, nodeAndEdges)

  def countNodesAndEdges(nodeAndEdges: scala.collection.mutable.Map[Int, mutable.Set[Int]]): (Int, Int) =
    val numberOfEdges = nodeAndEdges.values.map(_.size).sum
    if numberOfEdges % 2 != 0 then throw new RuntimeException(s"Number of edges is not even: $numberOfEdges")
    (nodeAndEdges.size, numberOfEdges / 2)

  private def dumpGraph(
    projectName: String,
    nodeAndEdges: scala.collection.mutable.Map[Int, mutable.Set[Int]]
  ): Unit =
    val (nodes, edges) = countNodesAndEdges(nodeAndEdges)

    val f = new File(s"$projectName.gpmetis")
    val printer = new java.io.PrintWriter(f)
    printer.println(s"$nodes $edges")

    (1 to nodeAndEdges.size).foreach { i =>
      val edges = nodeAndEdges(i)
      printer.println(s"${edges.mkString(" ")}")
    }
    printer.close()
