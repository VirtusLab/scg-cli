package org.virtuslab.semanticgraphs.analytics.partitions.patoh

import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode
import org.virtuslab.semanticgraphs.analytics.dto.{EdgeDTO, GraphNodeDTO}
import org.virtuslab.semanticgraphs.analytics.partitions.{DockerDistribution, PartitionHelpers, PartitionResults}
import org.virtuslab.semanticgraphs.analytics.scg.{ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.utils.MultiPrinter
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO.toGraphNodeDto

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object PatohClustering extends App:

  val workspace = args(0)
  val nparts = args(1).toInt
  val projectName = workspace.split("/").lastOption.get
  implicit val (tmpFolder: File, multiPrinter: MultiPrinter) = PartitionHelpers.multiPrinter(projectName, "patoh")

  val biggestComponentNodes =
    PartitionHelpers
      .takeBiggestComponentOnly(
        SemanticCodeGraph.readOnlyGlobalNodes(ProjectAndVersion(workspace, projectName, ""))
      )
      .map(_.toGraphNodeDto)

  val results = PatohPartitions.partition(biggestComponentNodes, projectName, nparts, false, PA = 13, IB = 0.9)

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

object PatohPartitions:

  def partition(
    nodes: List[GraphNodeDTO],
    projectName: String,
    nparts: Int,
    useDocker: Boolean,
    PA: Int,
    IB: Double
  ): List[PartitionResults] =
    val indexes = PatohPartitions.exportPatohInputGraph(projectName, nodes)
    val result = computePatohPartitioning(nodes, indexes, nparts, projectName, useDocker, PA, IB)
    new File(s"$projectName.patoh").delete()
    result

  def computePatohPartitioning(
    nodes: List[GraphNodeDTO],
    indexes: Array[String],
    nparts: Int,
    projectName: String,
    useDocker: Boolean,
    PA: Int,
    IB: Double,
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
            "patoh",
            s"$projectName.patoh",
            nparts,
            f"IB=${IB}%1.2f",
            "PA=13"
          ).call()
        else
          os.proc("patoh", s"$projectName.patoh", nparts, f"IB=${IB}%1.2f", s"PA=$PA")
            .call()

      if computing.exitCode != 0 then throw new RuntimeException(s"Computation failed")
      println(computing.out.text())

      val patohPartFile = s"$projectName.patoh.part.$nparts"
      val patohResults = readPatohResults(patohPartFile, indexes)
      new File(patohPartFile).delete()

      computePatohPartitioning(nodes, indexes, nparts - 1, projectName, useDocker, PA, IB) :+ PartitionResults(
        method = "patoh",
        nodes = nodes,
        nparts = nparts,
        nodeToPart = patohResults,
        comment = computing.out.text()
      )
    else Nil

  def readPatohResults(file: String, indexes: Array[String]): Map[String, Int] =
    Source
      .fromFile(file)
      .getLines()
      .toList
      .zipWithIndex
      .map { case (part, index) => (indexes(index), part.toInt) }
      .toMap

  def exportPatohInputGraph(projectName: String, nodes: List[GraphNodeDTO]): Array[String] =
    val (nodeToIndex, networks) = toNodeAndEdges(nodes)
    dumpGraph(projectName, nodeToIndex.size, networks)
    nodeToIndex.toList.sortBy(_._2).map(_._1).toArray

  private def toNodeAndEdges(nodes: List[GraphNodeDTO]): (mutable.Map[String, Int], List[Set[Int]]) =
    var counter = 0
    val nodeAndNumber = scala.collection.mutable.Map.empty[String, Int]
    def getNodeNumber(id: String): Int =
      nodeAndNumber.getOrElseUpdate(id, { counter = counter + 1; counter })

    val networks = ListBuffer.empty[Set[Int]]
    def addNetwork(network: Set[Int]): Unit =
      networks.addOne(network)
    nodes.foreach { currentNode =>
      if currentNode.edges.nonEmpty then
        val nodeNumber = getNodeNumber(currentNode.id)

        extension (edges: Seq[EdgeDTO])
          def asHyperEdge(edgeType: String, exclude: Boolean = false): Seq[EdgeDTO] = {
            addNetwork(edges.filter(_.`type` == edgeType).map(_.to).map(getNodeNumber).toSet + nodeNumber)
            if exclude then
              edges.filterNot(_.`type` == edgeType)
            else
              edges
          }

//        val rest = currentNode.edges
//          .asHyperEdge("PARAMETER", false)
//          .asHyperEdge("DECLARATION", false)
//
//          .asHyperEdge("TYPE_PARAMETER", false)
//          .asHyperEdge("RETURN_TYPE", false)
//          .asHyperEdge("EXTEND", false)
//          .asHyperEdge("CALL", false)

        val types = currentNode.edges.map(_.`type`).distinct
        types.foreach { edgeType =>
          currentNode.edges.asHyperEdge(edgeType)
        }

        currentNode.edges.foreach { edge =>
          val to = getNodeNumber(edge.to)
          addNetwork(Set(to, nodeNumber))
        }



//        val callEdges = currentNode.edges.filter(_.`type` == "CALL")
//        if callEdges.nonEmpty then
//          addNetwork(callEdges.map(_.to).map(getNodeNumber).toSet + nodeNumber)
//          callEdges.foreach { edge =>
//            val to = getNodeNumber(edge.to)
//            addNetwork(Set(to, nodeNumber))
//          }
//        val declarationEdges = currentNode.edges.filter(_.`type` == "DECLARATION")
//        if declarationEdges.nonEmpty then addNetwork(declarationEdges.map(_.to).map(getNodeNumber).toSet + nodeNumber)
//        val rest = currentNode.edges.filterNot(x => x.`type` == "DECLARATION" || x.`type` == "CALL")
//        if rest.nonEmpty then addNetwork(rest.map(_.to).map(getNodeNumber).toSet + nodeNumber)
    }

    (nodeAndNumber, networks.toList)

  private def dumpGraph(
    projectName: String,
    vSize: Int,
    networks: List[Set[Int]]
  ): Unit =
    val nSize = networks.size
    val pinsSize = networks.map(_.size).sum

    val f = new File(s"$projectName.patoh")
    val printer = new java.io.PrintWriter(f)
    printer.println(s"1 $vSize $nSize $pinsSize")

    networks.foreach { network =>
      printer.println(s"${network.mkString(" ")}")
    }

    printer.close()
