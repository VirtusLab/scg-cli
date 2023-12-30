package org.virtuslab.semanticgraphs.analytics.metrics

import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode
import org.jgrapht.alg.clustering.{GirvanNewmanClustering, KSpanningTreeClustering, LabelPropagationClustering}
import org.jgrapht.alg.connectivity.{
  BiconnectivityInspector,
  ConnectivityInspector,
  KosarajuStrongConnectivityInspector
}
import org.jgrapht.alg.scoring.{BetweennessCentrality, ClusteringCoefficient}
import org.jgrapht.alg.shortestpath.{BFSShortestPath, DijkstraShortestPath, GraphMeasurer, JohnsonShortestPaths}
import org.jgrapht.graph.{AsUndirectedGraph, DefaultEdge}
import org.jgrapht.graph.builder.GraphTypeBuilder
import org.jgrapht.{Graph, GraphMetrics}
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO
import org.virtuslab.semanticgraphs.analytics.partitions.PartitionResults
import org.virtuslab.semanticgraphs.analytics.scg.ScgJGraphT
import org.virtuslab.semanticgraphs.analytics.scg.ScgJGraphT.LabeledEdge
import scala.jdk.CollectionConverters.MapHasAsScala

object JGraphTMetrics:

  import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

  def computeStronglyConnectedComponents(directedGraph: Graph[String, LabeledEdge]): Map[String, Int] =
    val scAlg = new KosarajuStrongConnectivityInspector(directedGraph)
    import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}
    scAlg.stronglyConnectedSets().asScala.toList.map(_.asScala.toSet).zipWithIndex.foldLeft(Map.empty[String, Int]) {
      case (map, (nodes, index)) => nodes.foldLeft(map)((r, node) => r.updated(node, index))
    }

  def computeBridges(directedGraph: Graph[String, LabeledEdge]): Unit =
    val scAlg = new BiconnectivityInspector(directedGraph)
    import scala.jdk.CollectionConverters.SetHasAsScala
    scAlg.getBridges.asScala.foreach(le => println(s"${le.parentId}--${le.role}-->${le.childId}"))

  /**
    * Returns map of node id and it's component number
    */
  def computeConnectedComponents(graph: Graph[String, LabeledEdge]): Map[String, Int] =
    val connectedComponents = new ConnectivityInspector(graph)

    connectedComponents
      .connectedSets()
      .asScala
      .toList
      .map(_.asScala.toSet)
      .zipWithIndex
      .foldLeft(Map.empty[String, Int]) { case (map, (nodes, index)) =>
        nodes.foldLeft(map)((r, node) => r.updated(node, index))
      }

  def partition(directedGraph: Graph[String, LabeledEdge], nparts: Int): Map[String, Int] =
    val part = new KSpanningTreeClustering[String, LabeledEdge](directedGraph, nparts)
    part.getClustering.getClusters.asScala.toList.map(_.asScala.toSet).zipWithIndex.foldLeft(Map.empty[String, Int]) {
      case (map, (nodes, index)) => nodes.foldLeft(map)((r, node) => r.updated(node, index))
    }
  def labelPropagationClustering(graph: List[GraphNodeDTO], maxIterations: Int): PartitionResults =
    val undirectedGraph = ScgJGraphT.exportUndirected(graph)
    val part = new LabelPropagationClustering[String, LabeledEdge](undirectedGraph, maxIterations)
    val nodeToPart =
      part.getClustering.getClusters.asScala.toList.map(_.asScala.toSet).zipWithIndex.foldLeft(Map.empty[String, Int]) {
        case (map, (nodes, index)) => nodes.foldLeft(map)((r, node) => r.updated(node, index))
      }
    PartitionResults(
      "label",
      graph,
      part.getClustering.getClusters.size(),
      nodeToPart,
      comment = "Calculated by JGraphT LabelPropagationClustering"
    )

  def GirvanNewmanClustering(graph: List[GraphNodeDTO], k: Int): PartitionResults =
    val undirectedGraph = ScgJGraphT.exportUndirected(graph)
    val part = new GirvanNewmanClustering[String, LabeledEdge](undirectedGraph, k)
    val nodeToPart =
      part.getClustering.getClusters.asScala.toList.map(_.asScala.toSet).zipWithIndex.foldLeft(Map.empty[String, Int]) {
        case (map, (nodes, index)) => nodes.foldLeft(map)((r, node) => r.updated(node, index))
      }
    PartitionResults(
      "Girvan",
      graph,
      part.getClustering.getClusters.size(),
      nodeToPart,
      comment = "Calculated by JGraphT GirvanNewmanClustering"
    )

  def betweennessCentrality(graph: Graph[String, LabeledEdge]) =
    new BetweennessCentrality(graph, false).getScores

  def density(graph: Graph[String, LabeledEdge]): Double =
    val E = graph.edgeSet().size().toDouble
    val V = graph.vertexSet().size().toDouble
    E / (V * (V - 1))

  def averageDegree(graph: Graph[String, LabeledEdge]): Double =
    val E = graph.edgeSet().size().toDouble
    val V = graph.vertexSet().size().toDouble
    E / V

  def assortativityCoefficient(graph: Graph[String, LabeledEdge]): Double =
    val edgeCount = graph.edgeSet.size
    var n1, n2, dn = 0.0

    import scala.jdk.CollectionConverters.SetHasAsScala
    for e <- graph.edgeSet.asScala do
      val d1 = graph.degreeOf(graph.getEdgeSource(e))
      val d2 = graph.degreeOf(graph.getEdgeTarget(e))
      n1 += d1 * d2
      n2 += d1 + d2
      dn += d1 * d1 + d2 * d2
    n1 /= edgeCount
    n2 = (n2 / (2 * edgeCount)) * (n2 / (2 * edgeCount))
    dn /= (2 * edgeCount)

    (n1 - n2) / (dn - n2)

  def assortativityCoefficient2(graph: Graph[String, LabeledEdge]): Double =
    import scala.jdk.CollectionConverters.SetHasAsScala
    val S1 = 2 * graph.edgeSet().size()
    val S2 = graph.vertexSet().asScala.foldLeft(0.0) { case (r, x) => r + Math.pow(graph.degreeOf(x), 2) }
    val S3 = graph.vertexSet().asScala.foldLeft(0.0) { case (r, x) => r + Math.pow(graph.degreeOf(x), 3) }

    var S_e = 0.0
    for e <- graph.edgeSet.asScala do
      val d1 = graph.degreeOf(graph.getEdgeSource(e))
      val d2 = graph.degreeOf(graph.getEdgeTarget(e))
      S_e += d1 * d2

    (S_e * S1 - Math.pow(S2, 2)) / (S3 * S1 - Math.pow(S2, 2))

  def averageClusteringCoefficient(directed: Graph[String, LabeledEdge]): Double =
    new ClusteringCoefficient[String, LabeledEdge](directed).getAverageClusteringCoefficient

  def globalClusteringCoefficient(graph: Graph[String, LabeledEdge]): Double =
    new ClusteringCoefficient[String, LabeledEdge](
      if graph.getType.isDirected then
        new AsUndirectedGraph(graph)
      else
        graph
    ).getGlobalClusteringCoefficient

  def getClusteringCoefficientScores(graph: Graph[String, LabeledEdge]): scala.collection.mutable.Map[String, java.lang.Double] = {
    new ClusteringCoefficient[String, LabeledEdge](
      new AsUndirectedGraph(graph)
    ).getScores.asScala
  }

  def averageOutDegree(graph: Graph[String, LabeledEdge]): Double =
    val nodes = graph.vertexSet().asScala.toList.map(v => graph.outDegreeOf(v))
    nodes.sum / nodes.size.toDouble

  def averageInDegree(graph: Graph[String, LabeledEdge]): Double =
    val nodes = graph.vertexSet().asScala.toList.map(v => graph.inDegreeOf(v))
    nodes.sum / nodes.size.toDouble

  def median(graph: Graph[String, LabeledEdge]): Double =
    val nodes = graph.vertexSet().asScala.toList.map(v => graph.degreeOf(v)).filter(_ > 0).sorted
    if nodes.size % 2 == 1 then nodes(nodes.size / 2)
    else
      val up = nodes(nodes.size / 2)
      val down = nodes(nodes.size / 2 + 1)
      (up + down) / 2

  def numberOfTriangles(undirectedGraph: Graph[String, LabeledEdge]): Long =
    GraphMetrics.getNumberOfTriangles(new AsUndirectedGraph(undirectedGraph))

  def radius(graph: Graph[String, LabeledEdge]): Double =
    new GraphMeasurer[String, LabeledEdge](graph, new BFSShortestPath[String, LabeledEdge](graph)).getRadius

  case class DistanceBasedMetrics(radius: Double, diameter: Double)

  def distanceBasedMetrics(graph: Graph[String, LabeledEdge]): DistanceBasedMetrics = {
    val undirected = new AsUndirectedGraph(graph)
    val graphMeasurer =
      new GraphMeasurer[String, LabeledEdge](undirected, new DijkstraShortestPath[String, LabeledEdge](undirected))
    val radius = graphMeasurer.getRadius
    val diameter = graphMeasurer.getDiameter
    DistanceBasedMetrics(radius, diameter)
  }
