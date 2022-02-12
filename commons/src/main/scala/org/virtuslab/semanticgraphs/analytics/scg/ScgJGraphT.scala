package org.virtuslab.semanticgraphs.analytics.scg

import org.jgrapht.Graph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.builder.GraphTypeBuilder
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO

object ScgJGraphT:

  import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

  def emptyGraph(): Graph[String, LabeledEdge] =
    GraphTypeBuilder
      .directed[String, LabeledEdge]()
      .allowingMultipleEdges(true)
      .allowingSelfLoops(false)
      .edgeClass(classOf[LabeledEdge])
      .buildGraph()

  def emptyUndirectedGraph(): Graph[String, LabeledEdge] =
    GraphTypeBuilder
      .undirected[String, LabeledEdge]()
      // .allowingMultipleEdges(true)
      .allowingSelfLoops(false)
      .edgeClass(classOf[LabeledEdge])
      .buildGraph()

  case class LabeledEdge(parentId: String, childId: String, role: String) extends DefaultEdge

  def addEdge(graph: Graph[String, LabeledEdge], parentId: String, childId: String, role: String): Unit =
    if parentId != childId then
      graph.addVertex(childId)
      graph.addVertex(parentId)
      graph.addEdge(
        parentId,
        childId,
        LabeledEdge(parentId, childId, role)
      )

  def exportUndirected(nodes: Iterable[GraphNodeDTO]): Graph[String, LabeledEdge] =
    val graph: Graph[String, LabeledEdge] = emptyUndirectedGraph()

    nodes
      .foreach { node =>
        graph.addVertex(node.id)
        node.edges.foreach(edge => addEdge(graph, node.id, edge.to, edge.`type`))
      }

    graph