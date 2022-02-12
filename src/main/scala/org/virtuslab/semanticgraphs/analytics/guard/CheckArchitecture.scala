package org.virtuslab.semanticgraphs.analytics.guard

import com.virtuslab.semanticgraphs.proto.model.graphnode.{Edge, GraphNode, Location, SemanticGraphFile}
import org.virtuslab.semanticgraphs.analytics.utils.Common
import org.virtuslab.semanticgraphs.analytics.utils.PathHelpers.StringResolver

import java.nio.file.Files

case class ArchGuardClasses(classes: Map[String, GraphNode], allNodes: Map[String, GraphNode]):
  def withName(name: String): ArchGuardClasses = ArchGuardClasses(
    classes.filter(_._2.displayName.contains(name)),
    allNodes
  )

  def inPackage(name: String): ArchGuardClasses = ArchGuardClasses(
    classes.filter(_._2.properties.get("package").exists(_.contains(name))),
    allNodes
  )

  def shouldNotExtend(extendNode: GraphNode => Boolean): Boolean =
    classes.values
      .flatMap(_.edges)
      .filter(_.`type` == "EXTENDS")
      .forall(edge => allNodes.get(edge.to).forall(extendNode))

case class ArchGuardMethods(workspace: String, methods: Map[String, GraphNode], allNodes: Map[String, GraphNode]):
  def withName(name: String): ArchGuardMethods = ArchGuardMethods(
    workspace,
    methods.filter(_._2.displayName.contains(name)),
    allNodes
  )

  def inPackage(name: String): ArchGuardMethods = ArchGuardMethods(
    workspace,
    methods.filter(_._2.properties.get("package").exists(_.contains(name))),
    allNodes
  )

  def shouldNotCall(nodeCheck: GraphNode => Boolean): Boolean =
    def locationToUri(edge: Edge): String =
      val location = edge.location.get
      import location._

      s"file://$workspace/$uri:${startLine + 1}:${startCharacter + 1}:$endLine:$endCharacter ${edge.to} ${edge.`type`}"
    val a =
      methods.values.flatMap(_.edges).filter(_.`type` == "CALL").filter(edge => allNodes.get(edge.to).exists(nodeCheck))
    a.foreach(edge => println(locationToUri(edge)))
    a.isEmpty

case class ArchGuard(workspace: String, nodes: Map[String, GraphNode]):
  def classes(): ArchGuardClasses =
    ArchGuardClasses(nodes.filter { case (__, node) => node.kind == "CLASS" }, nodes)

  def methods(): ArchGuardMethods =
    ArchGuardMethods(workspace, nodes.filter { case (__, node) => node.kind == "METHOD" }, nodes)

object CheckArchitecture extends App:

  def readNodes(workspace: String): ArchGuard =
    val dir = workspace.resolve(Common.SEMANTIC_GRAPHS_DIR)
    val nodes = scala.collection.mutable.Map.empty[String, GraphNode]
    Files
      .walk(dir)
      .forEach { path =>
        if Files
            .isRegularFile(path) && path.toString.endsWith(
            Common.SEMANTIC_GRAPH_FILE_SUFFIX
          )
        then
          val graphFile = SemanticGraphFile.parseFrom(Files.readAllBytes(path))
          nodes ++= graphFile.nodes.map(node => (node.id, node))

      }
    ArchGuard(workspace, nodes.toMap)
