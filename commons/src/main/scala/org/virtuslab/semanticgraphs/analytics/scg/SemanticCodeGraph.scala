package org.virtuslab.semanticgraphs.analytics.scg

import com.virtuslab.semanticgraphs.proto.model.graphnode.{Edge, GraphNode, SemanticGraphFile}
import org.apache.commons.compress.archivers.zip.ZipFile
import org.jgrapht.Graph
import org.virtuslab.semanticgraphs.analytics.scg
import org.virtuslab.semanticgraphs.analytics.scg.ScgJGraphT.LabeledEdge
import org.virtuslab.semanticgraphs.analytics.utils.PathHelpers.*

import java.nio.file.Files

enum NetworkType:
  case SCG, CCN, CG

case class SemanticCodeGraph(
  val projectAndVersion: ProjectAndVersion,
  val nodesMap: Map[String, GraphNode],
  val networkType: NetworkType = NetworkType.SCG
):
  def projectName: String = projectAndVersion.projectName
  def version: String = projectAndVersion.version

  lazy val nodes: Iterable[GraphNode] = nodesMap.values

  lazy val graph: Graph[String, LabeledEdge] =
    buildGraph(ScgJGraphT.emptyGraph())

  private def buildGraph(graph: Graph[String, LabeledEdge]): Graph[String, LabeledEdge] =
    nodes.foreach { node =>
      graph.addVertex(node.id)
      node.edges.foreach(edge => addEdge(graph, node.id, edge.to, edge.`type`))
    }
    graph

  private def addEdge(graph: Graph[String, LabeledEdge], parentId: String, childId: String, `type`: String): Unit =
    if parentId != childId then
      graph.addVertex(childId)
      graph.addVertex(parentId)
      graph.addEdge(
        parentId,
        childId,
        LabeledEdge(parentId, childId, `type`)
      )

  def withoutZeroDegreeNodes() =
    new SemanticCodeGraph(projectAndVersion, nodesMap.filter { case (id, _) => graph.degreeOf(id) > 0 })

case class ProjectAndVersion(workspace: String, projectName: String, version: String)

object SemanticCodeGraph:

  val commonsIO = ProjectAndVersion("data/commons-io.zip", "commons-io", "2.12.0")
  val metals = ProjectAndVersion("data/metals.zip", "metals", "0.10.3")
  val springBoot = ProjectAndVersion("data/spring-boot.zip", "spring-boot", "2.7.5")
  val akka = ProjectAndVersion("data/akka.zip", "akka", "2.7.0")
  val spark = ProjectAndVersion("data/spark.zip", "spark", "3.3.0")

  val rxJava = ProjectAndVersion("data/RxJava.zip", "RxJava", "3.1.6")
  val retrofit = ProjectAndVersion("data/retrofit.zip", "retrofit", "2.9.0")
  val glide = ProjectAndVersion("data/glide.zip", "glide", "4.5.11")
  val dubbo = ProjectAndVersion("data/dubbo.zip", "dubbo", "3.2.4")
  val play = ProjectAndVersion("data/playframework.zip", "playframework", "2.8.19")
  val vertx = ProjectAndVersion("data/vert.x.zip", "vertx", "4.4.4")

  val allProjects = List(retrofit, commonsIO, play, metals, glide, vertx, rxJava, dubbo, springBoot, akka, spark)

  def readAllProjects(): List[SemanticCodeGraph] =
    allProjects.map(SemanticCodeGraph.read(_))

  def readAllProjectsCallGraph(): List[SemanticCodeGraph] =
    allProjects.map(fetchCallGraph)

  def readAllProjectsFullCallGraph(): List[SemanticCodeGraph] =
    allProjects.map(fetchFullCallGraph)

  def readAllProjectClassCollaborationGraph(): List[SemanticCodeGraph] =
    allProjects.map(fetchClassCollaborationGraph)

  def isNodeDefinedInProject(node: GraphNode): Boolean =
    node.kind.nonEmpty && node.location.isDefined && !node.kind.contains("FILE") && node.kind != "PACKAGE_OBJECT"

  def notLocal(node: GraphNode): Boolean =
    !node.id.startsWith("local")

  def isEdgeDefinedInProject(edge: Edge): Boolean =
    edge.location.isDefined

  def fetchCallGraph(projectAndVersion: ProjectAndVersion) =
    SemanticCodeGraph.read(
      projectAndVersion,
      node => isNodeDefinedInProject(node) && node.kind == "METHOD",
      edge => isEdgeDefinedInProject(edge) && edge.`type` == "CALL"
    )

  def fetchClassCollaborationGraph(projectAndVersion: ProjectAndVersion): SemanticCodeGraph = {
    val scg = SemanticCodeGraph
      .read(
        projectAndVersion,
        node => isNodeDefinedInProject(node),
        edge => isEdgeDefinedInProject(edge)
      )
      .withoutZeroDegreeNodes()

    val nodesMap = scg.nodesMap

    def extractAggregationEdges(clazz: GraphNode): Seq[Edge] = (for {
      decl <- clazz.edges.filter(_.`type` == "DECLARATION").map(_.to)
      declNode <- nodesMap.get(decl).filter(d => d.kind == "VALUE" || d.kind == "VARIABLE").toList
    } yield declNode.edges.filter(e => e.`type` == "TYPE").map(to => Edge(to.to, "AGGREGATION", to.location))).flatten

    def extractReferenceEdges(clazz: GraphNode): Seq[Edge] = (for {
      decl <- clazz.edges.filter(_.`type` == "DECLARATION").map(_.to)
      methodNode <- nodesMap.get(decl).filter(d => d.kind == "METHOD" || d.kind == "CONSTRUCTOR").toList
      pe <- methodNode.edges.filter(_.`type` == "PARAMETER").toList
      parameterNode <- nodesMap.get(pe.to).toList
    } yield methodNode.edges.filter(_.`type` == "RETURN_TYPE").map(e => Edge(e.to, "REFERENCE", e.location)) ++
      parameterNode.edges.filter(_.`type` == "TYPE").map(e => Edge(e.to, "REFERENCE", e.location))).flatten
      .filterNot(_.to == clazz.id)

    val classesCollaboration = for {
      clazz <- scg.nodes.filter(n =>
        n.kind == "CLASS" || n.kind == "INTERFACE" || n.kind == "TRAIT" || n.kind == "OBJECT"
      )
    } yield {
      val aggregated = extractAggregationEdges(clazz)
      val referenced = extractReferenceEdges(clazz).filterNot(r => aggregated.exists(a => a.location == r.location))
      val allEdges =
        referenced ++ aggregated ++ clazz.edges.filter(_.`type` == "EXTEND").map(_.copy(`type` = "INHERITANCE"))
      val newClassNode = clazz.withEdges(allEdges.toSet.toSeq)
      newClassNode
    }

    val m = classesCollaboration.map(n => n.id -> n).toMap
    SemanticCodeGraph(
      projectAndVersion.copy(projectName = s"${projectAndVersion.projectName}"),
      classesCollaboration.map(c => c.id -> c.withEdges(c.edges.filter(e => m.contains(e.to)))).toMap,
      NetworkType.CCN
    ).withoutZeroDegreeNodes()
  }

  def fetchFullCallGraph(projectAndVersion: ProjectAndVersion) =
    SemanticCodeGraph
      .read(
        projectAndVersion.copy(projectName = s"${projectAndVersion.projectName}"),
        node =>
          isNodeDefinedInProject(
            node
          ) && (node.kind == "METHOD" || node.kind == "CONSTRUCTOR" || node.kind == "VALUE" || node.kind == "VARIABLE"),
        // ) && node.kind != "CLASS" && node.kind != "OBJECT" && node.kind != "TRAIT" && node.kind != "INTERFACE",
        edge => isEdgeDefinedInProject(edge) && edge.`type` == "CALL"
      )
      .copy(networkType = NetworkType.CG)

  def readOnlyGlobalNodes(projectAndVersion: ProjectAndVersion): SemanticCodeGraph =
    val semanticCodeGraph = SemanticCodeGraph.read(
      projectAndVersion,
      nodeFilter = node => SemanticCodeGraph.isNodeDefinedInProject(node) && SemanticCodeGraph.notLocal(node)
    )
    semanticCodeGraph.withoutZeroDegreeNodes()

  def read(
    projectAndVersion: ProjectAndVersion,
    nodeFilter: GraphNode => Boolean = SemanticCodeGraph.isNodeDefinedInProject,
    edgeFilter: Edge => Boolean = SemanticCodeGraph.isEdgeDefinedInProject
  ) =
    if projectAndVersion.workspace.endsWith(".zip") then fromZip(projectAndVersion, nodeFilter, edgeFilter)
    else fromDir(projectAndVersion, nodeFilter, edgeFilter)

  private def fromDir(
    projectAndVersion: ProjectAndVersion,
    nodeFilter: GraphNode => Boolean = SemanticCodeGraph.isNodeDefinedInProject,
    edgeFilter: Edge => Boolean = SemanticCodeGraph.isEdgeDefinedInProject
  ): SemanticCodeGraph =
    lazy val nodesMap: scala.collection.mutable.Map[String, GraphNode] =
      val map = scala.collection.mutable.Map.empty[String, GraphNode]
      val dir = projectAndVersion.workspace.resolve(".semanticgraphs")
      Files
        .walk(dir)
        .iterator()
        .forEachRemaining { path =>
          if Files.isRegularFile(path) && path.toString.endsWith(".semanticgraphdb") then
            val graphFile = SemanticGraphFile.parseFrom(Files.readAllBytes(path))
            graphFile.nodes.foreach { node =>
              if nodeFilter(node) then map.update(node.id, node.copy(edges = node.edges.filter(edgeFilter)))
            }
        }
      map.map { case (id, node) =>
        (id, node.copy(edges = node.edges.filter(edge => map.isDefinedAt(edge.to))))
      } // filter out edges pointing to outside nodes
    new SemanticCodeGraph(projectAndVersion, nodesMap.toMap)

  private def fromZip(
    projectAndVersion: ProjectAndVersion,
    nodeFilter: GraphNode => Boolean = SemanticCodeGraph.isNodeDefinedInProject,
    edgeFilter: Edge => Boolean = SemanticCodeGraph.isEdgeDefinedInProject
  ): SemanticCodeGraph =
    lazy val nodesMap: scala.collection.mutable.Map[String, GraphNode] =
      val map = scala.collection.mutable.Map.empty[String, GraphNode]
      val zipFile = new ZipFile(s"${projectAndVersion.workspace}")
      val entries = zipFile.getEntries
      entries.asIterator().forEachRemaining { entry =>
        if !entry.isDirectory && entry.getName.endsWith(".semanticgraphdb") then
          val graphFile = SemanticGraphFile.parseFrom(zipFile.getInputStream(entry))
          graphFile.nodes.foreach { node =>
            if nodeFilter(node) then map.update(node.id, node.copy(edges = node.edges.filter(edgeFilter)))
          }
      }
      zipFile.close()
      map.map { case (id, node) =>
        (id, node.copy(edges = node.edges.filter(edge => map.isDefinedAt(edge.to))))
      } // filter out edges pointing to outside nodes
    new SemanticCodeGraph(projectAndVersion, nodesMap.toMap)

  def readLOC(
    projectAndVersion: ProjectAndVersion
  ): Long =
    if projectAndVersion.workspace.endsWith(".zip") then readLOCFromZip(projectAndVersion)
    else readLOCFromDir(projectAndVersion)

  private def readLOCFromDir(
    projectAndVersion: ProjectAndVersion
  ): Long =
    var loc = 0L
    val dir = projectAndVersion.workspace.resolve(".semanticgraphs")

    Files
      .walk(dir)
      .iterator()
      .forEachRemaining { path =>
        if Files.isRegularFile(path) && path.toString.endsWith(".semanticgraphdb") then
          val graphFile = SemanticGraphFile.parseFrom(Files.readAllBytes(path))
          graphFile.nodes.foreach { node =>
            if node.kind.contains("FILE") then loc += node.properties.get("LOC").map(_.toInt).getOrElse(0)
          }
      }
    loc

  private def readLOCFromZip(
    projectAndVersion: ProjectAndVersion
  ): Long =
    var loc = 0L

    val zipFile = new ZipFile(s"${projectAndVersion.workspace}")
    val entries = zipFile.getEntries
    entries.asIterator().forEachRemaining { entry =>
      if !entry.isDirectory && entry.getName.endsWith(".semanticgraphdb") then
        val graphFile = SemanticGraphFile.parseFrom(zipFile.getInputStream(entry))
        graphFile.nodes.foreach { node =>
          if node.kind.contains("FILE") then loc += node.properties.get("LOC").map(_.toInt).getOrElse(0)
        }
    }
    zipFile.close()
    loc
