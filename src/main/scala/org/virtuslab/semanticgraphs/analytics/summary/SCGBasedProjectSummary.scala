package org.virtuslab.semanticgraphs.analytics.summary

import scala.jdk.CollectionConverters.*
import org.virtuslab.semanticgraphs.analytics.crucial.{CrucialNodes, CrucialNodesSummary}
import org.virtuslab.semanticgraphs.analytics.metrics.JGraphTMetrics
import org.virtuslab.semanticgraphs.analytics.scg.{ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.utils.FileUtils

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import upickle.default.*

case class NodeKindAndNumber(kind: String, number: Int) derives ReadWriter
case class EdgeTypeAndNumber(`type`: String, number: Int) derives ReadWriter
case class SCGProjectSummary(
  name: String,
  version: String,
  nodes: Int,
  edges: Int,
  nodesDistribution: List[NodeKindAndNumber],
  edgesDistribution: List[EdgeTypeAndNumber],
  density: Double,
  averageInDegree: Double,
  averageOutDegree: Double,
  globalClusteringCoefficient: Double,
  assortativityCoefficient: Double,
  totalLoc: Long
) derives ReadWriter

object SCGProjectSummary:
  def summary(scg: SemanticCodeGraph): SCGProjectSummary =
    val semanticCodeGraph = scg.withoutZeroDegreeNodes()
    val nodesTotal = semanticCodeGraph.graph.vertexSet().size()
    val edgesTotal = semanticCodeGraph.graph.edgeSet().size()

    val edgeDistribution = semanticCodeGraph.graph
      .edgeSet()
      .asScala
      .toList
      .map(_.role)
      .groupBy(identity)
      .map { case (role, set) => EdgeTypeAndNumber(role, set.size) }
      .toList
      .sortBy(-_.number)
    val nodesDistribution = semanticCodeGraph.nodes
      .map(_.kind)
      .groupBy(identity)
      .map { case (kind, set) => NodeKindAndNumber(kind, set.size) }
      .toList
      .sortBy(-_.number)

    val density = JGraphTMetrics.density(semanticCodeGraph.graph)

    val averageInDegree = JGraphTMetrics.averageInDegree(semanticCodeGraph.graph)
    val averageOutDegree = JGraphTMetrics.averageOutDegree(semanticCodeGraph.graph)
    val globalClusteringCoefficient = JGraphTMetrics.globalClusteringCoefficient(semanticCodeGraph.graph)
    val assortativityCoefficient = JGraphTMetrics.assortativityCoefficient2(semanticCodeGraph.graph)
    val totalLoc = SemanticCodeGraph.readLOC(semanticCodeGraph.projectAndVersion)

    SCGProjectSummary(
      name = semanticCodeGraph.projectName,
      version = semanticCodeGraph.version,
      nodes = nodesTotal,
      edges = edgesTotal,
      nodesDistribution = nodesDistribution,
      edgesDistribution = edgeDistribution,
      density = density,
      averageInDegree = averageInDegree,
      averageOutDegree = averageOutDegree,
      globalClusteringCoefficient = globalClusteringCoefficient,
      assortativityCoefficient = assortativityCoefficient,
      totalLoc = totalLoc
    )

  def exportHtmlSummary(summary: SCGProjectSummary): Unit = {
    exportJsSummary("summary.js", summary)
    copySummaryHtml(Path.of("."))
  }

  def exportTxt(summary: SCGProjectSummary): Unit = {
    print(write(summary))
  }

  private def exportJsSummary(fileName: String, summary: SCGProjectSummary): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(fileName))
    val json = s"const summary = ${write(summary)};"
    pw.write(json)
    pw.close()
  }

  private def copySummaryHtml(summaryResultDirectory: Path): Unit = {
    val inputStream = getClass.getClassLoader.getResourceAsStream("summary.html")
    Files.copy(inputStream, summaryResultDirectory.resolve("summary.html"), StandardCopyOption.REPLACE_EXISTING)
    inputStream.close()
  }

object SCGBasedProjectSummary extends App:

  val projects = SemanticCodeGraph.readAllProjects() ++ SemanticCodeGraph.readAllProjectsFullCallGraph()

  def exportSummary(): Unit =
    val summary = projects.map(SCGProjectSummary.summary)
    FileUtils.dumpFile("summary.json", write(summary))

  def printSummaryLatexTable() =
    println()
    println("Name & Version & LOC & Nodes & LOC/Nodes & Edges & Density & AOD & AID & & GCC & AC\\\\")
    println("\\hline")
    projects.map(_.withoutZeroDegreeNodes()).foreach { semanticCodeGraph =>
      val nodesTotal = semanticCodeGraph.graph.vertexSet().size()
      val edgesTotal = semanticCodeGraph.graph.edgeSet().size()
      val density = JGraphTMetrics.density(semanticCodeGraph.graph)

      val averageInDegree = JGraphTMetrics.averageInDegree(semanticCodeGraph.graph)
      val averageOutDegree = JGraphTMetrics.averageOutDegree(semanticCodeGraph.graph)
      val globalClusteringCoefficient = JGraphTMetrics.globalClusteringCoefficient(semanticCodeGraph.graph)
      val assortativityCoefficient = JGraphTMetrics.assortativityCoefficient2(semanticCodeGraph.graph)
      val totalLoc = SemanticCodeGraph.readLOC(semanticCodeGraph.projectAndVersion)

      println(
        f"${semanticCodeGraph.projectName} & ${semanticCodeGraph.version} & $totalLoc & $nodesTotal & ${totalLoc.toDouble / nodesTotal}%1.2f  & $edgesTotal & $density%1.5f & $averageOutDegree%1.2f & $averageInDegree%1.2f & $globalClusteringCoefficient%1.2f & $assortativityCoefficient%1.2f\\\\"
      )
    }
    println()

  exportSummary()
  printSummaryLatexTable()

end SCGBasedProjectSummary

object ComputeProjectSummary extends App:

  import scala.concurrent.ExecutionContext.Implicits.global

  def analyze(projects: List[SemanticCodeGraph], filePrefix: String): Future[List[CrucialNodesSummary]] =
    Future.sequence(projects.map { project =>
      Future {
        CrucialNodes.analyze(project, filePrefix, 10)
      }.recover { case e =>
        println(s"Exception for ${project.projectName} ${e.getMessage}")
        CrucialNodesSummary(project.projectName, project.projectAndVersion.workspace, Nil)
      }
    })

  def printStats(results: List[CrucialNodesSummary]) =
    println("Crucial nodes")
    println(results.head.stats.drop(3).map(_.id).mkString("Name & ", " & \\# & ", " & \\# \\\\"))
    results.foreach { stats =>
      val topStats = stats.stats.drop(3).map { s =>
        val topNode = s.nodes.head
        if topNode.score == topNode.score.toLong then f"${topNode.label} & ${topNode.score}"
        else f"${topNode.label} & ${topNode.score}%.4f"
      }
      println(s"${stats.projectName} & ${topStats.mkString("", " & ", " \\\\")}")
    }
    println("Finished")

  val all = analyze(SemanticCodeGraph.readAllProjects(), "scg")
  val fullCallGraph = analyze(SemanticCodeGraph.readAllProjectsFullCallGraph(), "call")
  val whole = for
    allR <- all
    // callR <- callGraph
    fullR <- fullCallGraph
  yield
    printStats(allR)
    // printStats(callR)
    printStats(fullR)

  Await.result(whole, Duration.Inf)
