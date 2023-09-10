package org.virtuslab.semanticgraphs.analytics.summary

import scala.jdk.CollectionConverters.*
import org.virtuslab.semanticgraphs.analytics.crucial.{CrucialNodes, CrucialNodesSummary}
import org.virtuslab.semanticgraphs.analytics.metrics.JGraphTMetrics
import org.virtuslab.semanticgraphs.analytics.partitions.PartitionHelpers
import org.virtuslab.semanticgraphs.analytics.scg.{ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.utils.{FileUtils, MathHelper}

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
  averageDegree: Double,
  averageClusteringCoefficient: Double,
  globalClusteringCoefficient: Double,
  assortativityCoefficient: Double,
  totalLoc: Long,
  diameter: Double,
  radius: Double
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

    val averageDegree = JGraphTMetrics.averageDegree(semanticCodeGraph.graph)
    // val averageOutDegree = JGraphTMetrics.averageOutDegree(semanticCodeGraph.graph)
    val globalClusteringCoefficient = JGraphTMetrics.globalClusteringCoefficient(semanticCodeGraph.graph)
    val averageClusteringCoefficient = JGraphTMetrics.averageClusteringCoefficient(semanticCodeGraph.graph)
    val assortativityCoefficient = JGraphTMetrics.assortativityCoefficient2(semanticCodeGraph.graph)
    val totalLoc = SemanticCodeGraph.readLOC(semanticCodeGraph.projectAndVersion)
    val biggestComponent = SemanticCodeGraph(
      scg.projectAndVersion,
      PartitionHelpers.takeBiggestComponentOnly(semanticCodeGraph).map(g => g.id -> g).toMap
    )

    // val distanceMetrics = JGraphTMetrics.distanceBasedMetrics(biggestComponent.graph)

    SCGProjectSummary(
      name = semanticCodeGraph.projectName,
      version = semanticCodeGraph.version,
      nodes = nodesTotal,
      edges = edgesTotal,
      nodesDistribution = nodesDistribution,
      edgesDistribution = edgeDistribution,
      density = density,
      averageDegree = averageDegree,
      averageClusteringCoefficient = averageClusteringCoefficient,
      globalClusteringCoefficient = globalClusteringCoefficient,
      assortativityCoefficient = assortativityCoefficient,
      totalLoc = totalLoc,
      // diameter = distanceMetrics.diameter,
      diameter = 0,
      // radius = distanceMetrics.radius
      radius = 0
    )

  def exportHtmlSummary(summary: SCGProjectSummary): Unit = {
    exportJsSummary("summary.js", summary)
    copySummaryHtml(Path.of("."))
  }

  def exportTxt(summary: SCGProjectSummary): Unit =
    print(write(summary))

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

object ComputeSCGSummaryForAllProjects extends App:

  val projects = SemanticCodeGraph.readAllProjects() ++ SemanticCodeGraph
    .readAllProjectClassCollaborationGraph() ++ SemanticCodeGraph.readAllProjectsFullCallGraph()

  def exportSummary(summaries: Seq[SCGProjectSummary]): Unit =
    FileUtils.dumpFile("summary.json", write(summaries))

  def printSummaryLatexTable(): Seq[SCGProjectSummary] =
    println()
    println(
      """Name & Version & \#LOC & $|V|$ & $\frac{\#LOC}{|V|}$ & $|E|$ & $D$ & $A_{D}$ & $\sigma_{ID}$ & $IoD_{ID} $ & $\sigma_{OD}$ & $IoD_{OD}$ & ACC & GCC & DAC \\\\"""
    )
    println("\\hline")
    projects.map(_.withoutZeroDegreeNodes()).map { semanticCodeGraph =>
      val summary = SCGProjectSummary.summary(semanticCodeGraph)

      val nodesTotal = summary.nodes
      val edgesTotal = summary.edges
      val density = summary.density

      val averageDegree = summary.averageDegree
      val averageClusteringCoefficient = summary.averageClusteringCoefficient
      val globalClusteringCoefficient = summary.globalClusteringCoefficient
      val assortativityCoefficient = summary.assortativityCoefficient
      val totalLoc = summary.totalLoc
      val diameter = summary.diameter
      val radius = summary.radius
      val distribution = MathHelper.extractDistribution(semanticCodeGraph)
      val a_ID = distribution.in.sum.toDouble / distribution.in.size
      val a_OD = distribution.out.sum.toDouble / distribution.out.size
      val sumSigma = distribution.deviationID + distribution.deviationOD
      val lowDegreeNodes_ID = distribution.in.count(_ < a_ID).toDouble / distribution.in.size * 100
      val lowDegreeNodes_OD = distribution.out.count(_ < a_OD).toDouble / distribution.out.size * 100
      val COD_ID = MathHelper.calculateCOD(distribution.in)
      val COD_OD = MathHelper.calculateCOD(distribution.out)

      println(
        f"${semanticCodeGraph.projectName} & ${semanticCodeGraph.version} & $totalLoc & $nodesTotal & ${totalLoc.toDouble / nodesTotal}%1.2f  & $edgesTotal & $density%1.5f & $averageDegree%1.1f & ${distribution.deviationID}%1.1f & $COD_ID%1.1f & ${distribution.deviationOD}%1.1f & $COD_OD%1.1f & $averageClusteringCoefficient%1.2f & $globalClusteringCoefficient%1.2f & $assortativityCoefficient%1.2f \\\\"
        // f"${semanticCodeGraph.projectName} & ${semanticCodeGraph.version} & $totalLoc & $nodesTotal & ${totalLoc.toDouble / nodesTotal}%1.2f  & $edgesTotal & $density%1.5f & $averageOutDegree%1.2f & $averageInDegree%1.2f & $globalClusteringCoefficient%1.2f & $assortativityCoefficient%1.2f & $diameter%1.0f & $radius%1.0f \\\\"
      )
      summary
    }

  private val summaries = printSummaryLatexTable()
  exportSummary(summaries)

end ComputeSCGSummaryForAllProjects

object ComputeCrucialNodesForAllProjects extends App:

  import scala.concurrent.ExecutionContext.Implicits.global

  def analyze(projects: List[SemanticCodeGraph], filePrefix: String): Future[List[CrucialNodesSummary]] =
    Future.sequence(projects.map { project =>
      Future {
        CrucialNodes.analyze(project, filePrefix, 10)
      }.recover { case e =>
        e.printStackTrace()
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

  val scg = analyze(SemanticCodeGraph.readAllProjects(), "scg")
  // val callGraph = analyze(SemanticCodeGraph.readAllProjectsFullCallGraph(), "full-call")
  // val classCollaborationGraph = analyze(SemanticCodeGraph.readAllProjectClassCollaborationGraph(), "ccn")
  val whole =
    for scgR <- scg
      // callR <- callGraph
      // callGraphR <- callGraph
      // classR <- classCollaborationGraph
    yield printStats(scgR)
    // printStats(callGraphR)
    // printStats(classR)

  Await.result(whole, Duration.Inf)
end ComputeCrucialNodesForAllProjects

object CrucialNodesTest extends App:
  val scg = SemanticCodeGraph.fetchClassCollaborationGraph(SemanticCodeGraph.metals)
  CrucialNodes.analyze(scg, "class", 10);
  println();
  Thread.sleep(100)

object ExportDistribution extends App:

  case class Distribution(in: List[Int], varianceIn: Double, out: List[Int], varianceOut: Double, cc: List[Double]) derives ReadWriter

  case class ProjectDistribution(name: String, scg: Distribution, ccn: Distribution, cg: Distribution)
    derives ReadWriter

  def extractDistribution(scg: SemanticCodeGraph): Distribution = {
    val g = scg.graph
    val in = g.vertexSet().asScala.toList.map(v => g.inDegreeOf(v))
    val out = g.vertexSet().asScala.toList.map(v => g.outDegreeOf(v))
    val ccScores = JGraphTMetrics.getClusteringCoefficientScores(g)
    println(scg.projectAndVersion.projectName)

    Distribution(
      in,
      MathHelper.calculateVariance(in),
      out,
      MathHelper.calculateVariance(out),
      ccScores.values.toList.sorted.map(_.toDouble)
    )
  }

  val result = SemanticCodeGraph.allProjects.map { project =>
    val scg = SemanticCodeGraph.read(project)
    val ccn = SemanticCodeGraph.fetchClassCollaborationGraph(project)
    val cg = SemanticCodeGraph.fetchFullCallGraph(project)
    ProjectDistribution(
      project.projectName,
      extractDistribution(scg),
      extractDistribution(ccn),
      extractDistribution(cg)
    )
  }

  FileUtils.dumpFile("distribution.json", write(result))

object ExportSummariesForEachProject extends App:

  import scala.concurrent.ExecutionContext.Implicits.global

  val projects = SemanticCodeGraph.readAllProjects() ++ SemanticCodeGraph
    .readAllProjectClassCollaborationGraph() ++ SemanticCodeGraph.readAllProjectsFullCallGraph()

  private def exportSummary(scg: SemanticCodeGraph, summary: SCGProjectSummary): Unit =
    val fileName = s"${scg.networkType}-${scg.projectAndVersion.projectName}-summary.json"
    println(fileName)
    FileUtils.dumpFile(fileName, write(summary))

  private def fireComputations() =
    val computations: Seq[Future[Unit]] = projects
      .map(_.withoutZeroDegreeNodes())
      .map { scg =>
        Future(exportSummary(scg, SCGProjectSummary.summary(scg))).recover { case e =>
          println(s"${scg.networkType}-${scg.projectAndVersion.projectName} has crashed with ${e.getMessage}")
          e.printStackTrace()
        }
      }
    Await.ready(Future.sequence(computations), Duration.Inf)

  fireComputations()
