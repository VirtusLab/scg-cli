package org.virtuslab.semanticgraphs.analytics.crucial

import org.jgrapht.alg.scoring.*
import org.virtuslab.semanticgraphs.analytics.metrics.JGraphTMetrics
import org.virtuslab.semanticgraphs.analytics.scg.ScgJGraphT.LabeledEdge
import org.virtuslab.semanticgraphs.analytics.scg.{ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.summary.SCGProjectSummary
import org.virtuslab.semanticgraphs.analytics.summary.SCGProjectSummary.getClass
import org.virtuslab.semanticgraphs.analytics.utils.JsonUtils

import upickle.default.*

import java.lang
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.concurrent.Executors
import scala.jdk.CollectionConverters.{CollectionHasAsScala, MapHasAsScala}

case class NodeScore(id: String, label: String, score: Double) derives ReadWriter
case class Statistic(id: String, description: String, nodes: List[NodeScore]) derives ReadWriter

case class MetricIdAndDescription(id: String, desc: String) derives ReadWriter:
  override def toString(): String = desc

object Statistic:
  val loc = MetricIdAndDescription("loc", "Lines of Code")
  val outDegree = MetricIdAndDescription("out-degree", "Outgoing Degree")
  val inDegree = MetricIdAndDescription("in-degree", "Incoming Degree")
  val pageRank = MetricIdAndDescription("pagerank", "Page Rank")
  val eigenvector = MetricIdAndDescription("eigenvector", "Eigenvector Centrality")
  val katz = MetricIdAndDescription("Katz", "Katz Centrality")
  val betweenness = MetricIdAndDescription("betweenness", "Betweenness Centrality")
  val harmonic = MetricIdAndDescription("harmonic", "Harmonic Centrality")
  val combined = MetricIdAndDescription("combined", "All metrics combined")
case class CrucialNodesSummary(
  projectName: String,
  workspace: String,
  stats: List[Statistic]
) derives ReadWriter

object CrucialNodes:

  def analyze(semanticCodeGraph: SemanticCodeGraph, quick: Boolean): CrucialNodesSummary =
    val jGraphTExporter = new JGraphTAnalyzer(semanticCodeGraph)
    jGraphTExporter.computeStatistics(
      semanticCodeGraph.projectName,
      semanticCodeGraph.projectAndVersion.workspace,
      quick
    )

  def analyze(semanticCodeGraph: SemanticCodeGraph, filePrefix: String): CrucialNodesSummary =
    val jGraphTExporter = new JGraphTAnalyzer(semanticCodeGraph)
    val stats =
      jGraphTExporter.computeStatistics(
        semanticCodeGraph.projectName,
        semanticCodeGraph.projectAndVersion.workspace,
        false
      )
    val outputFile = s"$filePrefix-stats-${semanticCodeGraph.projectName}.crucial.json"
    JsonUtils.dumpJsonFile(outputFile, write(stats))
    println(s"Results exported to: $outputFile")
    stats

  def exportHtmlSummary(summary: CrucialNodesSummary): Unit = {
    exportJsSummary("partition.js", summary)
    copySummaryHtml(Path.of("."))
  }

  private def exportJsSummary(fileName: String, summary: CrucialNodesSummary): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(fileName))
    val json = s"const crucial = ${write(summary)};"
    pw.write(json)
    pw.close()
  }

  private def copySummaryHtml(summaryResultDirectory: Path): Unit = {
    val inputStream = getClass.getClassLoader.getResourceAsStream("crucial.html")
    Files.copy(inputStream, summaryResultDirectory.resolve("crucial.html"), StandardCopyOption.REPLACE_EXISTING)
    inputStream.close()
  }

object CrucialNodesApp extends App:
  val workspace = args(0)
  val projectName = workspace.split("/").last
  val scg = SemanticCodeGraph.read(ProjectAndVersion(workspace, workspace.split("/").last, ""))
  CrucialNodes.analyze(scg, projectName)

object CrucialNodesAnalyzeAll extends App:
  def analyzeAll() =
    SemanticCodeGraph.readAllProjects().foreach { scg =>
      CrucialNodes.analyze(scg, "all"); println(); Thread.sleep(100)
    }

  analyzeAll()

object CrucialNodesAnalyzeAllZipped extends App:
  def analyzeAll() =
    SemanticCodeGraph.readAllProjects().foreach { scg =>
      CrucialNodes.analyze(scg, "all"); println(); Thread.sleep(100)
    }

  analyzeAll()

class JGraphTAnalyzer(semanticCodeGraph: SemanticCodeGraph):

  val graph = semanticCodeGraph.graph
  val nodes = semanticCodeGraph.nodesMap

  def pickTopN[T](k: Int, iterable: Iterable[T])(implicit ord: Ordering[T]): List[T] =
    val q = collection.mutable.PriorityQueue[T]()
    iterable.foreach { elem =>
      q.enqueue(elem)
      if q.size > k then q.dequeue()
    }
    q.toList.sorted

  def computeStats(
    id: MetricIdAndDescription,
    desc: String,
    stats: Iterable[(String, java.lang.Double)],
    take: Int = 10
  ): Statistic =
    println(s"Computed: $desc")

    val statistics = pickTopN(
      take,
      stats
    )((x: (String, lang.Double), y: (String, lang.Double)) => -x._2.compareTo(y._2))

    Statistic(
      id = id.id,
      description = desc,
      statistics.collect { case (nodeId, score) =>
        val node = nodes(nodeId)
        NodeScore(nodeId, node.displayName, score)
      }
    )

  def computeStatistics(projectName: String, workspace: String, quick: Boolean): CrucialNodesSummary =
    println(
      s"Nodes size: ${graph.vertexSet().size()}, Edges ${graph.edgeSet().size()}"
    )

    val statistics = List(
      computeStats(
        id = Statistic.loc,
        desc = "Top by node LOC size",
        nodes.values
          .map(node =>
            (
              node.id,
              node.properties.get("LOC").map(_.toInt).getOrElse(0).toDouble
            )
          )
      ),
      computeStats(
        id = Statistic.outDegree,
        desc = "Top by node degree outgoing",
        graph
          .vertexSet()
          .asScala
          .toList
          .map(v => (v, graph.outDegreeOf(v).toDouble))
      ),
      computeStats(
        id = Statistic.inDegree,
        desc = "Top by node degree incoming",
        graph
          .vertexSet()
          .asScala
          .toList
          .map(v => (v, graph.inDegreeOf(v).toDouble))
      ),
      computeStats(
        id = Statistic.pageRank,
        desc = "PageRank - how influential the node is",
        new PageRank(graph, 0.99, 1000, 0.000001).getScores.asScala
      ),
      computeStats(
        id = Statistic.eigenvector,
        desc = "eigenvector Centrality",
        new EigenvectorCentrality(
          graph,
          EigenvectorCentrality.MAX_ITERATIONS_DEFAULT,
          EigenvectorCentrality.TOLERANCE_DEFAULT
        ).getScores.asScala
      ),
      computeStats(
        id = Statistic.katz,
        desc = "Katz Centrality - how influential the node is",
        new KatzCentrality(graph).getScores.asScala
      )
//      computeStats(
//        id = "codesmell",
//        desc = "Code smell - methods with too many local declarations",
//        nodes.values
//          .filter(_.kind == "METHOD")
//          .toList
//          .map(node => (node.id, node.edges.count(_.`type` == "DECLARATION").toDouble))
//      ),
//      computeStats(
//        id = "clustering_coefficient",
//        desc = "Top clustering coefficient",
//        new ClusteringCoefficient[String, LabeledEdge](graph).getScores.asScala
//      ),
    )

    val computeIntensive =
      if !quick then
        List(
          computeStats(
            id = Statistic.betweenness,
            desc = "Betweenness Centrality",
            JGraphTMetrics.betweennessCentrality(graph).asScala
          ),
          computeStats(
            id = Statistic.harmonic,
            desc = "Top harmonic centrality",
            new HarmonicCentrality[String, LabeledEdge](graph).getScores.asScala
          )
        )
      else Nil

    val allStatistics = statistics ++ computeIntensive
    CrucialNodesSummary(
      projectName,
      workspace,
      allStatistics :+ computeCombinedMetrics(allStatistics)
    )

  def computeCombinedMetrics(stats: List[Statistic]): Statistic =
    val list = stats.flatMap(_.nodes)
    val a = list.groupBy(node => (node.id, node.label)).mapValues(_.size)
    Statistic(
      id = Statistic.combined.id,
      description = "Crucial code elements, combined",
      a.toList.sortBy { case (_, size) => -size }.map { case ((id, label), score) =>
        NodeScore(id, label, score)
      }
    )
