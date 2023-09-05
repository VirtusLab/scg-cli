package org.virtuslab.semanticgraphs.analytics.crucial

import org.jgrapht.alg.scoring.*
import org.virtuslab.semanticgraphs.analytics.metrics.JGraphTMetrics
import org.virtuslab.semanticgraphs.analytics.scg.ScgJGraphT.LabeledEdge
import org.virtuslab.semanticgraphs.analytics.scg.{ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.summary.SCGProjectSummary
import org.virtuslab.semanticgraphs.analytics.summary.SCGProjectSummary.getClass
import org.virtuslab.semanticgraphs.analytics.utils.FileUtils

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

  def analyze(semanticCodeGraph: SemanticCodeGraph, quick: Boolean, n: Int): CrucialNodesSummary =
    val jGraphTExporter = new JGraphTAnalyzer(semanticCodeGraph)
    jGraphTExporter.computeStatistics(
      semanticCodeGraph.projectName,
      semanticCodeGraph.projectAndVersion.workspace,
      quick,
      n
    )

  def analyze(semanticCodeGraph: SemanticCodeGraph, filePrefix: String, n: Int): CrucialNodesSummary =
    val jGraphTExporter = new JGraphTAnalyzer(semanticCodeGraph)
    val stats =
      jGraphTExporter.computeStatistics(
        semanticCodeGraph.projectName,
        semanticCodeGraph.projectAndVersion.workspace,
        false,
        n
      )
    val outputFile = s"$filePrefix-stats-${semanticCodeGraph.projectName}.crucial.json"
    FileUtils.dumpFile(outputFile, write(stats))
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

  def computeStatistics(projectName: String, workspace: String, quick: Boolean, n: Int): CrucialNodesSummary =
    val statistics = List(
      computeStats(
        id = Statistic.loc,
        desc = "Lines of code",
        nodes.values
          .map(node =>
            (
              node.id,
              node.properties.get("LOC").map(_.toInt).getOrElse(0).toDouble
            )
          ),
        take = n
      ),
      computeStats(
        id = Statistic.outDegree,
        desc = "Node out-degree",
        graph
          .vertexSet()
          .asScala
          .toList
          .map(v => (v, graph.outDegreeOf(v).toDouble)),
        take = n
      ),
      computeStats(
        id = Statistic.inDegree,
        desc = "Node in-degree",
        graph
          .vertexSet()
          .asScala
          .toList
          .map(v => (v, graph.inDegreeOf(v).toDouble)),
        take = n
      ),
      computeStats(
        id = Statistic.pageRank,
        desc = "PageRank",
        new PageRank(graph, 0.99, 1000, 0.000001).getScores.asScala,
        take = n
      ),
      computeStats(
        id = Statistic.eigenvector,
        desc = "Eigenvector centrality",
        new EigenvectorCentrality(
          graph,
          EigenvectorCentrality.MAX_ITERATIONS_DEFAULT,
          EigenvectorCentrality.TOLERANCE_DEFAULT
        ).getScores.asScala,
        take = n
      ),
      computeStats(
        id = Statistic.katz,
        desc = "Katz centrality",
        new KatzCentrality(graph).getScores.asScala,
        take = n
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
            desc = "Betweenness centrality",
            JGraphTMetrics.betweennessCentrality(graph).asScala,
            take = n
          ),
          computeStats(
            id = Statistic.harmonic,
            desc = "Harmonic centrality",
            new HarmonicCentrality[String, LabeledEdge](graph).getScores.asScala,
            take = n
          )
        )
      else Nil

    val allStatistics = statistics ++ computeIntensive
    CrucialNodesSummary(
      projectName,
      workspace,
      allStatistics :+ computeCombinedMetrics(allStatistics, n)
    )

  private def computeCombinedMetrics(stats: List[Statistic], n: Int): Statistic =
    val list = stats.flatMap(_.nodes)
    val a = list.groupBy(node => (node.id, node.label)).mapValues(_.size)
    Statistic(
      id = Statistic.combined.id,
      description = "Combined importance",
      a.toList
        .sortBy { case (_, size) => -size }
        .map { case ((id, label), score) =>
          NodeScore(id, label, score)
        }
        .take(2 * n)
    )
