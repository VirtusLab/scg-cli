package org.virtuslab.semanticgraphs.analytics.print

import org.virtuslab.semanticgraphs.analytics.crucial.{CrucialNodesSummary, MetricIdAndDescription, Statistic}
import org.virtuslab.semanticgraphs.analytics.scg.SemanticCodeGraph

import java.nio.file.{Files, Path}
import scala.io.Source
import scala.jdk.CollectionConverters.IteratorHasAsScala
import upickle.default.*

object LatexPrinter:

  def toTexTable(scores: List[CrucialNodesSummary], metrics: List[MetricIdAndDescription]): String =
    val stringBuilder = new StringBuilder()
    stringBuilder.addAll("\\hdashline\n")
    scores.foreach { stats =>
      val topStats = metrics.map(id => stats.stats.find(_.id == id.id).head).map { s =>
        val topNode = s.nodes.head
        if topNode.score == topNode.score.toLong then f"${topNode.label} & ${topNode.score}%.0f"
        else f"${topNode.label} & ${topNode.score}%.4f"
      }
      stringBuilder.addAll(s"${stats.projectName} & ${topStats.mkString("", " & ", " \\\\")} \n")
    }
    stringBuilder.toString()

  def topN(scores: List[CrucialNodesSummary], metric: MetricIdAndDescription, n: Int): String =
    val stringBuilder = new StringBuilder()
    stringBuilder.addAll("\\hdashline\n")
    scores.foreach { stats =>
      val topStats = stats.stats.find(_.id == metric.id).toList.flatMap { s =>
        s.nodes.take(n).map { topNode =>
          if topNode.score == topNode.score.toLong then f"${topNode.label} & ${topNode.score}%.0f"
          else f"${topNode.label} & ${topNode.score}"
        }
      }
      stringBuilder.addAll(s"${stats.projectName} & ${topStats.mkString("", " & ", " \\\\")} \n")
    }
    stringBuilder.toString()

  def tableHeader(headers: List[String]): String =
    headers.mkString("Name & ", " & \\# & ", " & \\# \\\\")

object LatexCrucialNodesPrinterApp extends App:

  def readScores(filePrefix: String): List[CrucialNodesSummary] =
    Files
      .list(Path.of("./analysis"))
      .iterator()
      .asScala
      .filter(x => Files.isRegularFile(x) && x.getFileName.toString.startsWith(filePrefix))
      .map { file =>
        val fileContent = Source.fromFile(file.toUri).getLines().mkString
        read[CrucialNodesSummary](fileContent)
      }
      .toList
      .sortBy(summary => SemanticCodeGraph.allProjects.map(_.projectName).indexOf(summary.projectName))

  def printWholeTable(
    scgScores: List[CrucialNodesSummary],
    callScores: List[CrucialNodesSummary],
    fullCallScores: List[CrucialNodesSummary],
    metrics: List[MetricIdAndDescription]
  ) =
    // println(s"\\begin{tabular}{${"r|".repeat(metrics.size * 2 + 1)}}")
    println("\\hline")
    println(LatexPrinter.tableHeader(metrics.map(_.desc)))
    println("\\hline")
    println(s"\\multicolumn{${metrics.size * 2 + 1}}{l|}{Semantic Code Graph} \\\\")
    println(LatexPrinter.toTexTable(scgScores, metrics))
//    println("\\hline")
//    println(s"\\multicolumn{${metrics.size * 2 + 1}}{l|}{Call Graph}\\\\")
//    println(LatexPrinter.toTexTable(callScores, metrics))
    println("\\hline")
    println(s"\\multicolumn{${metrics.size * 2 + 1}}{l|}{SCG based Call Graph}\\\\")
    println(LatexPrinter.toTexTable(fullCallScores, metrics))
    // println(s"\\end{tabular}")
    println()

  println(Path.of("analysis").toAbsolutePath.toString)
  val scgScores = readScores("scg")
  val callScores = readScores("call")
  val fullCallScores = readScores("full-call")

  val general = List(Statistic.loc, Statistic.outDegree, Statistic.inDegree)
  printWholeTable(scgScores, callScores, fullCallScores, general)

  val influenceBased = List(Statistic.eigenvector, Statistic.katz, Statistic.pageRank)
  printWholeTable(scgScores, callScores, fullCallScores, influenceBased)

  val distanceBased = List(Statistic.betweenness, Statistic.harmonic, Statistic.combined)
  printWholeTable(scgScores, callScores, fullCallScores, distanceBased)

object LatexCrucialNodesCombinedPrinterApp extends App:

  def readScores(filePrefix: String): List[CrucialNodesSummary] =
    Files
      .list(Path.of("./analysis"))
      .iterator()
      .asScala
      .filter(x => Files.isRegularFile(x) && x.getFileName.toString.startsWith(filePrefix))
      .map { file =>
        val fileContent = Source.fromFile(file.toUri).getLines().mkString
        read[CrucialNodesSummary](fileContent)
      }
      .toList
      .sortBy(summary => SemanticCodeGraph.allProjects.map(_.projectName).indexOf(summary.projectName))

  def printWholeTable(
    scgScores: List[CrucialNodesSummary],
    callScores: List[CrucialNodesSummary],
    fullCallScore: List[CrucialNodesSummary],
    n: Int
  ) =
    // println(s"\\begin{tabular}{${"r|".repeat(metrics.size * 2 + 1)}}")
    println("\\hline")
    println(LatexPrinter.tableHeader((1 to n).map(i => s"\\#$i").toList))
    println("\\hline")
    println(s"\\multicolumn{${n * 2 + 1}}{l|}{Semantic Code Graph} \\\\")
    println(LatexPrinter.topN(scgScores, Statistic.combined, n))
//    println("\\hline")
//    println(s"\\multicolumn{${n * 2 + 1}}{l|}{Call Graph}\\\\")
//    println(LatexPrinter.topN(callScores, "combined", n))
    println("\\hline")
    println(s"\\multicolumn{${n * 2 + 1}}{l|}{SCG based Call Graph}\\\\")
    println(LatexPrinter.topN(fullCallScore, Statistic.combined, n))
    // println(s"\\end{tabular}")
    println()

  println(Path.of("analysis").toAbsolutePath.toString)
  val scgScores = readScores("scg")
  val callScores = readScores("call")
  val fullCallScores = readScores("full-call")

  printWholeTable(scgScores, callScores, fullCallScores, 3)
