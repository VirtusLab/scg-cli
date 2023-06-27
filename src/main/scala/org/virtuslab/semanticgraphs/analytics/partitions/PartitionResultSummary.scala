package org.virtuslab.semanticgraphs.analytics.partitions

import org.virtuslab.semanticgraphs.analytics.crucial.CrucialNodes.getClass
import org.virtuslab.semanticgraphs.analytics.crucial.CrucialNodesSummary
import org.virtuslab.semanticgraphs.analytics.exporters.ExportToGml
import org.virtuslab.semanticgraphs.analytics.scg.{ProjectAndVersion, SemanticCodeGraph}
import upickle.default.*

import java.nio.file.{Files, Path, StandardCopyOption}

//|Method |NPart|Modularity|Coefficient|Weighted|Accuracy|Weighted|Accuracy|Variance|Distribution
case class AverageAccuracy(weighted: Int, standard: Int) derives ReadWriter
case class ShortSummary(
  method: String,
  npart: Int,
  modularity: Double,
  coefficient: Double,
  file: AverageAccuracy,
  `package`: AverageAccuracy,
  variance: Double,
  distribution: String
) derives ReadWriter

case class PartitionPackageSummary(method: String, part: Int, accuracy: Int, `package`: String, distribution: String)
  derives ReadWriter
case class PartitionFileSummary(method: String, part: Int, accuracy: Int, file: String, distribution: String)
  derives ReadWriter

case class PartitionResultsSummary(
  nparts: Int,
  byPackage: List[PartitionPackageSummary],
  byFile: List[PartitionFileSummary]
) derives ReadWriter

case class PartitionResultsWrapper(results: List[PartitionResultsSummary], summary: List[ShortSummary])
  derives ReadWriter

object PartitionResultsSummary:

  def apply(partitionResults: List[PartitionResults]): PartitionResultsWrapper = PartitionResultsWrapper(
    results = partitionResults.map { result =>
      val byPackage =
        for partitionResults <- result.packageDistribution.groupPartitionStats
        yield PartitionPackageSummary(
          result.method,
          partitionResults.partition,
          partitionResults.accuracy,
          partitionResults.partitionName,
          partitionResults.distribution.mkString("[", ",", "]")
        )
      val byFile =
        for partitionResults <- result.fileDistribution.groupPartitionStats
        yield PartitionFileSummary(
          result.method,
          partitionResults.partition,
          partitionResults.accuracy,
          partitionResults.partitionName,
          partitionResults.distribution.mkString("[", ",", "]")
        )
      PartitionResultsSummary(result.nparts, byPackage, byFile)
    },
    summary = partitionResults.map { result =>
      ShortSummary(
        result.method,
        result.nparts,
        result.modularityRatio,
        result.clusteringCoefficient,
        AverageAccuracy(
          result.fileDistribution.weightedAverageAccuracy,
          result.fileDistribution.arithmeticAverageAccuracy
        ),
        AverageAccuracy(
          result.packageDistribution.weightedAverageAccuracy,
          result.packageDistribution.arithmeticAverageAccuracy
        ),
        result.distributionVariance,
        result.globalNodesDistribution.map(i => i * 100 / result.nodes.size).mkString("[", ",", "]%")
      )
    }
  )

  def exportHtmlSummary(summary: PartitionResultsWrapper): Unit = {
    exportJsSummary("partition.js", summary)
    copySummaryHtml(Path.of("."))
  }

  private def exportJsSummary(fileName: String, summary: PartitionResultsWrapper): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(fileName))
    val json = s"const partition = ${write(summary)};"
    pw.write(json)
    pw.close()
  }

  private def copySummaryHtml(summaryResultDirectory: Path): Unit = {
    val inputStream = getClass.getClassLoader.getResourceAsStream("partition.html")
    Files.copy(inputStream, summaryResultDirectory.resolve("partition.html"), StandardCopyOption.REPLACE_EXISTING)
    inputStream.close()
  }

  def exportTex(summary: PartitionResultsWrapper): String = {

    extension (number: Double) def to3: String = String.format("%.3f", number)

    val builder = new StringBuilder()
    builder.append(
      "\\begin{tabular}{|r|r|r|r|r|r|r|r|r|l|}  \n"
    )
    builder.append("\\hline \n")
    builder.append(
      "Method &  NPart & Modularity & ACC & File W. & File A. & Package W. & Package A. & Variance & Distribution \\\\ \n"
    )
    builder.append("\\hline \n")
    summary.summary.foreach { shortSummary =>
      import shortSummary._
      builder.append(
        s"$method & $npart & ${modularity.to3} & ${coefficient.to3} & ${file.weighted}\\% & ${file.standard}\\% & ${`package`.weighted}\\% & ${`package`.standard}\\% & ${variance.to3} & ${distribution
            .replace("%", "\\%")} \\\\ \n"
      )
    }
    builder.append("\\hline \n")
    builder.append("\\end{tabular} \n")

    builder.toString()
  }
