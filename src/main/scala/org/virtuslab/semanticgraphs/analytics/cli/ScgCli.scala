package org.virtuslab.semanticgraphs.analytics.cli

import ch.qos.logback.classic.Logger
import com.virtuslab.semanticgraphs.javaparser.JavaParserMain
import com.virtuslab.semanticgraphs.parsercommon.logger.LoggerFactory
import org.virtuslab.semanticgraphs.analytics.partitions.comparison.PartitioningComparisonApp
import org.virtuslab.semanticgraphs.analytics.scg.{NetworkType, ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.utils.{FileUtils, MultiPrinter}
import org.virtuslab.semanticgraphs.analytics.summary.SCGProjectSummary
import org.virtuslab.semanticgraphs.analytics.crucial.CrucialNodes
import org.virtuslab.semanticgraphs.analytics.dto.{EdgeDTO, GraphNodeDTO, LocationDTO}
import org.virtuslab.semanticgraphs.analytics.exporters.{ExportToGdf, ExportToGraphML, JupyterNotebook}
import org.virtuslab.semanticgraphs.analytics.partitions.{PartitionResults, PartitionResultsSummary}
import picocli.CommandLine
import picocli.CommandLine.{Command, HelpCommand, Option, Parameters}
import picocli.CommandLine.Model.CommandSpec

import java.io.{File, PrintWriter}
import java.util.Locale
import upickle.default.*
import upickle.default.{macroRW, ReadWriter as RW}

import java.nio.file.{Files, Path}

@Command(
  name = "scg-cli",
  description = Array("CLI to analyse projects based on SCG data"),
  subcommands = Array(classOf[HelpCommand])
)
class ScgCli:

  @Command(name = "version", description = Array("Show scg-cli version."))
  def version() =
    println("scg-cli 0.1.9-SNAPSHOT")

  @Command(name = "generate", description = Array("Generate SCG metadata."))
  def generate(
    @Parameters(
      paramLabel = "<workspace>",
      description = Array("Workspace where the project is located")
    )
    workspace: String,
    @Option(
      names = Array("-l", "--language"),
      description = Array("Language of the project"),
      arity = "0..1",
      defaultValue = "java"
    )
    language: String,
    @Option(
      names = Array("-wt", "--with-tests"),
      description = Array("Generate SCG files including test files"),
      arity = "0..1",
      defaultValue = "false"
    )
    withTests: Boolean,
  ): Unit =
    println(s"Generating SCG metadata for $workspace")
    JavaParserMain.generateSemanticGraphFiles(workspace, withTests)
    println(s"SCG was generated to $workspace/.semanticgraphs")

  @Command(name = "summary", description = Array("Summarize the project."))
  def summary(
    @Parameters(
      paramLabel = "<workspace>",
      description = Array("Workspace where SCG proto files are located in .semanticgraphs directory or zipped archive")
    )
    workspace: String,
    @Option(
      names = Array("-o", "--output"),
      description = Array("Output format: html, txt, json"),
      arity = "0..1",
      defaultValue = "html"
    )
    output: String,
    @Option(
      names = Array("-g", "--graph"),
      description = Array("Software Network to extract: SCG, CG, CCN default: ${DEFAULT-VALUE}"),
      arity = "0..1",
      defaultValue = "SCG"
    )
    graph: String
  ): Unit =
    val projectAndVersion = ProjectAndVersion(workspace, workspace.split("/").last, "")
    val scg = createScg(projectAndVersion, graph)
    output match {
      case "html" =>
        val summary = SCGProjectSummary.summary(scg)
        SCGProjectSummary.exportHtmlSummary(summary)
      case "txt" =>
        val summary = SCGProjectSummary.summary(scg)
        SCGProjectSummary.exportTxt(summary)
      case "json" =>
        val summary = SCGProjectSummary.summary(scg)
        SCGProjectSummary.exportTxt(summary)
    }

  @Command(name = "crucial", description = Array("Find crucial code entities."))
  def crucial(
    @Parameters(
      paramLabel = "<workspace>",
      description = Array("Workspace where SCG proto files are located in .semanticgraphs directory or zipped archive.")
    )
    workspace: String,
    @Option(
      names = Array("-o", "--output"),
      description = Array("Output format: html, json, txt, tex"),
      arity = "0..1",
      defaultValue = "html"
    )
    output: String,
    @Option(
      names = Array("-m", "--mode"),
      description = Array("Analysis mode: quick, full"),
      arity = "0..1",
      defaultValue = "quick"
    )
    mode: String,
    @Option(
      names = Array("-n", "--number"),
      description = Array("How many top nodes should be returned"),
      arity = "0..1",
      defaultValue = "10"
    )
    n: Int,
    @Option(
      names = Array("-g", "--graph"),
      description = Array("Software Network to extract: SCG, CG, CCN default: ${DEFAULT-VALUE}"),
      arity = "0..1",
      defaultValue = "SCG"
    )
    graph: String
  ): Unit =
    val projectAndVersion = ProjectAndVersion(workspace, workspace.split("/").last, "")
    val scg = createScg(projectAndVersion, graph)
    val summary = CrucialNodes.analyze(scg, mode == "quick", n)
    output match {
      case "html" =>
        CrucialNodes.exportHtmlSummary(summary)
      case "json" =>
        val outputFile = s"${scg.projectName}.crucial.json"
        FileUtils.dumpFile(outputFile, write(summary))
        println(s"Results exported to: $outputFile")
      case "txt" =>
        println(summary.projectName)
        println(summary.workspace)
        summary.stats.foreach { stat =>
          println(s"${stat.id}, ${stat.description}")
          stat.nodes.foreach { node =>
            println(s"${node.id}, ${formatScore(node.score)}")
          }
        }
      case "tex" =>
        println(summary.projectName)
        println(summary.workspace)
        summary.stats.foreach { stat =>
          println(s"\\hline")
          println(s"${stat.description} & Score \\\\")
          println(s"\\hline")
          stat.nodes.foreach { node =>
            println(s"${node.id} & ${formatScore(node.score)} \\\\")
          }
        }
    }

  private def formatScore(value: Double): String =
    if value == value.toInt then value.toInt.toString else f"$value%.3f"

  case class PartitionResult(results: List[PartitionResults])
  object PartitionResult:
    given RW[EdgeDTO] = macroRW
    given RW[LocationDTO] = macroRW
    given RW[GraphNodeDTO] = macroRW
    given RW[PartitionResults] = macroRW
    given RW[PartitionResult] = macroRW

  @Command(name = "partition", description = Array("Suggest project partitioning."))
  def partition(
    @Parameters(
      paramLabel = "<workspace>",
      description = Array("Workspace where SCG proto files are located in .semanticgraphs directory or zipped archive")
    )
    workspace: String,
    @Parameters(paramLabel = "<nparts>", description = Array("Up to how many partitions split the project"))
    nparts: Int,
    @Option(
      names = Array("-o", "--output"),
      description = Array("Output format: html, json, txt, csv, graphml default: ${DEFAULT-VALUE}"),
      arity = "0..1",
      defaultValue = "html"
    )
    output: String,
    @Option(
      names = Array("--use-docker"),
      description = Array("Use partition gpmetis/patoh programs through docker image, default: ${DEFAULT-VALUE}"),
      arity = "0..1",
      defaultValue = "false"
    )
    useDocker: Boolean,
    @Option(
      names = Array("-g", "--graph"),
      description = Array("Software Network to extract: SCG, CG, CCN default: ${DEFAULT-VALUE}"),
      arity = "0..1",
      defaultValue = "SCG"
    )
    graph: String,
    @Option(
      names = Array("-ot", "--objtype"),
      description = Array("Specifies the objective that the partitioning routines will optimize. Default: ${DEFAULT-VALUE}"),
      arity = "0..1",
      defaultValue = "cut"
    )
    objtype: String,
    @Option(
      names = Array("-uf", "--ufactor"),
      description = Array("-ufactor param for gpmetis algorithm; A value of x indicates that the allowed load imbalance is 1+x/1000"),
      arity = "0..1",
      defaultValue = "999"
    )
    ufactor: Int,
    @Option(
      names = Array("-nc", "--ncuts"),
      description = Array("-ncuts param for gpmetis algorithm; Specifies the number of different partitionings that it will compute."),
      arity = "0..1",
      defaultValue = "5"
    )
    ncuts: Int,
    @Option(
      names = Array("-pa", "--initp_alg"),
      description = Array("Determines the initial partitioning algorithm for PaToH. Default = 13"),
      arity = "0..1",
      defaultValue = "13"
    )
    PA: Int,
    @Option(
      names = Array("-ib", "--imb_both"),
      description = Array("IB(f) : Sets both Initial & Final Imbalance ratio. <0.1, 0.99>, Default = 0.9 "),
      arity = "0..1",
      defaultValue = "0.9"
    )
    IB: Double,
    @Option(
      names = Array("--biggest-component", "-bc"),
      description = Array("Partition only biggest weekly connected component for the project, default: ${DEFAULT-VALUE}"),
      arity = "0..1",
      defaultValue = "false"
    )
    biggestComponentOnly: Boolean
  ): Unit =
    val projectAndVersion = ProjectAndVersion(workspace, workspace.split("/").last, "")
    val (scg, results) = PartitioningComparisonApp.runPartitionComparison(
      projectAndVersion,
      NetworkType.fromString(graph),
      nparts,
      useDocker,
      objtype,
      ufactor,
      ncuts,
      PA,
      IB,
      biggestComponentOnly
    )
    output match {
      case "html" =>
        PartitionResultsSummary.exportHtmlSummary(PartitionResultsSummary(results))
      case "json" =>
        val outputFile = s"${projectAndVersion.projectName}.partition.json"
        FileUtils.dumpFile(outputFile, write(PartitionResult(results)))
        println(s"Results exported to: $outputFile")
      case "graphml" =>
        PartitionResults.exportGraphML(scg, results)
      case "txt" =>
        PartitionResults.print(
          new MultiPrinter(
            new PrintWriter(System.out),
            new PrintWriter(new PrintWriter(new File(s"${workspace.replace("/", "-")}.partition.txt")))
          ),
          results
        )
      case "tex" =>
        println(PartitionResultsSummary.exportTex(PartitionResultsSummary(results), results))
      case "csv" =>
        results.foreach { result =>
          val fileName = s"${projectAndVersion.projectName}-npart-${result.nparts}-${result.method}.csv"
          val csvContent =
            "id,npart\n" + result.nodeToPart.map { case (key, value) => s"\"$key\", $value" }.mkString("\n")
          FileUtils.dumpFile(fileName, csvContent)
        }
    }

  @Command(name = "export", description = Array("Export SCG metadata for further analysis"))
  def `export`(
    @Parameters(
      paramLabel = "<workspace>",
      description = Array("Workspace where SCG proto files are located in .semanticgraphs directory or zipped archive")
    )
    workspace: String,
    @Option(
      names = Array("-o", "--output"),
      description = Array("Output format: jupyter, gdf, graphml default: ${DEFAULT-VALUE}"),
      arity = "0..1",
      defaultValue = "jupyter"
    )
    output: String,
    @Option(
      names = Array("-g", "--graph"),
      description = Array("Software Network to extract: SCG, CG, CCN default: ${DEFAULT-VALUE}"),
      arity = "0..1",
      defaultValue = "SCG"
    )
    graph: String
  ): Unit =
    val projectName = workspace.split("/").last
    val projectAndVersion = ProjectAndVersion(workspace, projectName, "")

    output match {
      case "jupyter" =>
        JupyterNotebook.runJupyterNotebook(projectAndVersion.workspace)
      case "graphml" =>
        ExportToGraphML.exportToGraphML(s"$projectName.graphml", createScg(projectAndVersion, graph))
      case "gdf" =>
        ExportToGdf.exportToGdf(s"$projectName.gdf", createScg(projectAndVersion, graph))
        println(s"Exported to `$projectName.gdf` file.")
    }

  private def createScg(projectAndVersion: ProjectAndVersion, graph: String) = graph match {
    case "CCN" => SemanticCodeGraph.fetchClassCollaborationGraph(projectAndVersion)
    case "CG"  => SemanticCodeGraph.fetchFullCallGraph(projectAndVersion)
    case _     => SemanticCodeGraph.read(projectAndVersion)
  }

object ScgCli:

  def main(args: Array[String]): Unit =
    val exitCode = new CommandLine(new ScgCli).execute(args: _*)
    System.exit(exitCode)
