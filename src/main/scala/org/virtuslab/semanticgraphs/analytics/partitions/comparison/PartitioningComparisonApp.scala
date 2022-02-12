package org.virtuslab.semanticgraphs.analytics.partitions.comparison

import com.virtuslab.semanticgraphs.proto.model.graphnode.{GraphNode, Location}
import org.virtuslab.semanticgraphs.analytics.partitions.gpmetis.GpmetisPartitions
import org.virtuslab.semanticgraphs.analytics.partitions.patoh.PatohPartitions
import org.virtuslab.semanticgraphs.analytics.partitions.{
  PartitionHelpers,
  PartitionResults
}
import org.virtuslab.semanticgraphs.analytics.scg.{ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.utils.MultiPrinter
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO.toGraphNodeDto

object PartitioningComparisonApp:

  def runAndPrinterPartitionComparison(projectAndVersion: ProjectAndVersion, nparts: Int) =
    val projectName = projectAndVersion.projectName
    val (comparisonTmp, comparisonPrinter) = PartitionHelpers.multiPrinter(projectName, "comparison")
    comparisonPrinter.println(s"Computing graph ${projectAndVersion.workspace}, $nparts with project name $projectName")
    val results = runPartitionComparison(projectAndVersion, nparts, false)
    PartitionResults.print(comparisonPrinter, results)

  def runPartitionComparison(
    projectAndVersion: ProjectAndVersion,
    nparts: Int,
    useDocker: Boolean
  ): List[PartitionResults] =
    val projectName = projectAndVersion.projectName

    val biggestComponentNodes =
      PartitionHelpers
        .takeBiggestComponentOnly(SemanticCodeGraph.readOnlyGlobalNodes(projectAndVersion))
        .map(_.toGraphNodeDto)

    val gpmetisResults = GpmetisPartitions.partition(biggestComponentNodes, projectName, nparts, useDocker)
    val patohResults = PatohPartitions.partition(biggestComponentNodes, projectName, nparts, useDocker)

    (gpmetisResults ::: patohResults).sortBy(x => (x.nparts, x.method))
