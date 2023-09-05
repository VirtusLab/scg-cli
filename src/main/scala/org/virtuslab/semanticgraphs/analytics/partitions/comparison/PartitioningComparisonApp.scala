package org.virtuslab.semanticgraphs.analytics.partitions.comparison

import com.virtuslab.semanticgraphs.proto.model.graphnode.{GraphNode, Location}
import org.virtuslab.semanticgraphs.analytics.partitions.gpmetis.GpmetisPartitions
import org.virtuslab.semanticgraphs.analytics.partitions.patoh.PatohPartitions
import org.virtuslab.semanticgraphs.analytics.partitions.{PartitionHelpers, PartitionResults}
import org.virtuslab.semanticgraphs.analytics.scg.{ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.utils.MultiPrinter
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO.toGraphNodeDto

object PartitioningComparisonApp:

  def runPartitionComparison(
    projectAndVersion: ProjectAndVersion,
    nparts: Int,
    useDocker: Boolean
  ): (SemanticCodeGraph, List[PartitionResults]) =
    val projectName = projectAndVersion.projectName
    val scg = SemanticCodeGraph.readOnlyGlobalNodes(projectAndVersion)

    val biggestComponentNodes =
      PartitionHelpers
        .takeBiggestComponentOnly(scg)
        .map(_.toGraphNodeDto)

    val gpmetisResults = GpmetisPartitions.partition(biggestComponentNodes, projectName, nparts, useDocker)
    val patohResults = PatohPartitions.partition(biggestComponentNodes, projectName, nparts, useDocker)

    scg -> (gpmetisResults ::: patohResults).sortBy(x => (x.nparts, x.method))
