package org.virtuslab.semanticgraphs.analytics.partitions.comparison

import com.virtuslab.semanticgraphs.proto.model.graphnode.{GraphNode, Location}
import org.virtuslab.semanticgraphs.analytics.partitions.gpmetis.GpmetisPartitions
import org.virtuslab.semanticgraphs.analytics.partitions.patoh.PatohPartitions
import org.virtuslab.semanticgraphs.analytics.partitions.{PartitionHelpers, PartitionResults}
import org.virtuslab.semanticgraphs.analytics.scg.{NetworkType, ProjectAndVersion, SemanticCodeGraph}
import org.virtuslab.semanticgraphs.analytics.utils.MultiPrinter
import org.virtuslab.semanticgraphs.analytics.dto.GraphNodeDTO.toGraphNodeDto

object PartitioningComparisonApp:

  def runPartitionComparison(
    projectAndVersion: ProjectAndVersion,
    networkType: NetworkType,
    nparts: Int,
    useDocker: Boolean,
    objtype: String,
    ufactor: Int,
    ncuts: Int,
    PA: Int,
    IB: Double,
    biggestComponentOnly: Boolean
  ): (SemanticCodeGraph, List[PartitionResults]) =
    val projectName = projectAndVersion.projectName
    val scg = networkType match
      case NetworkType.SCG =>
        SemanticCodeGraph.readOnlyGlobalNodes(projectAndVersion)
      case NetworkType.CCN =>
        SemanticCodeGraph.fetchClassCollaborationGraph(projectAndVersion)
      case NetworkType.CG =>
        SemanticCodeGraph.fetchFullCallGraph(projectAndVersion)

    val allNodes = scg.nodes.toList

    val allNodesDto =
      if biggestComponentOnly then
        PartitionHelpers
          .takeBiggestComponentOnly(scg)
          .map(_.toGraphNodeDto)
      else
        allNodes
          .map(_.toGraphNodeDto)

    val gpmetisResults = GpmetisPartitions.partition(allNodesDto, projectName, nparts, useDocker, objtype, ufactor, ncuts)
    val patohResults = PatohPartitions.partition(allNodesDto, projectName, nparts, useDocker, PA, IB)

    SemanticCodeGraph(scg.projectAndVersion, allNodes.map(x => (x.id, x)).toMap, networkType) -> (gpmetisResults ::: patohResults).sortBy(x => (x.nparts, x.method))
