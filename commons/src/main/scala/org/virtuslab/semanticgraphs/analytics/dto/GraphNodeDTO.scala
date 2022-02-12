package org.virtuslab.semanticgraphs.analytics.dto

import com.virtuslab.semanticgraphs.proto.model.graphnode.{GraphNode, Location}

object GraphNodeDTO:
  extension (graphNode: GraphNode)
    def toGraphNodeDto: GraphNodeDTO =
      GraphNodeDTO(
        graphNode.id,
        graphNode.kind,
        graphNode.displayName,
        graphNode.properties.get("package"),
        graphNode.location.map(toLocationDTO),
        graphNode.edges.map(e => EdgeDTO(e.to, e.`type`, e.location.map(toLocationDTO))),
        graphNode.properties.get("LOC").map(_.toInt)
      )

    private def toLocationDTO(l: Location): LocationDTO =
      LocationDTO(l.uri, l.startLine, l.startCharacter, l.endLine, l.endCharacter)
  end extension

case class EdgeDTO(
  to: String,
  `type`: String,
  location: Option[LocationDTO]
)
case class LocationDTO(
  uri: String,
  startLine: Int,
  startCharacter: Int,
  endLine: Int,
  endCharacter: Int
)
case class GraphNodeDTO(
  id: String,
  kind: String,
  displayName: String,
  `package`: Option[String],
  location: Option[LocationDTO],
  edges: Seq[EdgeDTO],
  loc: Option[Int]
)
