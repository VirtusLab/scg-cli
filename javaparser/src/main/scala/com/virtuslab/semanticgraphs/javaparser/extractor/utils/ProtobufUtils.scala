package com.virtuslab.semanticgraphs.javaparser.extractor.utils

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.proto.model.graphnode.{Edge, GraphNode, Location, SemanticGraphFile}

extension (location: Location) {
  def coordinatesString: String =
    s"${location.startLine}.${location.startCharacter}:${location.endLine}.${location.endCharacter}"
}

extension (graphNode: GraphNode) {
  def isEnumConstant: Boolean = graphNode.properties.get("enum").contains(true.toString)
  def getCallEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.CALL)
  def getDeclarationEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.DECLARATION)
  def getExtendEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.EXTEND)
  def getExtendTypeArgumentEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.EXTEND_TYPE_ARGUMENT)
  def getParameterEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.PARAMETER)
  def getReturnTypeEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.RETURN_TYPE)
  def getReturnTypeArgumentEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.RETURN_TYPE_ARGUMENT)
  def getTypeEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.TYPE)
  def getTypeArgumentEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.TYPE_ARGUMENT)
  def getTypeParameterEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.TYPE_PARAMETER)
  def getSuperEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.SUPER)
  def getOverrideEdges: Seq[Edge] = graphNode.getEdgesOfKind(EdgeKind.OVERRIDE)
  def getEdgesOfKind(edgeKind: EdgeKind): Seq[Edge] = graphNode.edges.filter(_.`type` == edgeKind.toString)
}

extension (file: SemanticGraphFile) {
  def getClasses: Seq[GraphNode] = file.getNodesOfKind(NodeKind.CLASS)
  def getConstructors: Seq[GraphNode] = file.getNodesOfKind(NodeKind.CONSTRUCTOR)
  def getMethods: Seq[GraphNode] = file.getNodesOfKind(NodeKind.METHOD)
  def getTraits: Seq[GraphNode] = file.getNodesOfKind(NodeKind.TRAIT)
  def getVariables: Seq[GraphNode] = file.getNodesOfKind(NodeKind.VARIABLE)
  def getParameters: Seq[GraphNode] = file.getNodesOfKind(NodeKind.PARAMETER)
  def getValues: Seq[GraphNode] = file.getNodesOfKind(NodeKind.VALUE)
  def getTypeParameters: Seq[GraphNode] = file.getNodesOfKind(NodeKind.TYPE_PARAMETER)
  def getEnums: Seq[GraphNode] = file.getNodesOfKind(NodeKind.ENUM)
  def getEnumConstants: Seq[GraphNode] = file.nodes.filter(_.isEnumConstant)
  def getNodesOfKind(nodeKind: NodeKind): Seq[GraphNode] = file.nodes.filter(_.kind == nodeKind.toString)
}
