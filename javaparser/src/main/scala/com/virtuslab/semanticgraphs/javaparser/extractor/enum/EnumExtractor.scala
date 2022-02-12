package com.virtuslab.semanticgraphs.javaparser.extractor.`enum`

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.javaparser.extractor.generics.GenericsExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.method.MethodLikeExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.javaparser.extractor.value.VariableExtractor
import com.virtuslab.semanticgraphs.parsercommon.logger.GraphBuddyLogging
import com.virtuslab.semanticgraphs.proto.model.graphnode.{Edge, GraphNode, Location}
import com.github.javaparser.ast.body.{
  CallableDeclaration,
  ClassOrInterfaceDeclaration,
  ConstructorDeclaration,
  MethodDeclaration
}
import com.github.javaparser.ast.body.EnumDeclaration

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.util.Try

object EnumExtractor extends GraphBuddyLogging:

  def createNodes(enums: Iterable[EnumDeclaration], uri: String): Iterable[GraphNode] = enums.flatMap(e => {
    e.getQualifiedSignature.toSeq
      .map(signature => {
        GraphNode(
          id = signature,
          kind = e.kind,
          location = e.simpleNameLocation(uri),
          properties = PackageExtractor.getPackage(e.getFullyQualifiedName),
          displayName = e.getNameAsString,
          edges = e.declarationEdges(uri) ++ e.extendEdges(uri)
        )
      }) ++ EnumConstantExtractor.createNodes(e.enumConstants, uri) ++
      MethodLikeExtractor.createNodes(e.methodLikeDeclarations, uri) ++
      VariableExtractor.createNodes(e.variableDeclarations, uri, isLocal = false)
  })
