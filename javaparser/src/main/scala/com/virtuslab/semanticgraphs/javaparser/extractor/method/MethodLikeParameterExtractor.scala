package com.virtuslab.semanticgraphs.javaparser.extractor.method

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.parsercommon.logger.GraphBuddyLogging
import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode

import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.expr.VariableDeclarationExpr
import com.github.javaparser.resolution.declarations.ResolvedMethodLikeDeclaration
import com.github.javaparser.resolution.Resolvable

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.util.Try

object MethodLikeParameterExtractor extends GraphBuddyLogging {

  def createNodes(methodLike: MethodLikeDeclaration, uri: String): Seq[GraphNode] =
    methodLike.allParameters
      .map(parameter => {
        GraphNode(
          id = parameter.getQualifiedSignature.getOrElse(parameter.getNameAsString),
          kind = parameter.kind,
          location = parameter.simpleNameLocation(uri),
          properties = createProperties(parameter),
          displayName = parameter.getNameAsString,
          edges = parameter.typeEdge(uri).toSeq ++
            parameter.typeArgumentEdges(uri) ++
            parameter.callEdges(uri)
        )
      })

  private def createProperties(parameter: Parameter): Map[String, String] = {
    Map(
      "type" -> parameter.getType.toString,
      "isFinal" -> parameter.isFinal.toString
    ) ++ PackageExtractor.getPackage(parameter.getQualifiedSignature.getOrElse(parameter.getNameAsString)) + ("isLocal" -> true.toString())
  }

}
