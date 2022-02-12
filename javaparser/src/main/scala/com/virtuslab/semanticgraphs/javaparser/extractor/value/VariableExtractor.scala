package com.virtuslab.semanticgraphs.javaparser.extractor.value

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.parsercommon.logger.GraphBuddyLogging
import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode

import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.expr.VariableDeclarationExpr
import com.github.javaparser.ast.Modifier
import com.github.javaparser.resolution.declarations.ResolvedMethodLikeDeclaration
import com.github.javaparser.resolution.Resolvable

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.util.Try

object VariableExtractor extends GraphBuddyLogging {

  def createNodes(
    variableDeclarators: Iterable[VariableDeclarator],
    uri: String,
    isLocal: Boolean
  ): Seq[GraphNode] = {
    variableDeclarators.toSeq.flatMap { variableDeclarator =>
      for { signature <- variableDeclarator.getQualifiedSignature(uri) } yield GraphNode(
        id = signature,
        kind = variableDeclarator.kind,
        location = variableDeclarator.simpleNameLocation(uri),
        properties = createProperties(variableDeclarator) ++ PackageExtractor.getPackage(
          signature
        ) + ("isLocal" -> isLocal.toString()),
        displayName = variableDeclarator.getNameAsString,
        edges = variableDeclarator.typeEdge(uri).toSeq ++
          variableDeclarator.typeArgumentEdges(uri) ++
          variableDeclarator.callEdges(uri)
      )
    }
  }

  private def createProperties(declaration: VariableDeclarator): Map[String, String] = {
    declaration.getParentNode.asScala.fold(Map.empty) {
      case expr: VariableDeclarationExpr =>
        Map(
          "type" -> expr.getElementType.toString,
          "isFinal" -> expr.isFinal.toString
        )
      case expr: FieldDeclaration =>
        Map(
          "type" -> expr.getElementType.toString,
          "isFinal" -> expr.isFinal.toString,
          "access" -> expr.getAccessSpecifier.toString.toLowerCase,
          "isStatic" -> expr.isStatic.toString
        )
      case _ => Map.empty
    }
  }

}
