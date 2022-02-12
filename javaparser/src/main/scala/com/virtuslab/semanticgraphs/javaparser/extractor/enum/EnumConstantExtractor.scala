package com.virtuslab.semanticgraphs.javaparser.extractor.`enum`

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.javaparser.extractor.NodeKind
import com.virtuslab.semanticgraphs.parsercommon.logger.GraphBuddyLogging
import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode

import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.body.EnumConstantDeclaration
import com.github.javaparser.ast.expr.VariableDeclarationExpr
import com.github.javaparser.resolution.declarations.ResolvedMethodLikeDeclaration
import com.github.javaparser.resolution.Resolvable

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.util.Try

object EnumConstantExtractor extends GraphBuddyLogging {

  def createNodes(enumConstants: Iterable[EnumConstantDeclaration], uri: String): Seq[GraphNode] = {
    enumConstants.toSeq
      .flatMap(ec => {
        ec.getQualifiedSignature.toSeq.map { ecSignature =>
          GraphNode(
            id = ecSignature,
            kind = NodeKind.VALUE,
            location = ec.simpleNameLocation(uri),
            displayName = ec.getNameAsString,
            properties = createProperties(ec) ++ PackageExtractor.getPackage(ecSignature),
            edges = ec.callEdges(uri)
          )
        }
      })
  }

  private def createProperties(declaration: EnumConstantDeclaration): Map[String, String] = Map(
    "enum" -> true.toString,
    "LOC" -> declaration.getRange.toScala.map(r => r.end.line - r.begin.line).getOrElse(0).toString
  )

}
