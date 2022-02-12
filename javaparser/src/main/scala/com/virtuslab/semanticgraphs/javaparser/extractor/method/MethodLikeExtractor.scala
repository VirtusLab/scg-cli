package com.virtuslab.semanticgraphs.javaparser.extractor.method

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.javaparser.extractor.generics.GenericsExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.javaparser.extractor.value.VariableExtractor
import com.virtuslab.semanticgraphs.parsercommon.logger.GraphBuddyLogging
import com.virtuslab.semanticgraphs.proto.model.graphnode.Edge
import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode

import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.expr.VariableDeclarationExpr
import com.github.javaparser.resolution.declarations.ResolvedMethodLikeDeclaration
import com.github.javaparser.resolution.Resolvable

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.util.Try

object MethodLikeExtractor extends GraphBuddyLogging {

  def createNodes(methodLikes: Iterable[MethodLikeDeclaration], uri: String): Seq[GraphNode] =
    methodLikes.toSeq
      .flatMap(methodLike => {
        Seq(methodLikeNode(methodLike, uri)) ++
          VariableExtractor.createNodes(methodLike.declaredVariables, uri, isLocal = true) ++
          MethodLikeParameterExtractor.createNodes(methodLike, uri) ++
          (methodLike match {
            case md: MethodDeclaration => GenericsExtractor.createNodes(md, uri)
            case _                     => Seq.empty
          })
      })

  private def methodLikeNode(methodLike: MethodLikeDeclaration, uri: String): GraphNode = GraphNode(
    id = methodLike.getQualifiedSignature,
    kind = methodLike.kind,
    location = methodLike.simpleNameLocation(uri),
    properties = createProperties(methodLike),
    displayName = methodLike.getNameAsString,
    edges = methodLike.callEdges(uri) ++
      methodLike.overrideEdges(uri) ++
      methodLike.parameterEdges(uri) ++
      methodLike.variableDeclarationEdges(uri) ++
      methodLike.returnTypeEdge(uri).toSeq ++
      methodLike.returnTypeParameterEdges(uri) ++
      (methodLike match { case md: MethodDeclaration => md.typeParameterEdges(uri); case _ => Seq.empty })
  )

  private def createProperties(declaration: MethodLikeDeclaration): Map[String, String] = {
    val properties: Map[String, String] = declaration match {
      case method: MethodDeclaration =>
        Map(
          "isStatic" -> method.isStatic.toString,
          "isFinal" -> method.isFinal.toString,
          "access" -> method.getAccessSpecifier.toString.toLowerCase,
          "isAbstract" -> method.isAbstract.toString,
          "type" -> method.getType.toString,
          "LOC" -> method.getLOC
        ) ++ PackageExtractor.getPackage(method.getQualifiedSignature)
      case const: ConstructorDeclaration =>
        PackageExtractor.getPackage(const.getQualifiedSignature)
    }

    properties
  }

}
