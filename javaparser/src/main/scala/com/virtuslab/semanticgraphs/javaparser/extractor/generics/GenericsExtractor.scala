package com.virtuslab.semanticgraphs.javaparser.extractor.generics

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.parsercommon.logger.GraphBuddyLogging
import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode

import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, Type, TypeParameter}
import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.expr.VariableDeclarationExpr
import com.github.javaparser.ast.nodeTypes.{NodeWithType, NodeWithTypeParameters}
import com.github.javaparser.ast.Node
import com.github.javaparser.resolution.declarations.{
  ResolvedDeclaration,
  ResolvedMethodLikeDeclaration,
  ResolvedTypeParameterDeclaration,
  ResolvedTypeParametrizable
}
import com.github.javaparser.resolution.declarations.ResolvedTypeParameterDeclaration.Bound
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedTypeVariable}
import com.github.javaparser.resolution.Resolvable

import java.util.Objects
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.util.Try

object GenericsExtractor extends GraphBuddyLogging {

  def createNodes[N <: Node, R <: ResolvedTypeParametrizable](
    node: Node with NodeWithTypeParameters[N] with Resolvable[R],
    uri: String
  ): Seq[GraphNode] = (node.typeParameters zip node.resolvedTypeParameters).map {
    case (typeParameter: TypeParameter, resolvedTypeParameter: ResolvedTypeParameterDeclaration) =>
      val typeBounds: Seq[ClassOrInterfaceType] = typeParameter.getTypeBound.iterator().asScala.toSeq
      val resolvedTypeBounds: Seq[Bound] = Try(resolvedTypeParameter.getBounds.asScala.toSeq).toOption.toSeq.flatten
      GraphNode(
        id = Try(resolvedTypeParameter.getQualifiedName).getOrElse(typeParameter.getNameAsString),
        kind = NodeKind.TYPE_PARAMETER,
        location = typeParameter.simpleNameLocation(uri),
        displayName = typeParameter.getNameAsString,
        edges = (typeBounds zip resolvedTypeBounds).flatMap {
          case (typeBound: ClassOrInterfaceType, resolvedTypeBound: Bound) =>
            typeBound.simpleNameNode.map(_.edge(resolvedTypeBound.getQualifiedName, resolvedTypeBound.edgeKind, uri))
        }
      )
  }

}
