package com.virtuslab.semanticgraphs.javaparser.extractor.utils

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.proto.model.graphnode.{Edge, Location}

import ch.qos.logback.classic.Logger
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, PrimitiveType, Type, TypeParameter}
import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.expr.*
import com.github.javaparser.ast.nodeTypes.{NodeWithName, NodeWithType, NodeWithTypeArguments, NodeWithTypeParameters}
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.ast.Node
import com.github.javaparser.resolution.declarations.*
import com.github.javaparser.resolution.declarations.ResolvedTypeParameterDeclaration.Bound
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType, ResolvedTypeVariable}
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametrized
import com.github.javaparser.resolution.Resolvable
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.{
  JavaParserFieldDeclaration,
  JavaParserParameterDeclaration,
  JavaParserVariableDeclaration
}

import java.util
import java.util.Optional
import scala.annotation.targetName
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.reflect.{classTag, ClassTag}
import scala.util.{Failure, Success, Try}

extension [N <: Node, T <: Type, R <: ResolvedDeclaration](
  node: N with NodeWithType[N, T] with Resolvable[R]
)(using
  logger: Logger
) {

  def getTypeFullyQualifiedName: String = {
    def fallback: String = node.getType.resolveTry
      .flatMap(r => Try(r.describe()))
      .getOrElse(node.getTypeAsString)
      .split('<')
      .headOption
      .getOrElse(node.getTypeAsString)
    node.resolveOption match {
      case Some(rvd: ResolvedValueDeclaration) =>
        Try(rvd.getType).flatMap(t => Try(t.getQualifiedName)).getOrElse(fallback)
      case _ => fallback
    }
  }

  def typeEdge(uri: String): Option[Edge] = node.typeNameNode.orElse(node.typeNode).orElse(Some(node)).map { n =>
    n.edge(
      targetId = node.getTypeFullyQualifiedName,
      edgeKind = EdgeKind.TYPE,
      uri = uri
    )
  }

  def typeArgumentEdges(uri: String): Seq[Edge] = {
    val types: Seq[Type] = node.getType.allNodesOf[Type] diff Seq(node.getType)
    val (resolvedTypes: Seq[ResolvedType], edgeKind) = (for {
      resolvedNode <- node.resolveTry
      kind = resolvedNode match {
        case rvd: ResolvedValueDeclaration  => EdgeKind.TYPE_ARGUMENT
        case rmd: ResolvedMethodDeclaration => EdgeKind.RETURN_TYPE_ARGUMENT
        case _                              => EdgeKind.TYPE_ARGUMENT
      }
      resolvedType: ResolvedType <- resolvedNode match {
        case rvd: ResolvedValueDeclaration  => Try(rvd.getType)
        case rmd: ResolvedMethodDeclaration => Try(rmd.getReturnType)
        case _                              => node.getType.resolveTry
      }
    } yield (resolvedType.allTypeArguments diff Seq(resolvedType)) -> kind)
      .getOrElse(Seq.empty -> EdgeKind.TYPE_ARGUMENT)
    types.zip(resolvedTypes).flatMap { case (t, rt) =>
      t.simpleNameNode.map(_.edge(rt.getQualifiedName, edgeKind, uri))
    }
  }

}

extension [N <: Node, R <: ResolvedTypeParametrizable](
  node: Node with NodeWithTypeParameters[N] with Resolvable[R]
)(using
  logger: Logger
) {
  def typeParameters: Seq[TypeParameter] = node.getTypeParameters.iterator().asScala.toSeq

  def resolvedTypeParameters: Seq[ResolvedTypeParameterDeclaration] =
    node.resolveOption.toSeq.flatMap(_.getTypeParameters.asScala.toSeq)

  def typeParameterEdges(uri: String): Seq[Edge] = for {
    (typeParameter, resolvedTypeParameter) <- typeParameters zip resolvedTypeParameters
    edge <- typeParameter.simpleNameNode
      .flatMap(sn => {
        Try(
          sn.edge(targetId = resolvedTypeParameter.getQualifiedName, edgeKind = EdgeKind.TYPE_PARAMETER, uri = uri)
        ).toOption
      })
      .toSeq
  } yield edge

}

extension (resolvedType: ResolvedType)(using
  logger: Logger
) {

  def getQualifiedName: String = Try {
    resolvedType match {
      case rrt: ResolvedReferenceType => rrt.getQualifiedName
      case rtv: ResolvedTypeVariable  => rtv.qualifiedName
      case rt @ _                     => rt.describe
    }
  }.getOrElse(resolvedType.describe())

  def typeArguments: Seq[ResolvedType] = resolvedType match {
    case rtp: ResolvedTypeParametrized => rtp.typeParameters
    case _                             => Seq.empty
  }

  def allTypeArguments: Seq[ResolvedType] = {
    def recExtractTypeArguments(current: ResolvedType): Seq[ResolvedType] =
      if current.typeArguments.nonEmpty then Seq(current) ++ current.typeArguments.flatMap(recExtractTypeArguments)
      else Seq(current)
    recExtractTypeArguments(resolvedType)
  }

}

extension (rtp: ResolvedTypeParametrized)(using
  logger: Logger
) {
  def typeParameters: Seq[ResolvedType] = rtp.typeParametersMap.getTypes.asScala.toSeq
}

extension (bound: Bound)(using
  logger: Logger
) {
  def getQualifiedName: String = bound.getType.getQualifiedName
  def edgeKind: EdgeKind = if bound.isExtends then EdgeKind.EXTEND else EdgeKind.SUPER
}

extension (coit: ClassOrInterfaceType)(using
  logger: Logger
) {
  def typeArguments: Seq[Type] = coit.getTypeArguments.toScala.toSeq.flatMap(_.asScala.toSeq)
}

extension (rmld: ResolvedMethodLikeDeclaration)(using
  logger: Logger
) {

  def qualifiedSignatureOption: Option[String] = rmld match {
    case rcd: (ResolvedConstructorDeclaration | ResolvedMethodDeclaration) => Try(rcd.getQualifiedSignature).toOption
    case _                                                                 => None
  }

}

extension (rrt: ResolvedReferenceType)(using
  logger: Logger
) {

  def allMethodsVisibleToInheritors: Seq[ResolvedMethodDeclaration] = Try(rrt.getAllMethodsVisibleToInheritors)
    .withLogging(stackTraceFramesToSkip = 2)
    .map(_.asScala.toSeq)
    .getOrElse(Seq.empty)

}
