package com.virtuslab.semanticgraphs.javaparser.extractor.utils

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.proto.model.graphnode.{Edge, Location}

import ch.qos.logback.classic.Logger
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, PrimitiveType, Type, TypeParameter}
import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.expr.*
import com.github.javaparser.ast.nodeTypes.*
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.ast.Node
import com.github.javaparser.resolution.declarations.*
import com.github.javaparser.resolution.declarations.ResolvedTypeParameterDeclaration.Bound
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType, ResolvedTypeVariable}
import com.github.javaparser.resolution.Resolvable
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.{
  JavaParserFieldDeclaration,
  JavaParserParameterDeclaration,
  JavaParserVariableDeclaration
}

import java.util
import java.util.Optional
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.reflect.{classTag, ClassTag}
import scala.util.{Failure, Success, Try}

extension (node: Node)(using
  logger: Logger
) {

  def allNodesOf[N <: Node](implicit
    classTag: ClassTag[N]
  ): Seq[N] = Try {
    node.findAll[N](classTag.runtimeClass.asInstanceOf[Class[N]]).asScala.toSeq
  }.toOption.toSeq.flatten

  def simpleNameNode: Option[SimpleName] = node.getChildNodes.asScala.flatMap {
    case sn: SimpleName => Some(sn)
    case _              => None
  }.headOption

  def typeNode: Option[Type] = node match {
    case nwt: NodeWithType[_, _] => Option(nwt.getType)
    case n =>
      n.getChildNodes.asScala.flatMap {
        case t: Type => Some(t)
        case _       => None
      }.headOption
  }

  def typeNameNode: Option[SimpleName] = typeNode.flatMap(_.simpleNameNode)

  def urilessLocation: Option[Location] = {
    for {
      range <- node.getRange.toScala
      start = range.begin
      end = range.end
    } yield Location(
      startLine = start.line - 1,
      startCharacter = start.column - 1,
      endLine = end.line - 1,
      endCharacter = end.column
    )
  }

  def location(uri: String): Option[Location] = urilessLocation.map(_.copy(uri = uri))

  def coordinates: Option[String] = urilessLocation.map(_.coordinatesString)

  def simpleNameLocation(uri: String): Option[Location] = simpleNameNode.flatMap(_.location(uri))

  def simpleNameCoordinates: Option[String] = simpleNameNode.flatMap(_.coordinates)

  def edge(targetId: String, edgeKind: EdgeKind, uri: String): Edge =
    Edge(to = targetId, `type` = edgeKind, location = location(uri))

  def scopeTypeParameters: Set[String] = {
    node.getParentNode.asScala match {
      case Some(m: NodeWithTypeParameters[_]) =>
        m.getTypeParameters.asScala.map(_.toString).toSet ++ m.scopeTypeParameters
      case _ => Set.empty
    }
  }

  def callEdges(uri: String): Seq[Edge] = (
    node.allNodesOf[MethodCallExpr]
      ++ node.allNodesOf[ObjectCreationExpr]
      ++ node.allNodesOf[ExplicitConstructorInvocationStmt].filter(_.isSuperStmt)
      ++ node.allNodesOf[NameExpr]
      ++ node.allNodesOf[FieldAccessExpr]
      ++ node.allNodesOf[MethodReferenceExpr]
  ).flatMap { call =>
    for {
      signature <- call match {
        case callExpression: (MethodCallExpr | ObjectCreationExpr | ExplicitConstructorInvocationStmt |
              MethodReferenceExpr) =>
          Try(callExpression.resolve().getQualifiedSignature).withLoggingOption(stackTraceFramesToSkip = 2)
        case expr: (NameExpr | FieldAccessExpr) => expr.getQualifiedSignature
      }
      sourceNode <- (call match {
        case oce: ObjectCreationExpr => Try(oce.getType).toOption
        case c @ _                   => c.simpleNameNode
      }).orElse(Some(call))
      location = call match {
        case superCall: ExplicitConstructorInvocationStmt => superCall.superLocation(uri)
        case _                                            => sourceNode.location(uri)
      }
    } yield Edge(to = signature, `type` = EdgeKind.CALL, location = location)
  }

  def closestPossiblyDeclaringAncestor: Option[(ClassLikeDeclaration | MethodLikeDeclaration)] = {
    node.getParentNode.toScala match {
      case Some(parent: (ClassLikeDeclaration | MethodLikeDeclaration)) => Some(parent)
      case None                                                         => None
      case Some(parent: Node)                                           => parent.closestPossiblyDeclaringAncestor
    }
  }

  def closestPossiblyDeclaringAncestorSignature: Option[String] = for {
    ancestorNode <- closestPossiblyDeclaringAncestor
    resolvedAncestor <- ancestorNode.resolveOption
    ancestorSignature <- resolvedAncestor match {
      case r: ResolvedReferenceTypeDeclaration => Try(r.getQualifiedName).withLoggingOption()
      case r: ResolvedMethodLikeDeclaration    => Try(r.getQualifiedSignature).withLoggingOption()
    }
  } yield ancestorSignature

}
