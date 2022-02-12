package com.virtuslab.semanticgraphs.javaparser.extractor.utils

import ch.qos.logback.classic.Logger
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, Type}
import com.github.javaparser.ast.body.{
  ClassOrInterfaceDeclaration,
  ConstructorDeclaration,
  EnumDeclaration,
  MethodDeclaration
}
import com.github.javaparser.ast.expr.{MethodCallExpr, MethodReferenceExpr, ObjectCreationExpr}
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.ast.Node
import com.github.javaparser.resolution.declarations.{
  ResolvedConstructorDeclaration,
  ResolvedEnumDeclaration,
  ResolvedMethodDeclaration,
  ResolvedMethodLikeDeclaration,
  ResolvedReferenceTypeDeclaration
}
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType}
import com.github.javaparser.resolution.Resolvable

import scala.annotation.targetName
import scala.util.Try

extension [T](r: Node with Resolvable[T])(using
  logger: Logger
) {
  def resolveLoggingStackTraceFrame(skip: Int = 3): Try[T] =
    Try(r.resolve()).withLogging(stackTraceFramesToSkip = skip)
  def resolveTry: Try[T] = r.resolveLoggingStackTraceFrame()
  def resolveOption: Option[T] = r.resolveLoggingStackTraceFrame().toOption
}

extension [T <: ResolvedType](r: Node with Resolvable[T])(using
  logger: Logger
) {
  def resolveQualifiedNameOption: Option[String] = r.resolveOption.flatMap(ro => Try(ro.getQualifiedName).toOption)
}

extension (cloml: (ClassLikeDeclaration | MethodLikeDeclaration))(using
  logger: Logger
) {

  def resolveOption: Option[(ResolvedReferenceTypeDeclaration | ResolvedMethodLikeDeclaration)] = cloml match {
    case coid: ClassOrInterfaceDeclaration => coid.resolveLoggingStackTraceFrame().toOption
    case ed: EnumDeclaration               => ed.resolveLoggingStackTraceFrame().toOption
    case cd: ConstructorDeclaration        => cd.resolveLoggingStackTraceFrame().toOption
    case md: MethodDeclaration             => md.resolveLoggingStackTraceFrame().toOption
  }

}

extension (classLikeDeclaration: ClassLikeDeclaration)(using
  logger: Logger
) {

  @targetName("resolveOption_ClassLikeDeclaration")
  def resolveOption: Option[(ResolvedReferenceTypeDeclaration | ResolvedEnumDeclaration)] = classLikeDeclaration match {
    case coid: ClassOrInterfaceDeclaration => coid.resolveLoggingStackTraceFrame().toOption
    case ed: EnumDeclaration               => ed.resolveLoggingStackTraceFrame().toOption
  }

}

extension (methodLike: MethodLikeDeclaration)(using
  logger: Logger
) {

  @targetName("resolveOption_MethodLikeDeclaration")
  def resolveOption: Option[(ResolvedMethodDeclaration | ResolvedConstructorDeclaration)] = {
    methodLike match {
      case cd: ConstructorDeclaration => cd.resolveLoggingStackTraceFrame().toOption
      case md: MethodDeclaration      => md.resolveLoggingStackTraceFrame().toOption
    }
  }

}
