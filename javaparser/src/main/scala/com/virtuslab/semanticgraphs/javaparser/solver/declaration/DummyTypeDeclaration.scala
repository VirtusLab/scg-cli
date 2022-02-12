package com.virtuslab.semanticgraphs.javaparser.solver.declaration

import com.github.javaparser.resolution.declarations.{
  ResolvedConstructorDeclaration,
  ResolvedFieldDeclaration,
  ResolvedMethodDeclaration,
  ResolvedReferenceTypeDeclaration
}
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType}
import com.github.javaparser.symbolsolver.logic.AbstractTypeDeclaration

import java.util
import java.util.{Optional, UUID}
import scala.collection.{concurrent, mutable}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait DummyTypeDeclaration extends AbstractTypeDeclaration {
  override def getAllFields: util.List[ResolvedFieldDeclaration] = List.empty.asJava

  override def getAncestors(acceptIncompleteList: Boolean): util.List[ResolvedReferenceType] = List.empty.asJava

  override def getConstructors: util.List[ResolvedConstructorDeclaration] = List.empty.asJava

  override def getDeclaredMethods: util.Set[ResolvedMethodDeclaration] = Set.empty.asJava

  override def hasDirectlyAnnotation(qualifiedName: String): Boolean = false

  override def isAssignableBy(`type`: ResolvedType): Boolean = false

  override def isAssignableBy(other: ResolvedReferenceTypeDeclaration): Boolean = false
}
