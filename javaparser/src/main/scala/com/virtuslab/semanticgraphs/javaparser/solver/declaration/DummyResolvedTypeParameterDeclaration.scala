package com.virtuslab.semanticgraphs.javaparser.solver.declaration

import com.virtuslab.semanticgraphs.javaparser.solver.declaration.{
  DummyResolvedTypeDeclaration,
  DummyResolvedTypeParametrizable
}
import com.virtuslab.semanticgraphs.javaparser.solver.TypeArgumentsResolutionCache

import com.github.javaparser.resolution.declarations.{
  ResolvedReferenceTypeDeclaration,
  ResolvedTypeParameterDeclaration,
  ResolvedTypeParametrizable
}
import com.github.javaparser.resolution.types.ResolvedReferenceType
import com.github.javaparser.resolution.UnsolvedSymbolException
import com.github.javaparser.symbolsolver.model.typesystem.ReferenceTypeImpl

import java.util
import java.util.{Optional, UUID}
import scala.collection.{concurrent, mutable}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

case class DummyResolvedTypeParameterDeclaration(
  name: String,
  qualifiedName: String,
  cache: TypeArgumentsResolutionCache
) extends ResolvedTypeParameterDeclaration
  with DummyResolvedTypeDeclaration(qualifiedName, name)
  with DummyResolvedTypeParametrizable(name, cache) {
  override def getBounds: util.List[ResolvedTypeParameterDeclaration.Bound] = List().asJava

  override def getContainerId: String = UUID.randomUUID().toString

  override def getContainer: ResolvedTypeParametrizable = null

  override def getContainerQualifiedName: String = UUID.randomUUID().toString

  override def `object`(): ResolvedReferenceType =
    throw UnsolvedSymbolException("DummyResolvedTypeParameterDeclaration")
}

object DummyResolvedTypeParameterDeclaration {

  def apply(i: Int, cache: TypeArgumentsResolutionCache): DummyResolvedTypeParameterDeclaration =
    DummyResolvedTypeParameterDeclaration(s"T$i", s"T$i", cache)

}
