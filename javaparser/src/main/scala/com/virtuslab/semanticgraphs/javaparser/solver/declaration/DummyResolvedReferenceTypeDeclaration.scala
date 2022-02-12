package com.virtuslab.semanticgraphs.javaparser.solver.declaration

import com.virtuslab.semanticgraphs.javaparser.solver.declaration.{
  DummyMethodResolutionCapability,
  DummyResolvedTypeDeclaration,
  DummyResolvedTypeParametrizable,
  DummyTypeDeclaration
}
import com.virtuslab.semanticgraphs.javaparser.solver.TypeArgumentsResolutionCache

import com.github.javaparser.resolution.declarations.ResolvedReferenceTypeDeclaration
import com.github.javaparser.symbolsolver.logic.MethodResolutionCapability

/**
  * Dummy declaration to mock references to classes in external dependencies.
  *
  * @param qualifiedName
  *   qualified name
  * @param name
  *   short name, last part of qualified name
  */
case class DummyResolvedReferenceTypeDeclaration(
  qualifiedName: String,
  name: String,
  cache: TypeArgumentsResolutionCache
) extends ResolvedReferenceTypeDeclaration
  with MethodResolutionCapability
  with DummyResolvedTypeDeclaration(qualifiedName, name)
  with DummyResolvedTypeParametrizable(name, cache)
  with DummyTypeDeclaration
  with DummyMethodResolutionCapability
