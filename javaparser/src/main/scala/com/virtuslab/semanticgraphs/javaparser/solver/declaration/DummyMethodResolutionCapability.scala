package com.virtuslab.semanticgraphs.javaparser.solver.declaration

import com.github.javaparser.resolution.declarations.ResolvedMethodDeclaration
import com.github.javaparser.resolution.types.ResolvedType
import com.github.javaparser.symbolsolver.logic.MethodResolutionCapability
import com.github.javaparser.symbolsolver.model.resolution.SymbolReference

import java.util
import java.util.{Optional, UUID}
import scala.collection.{concurrent, mutable}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait DummyMethodResolutionCapability extends MethodResolutionCapability {

  override def solveMethod(
    name: String,
    argumentsTypes: util.List[ResolvedType],
    staticOnly: Boolean
  ): SymbolReference[ResolvedMethodDeclaration] = SymbolReference.unsolved(classOf[ResolvedMethodDeclaration])

}
