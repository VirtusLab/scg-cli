package com.virtuslab.semanticgraphs.javaparser.solver

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.Node
import com.github.javaparser.resolution.types.ResolvedType
import com.github.javaparser.resolution.SymbolResolver
import com.github.javaparser.symbolsolver.JavaSymbolSolver

import java.util
import java.util.{Optional, UUID}
import scala.collection.{concurrent, mutable}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/**
  * The only task of this class is to pass every processed node to `DummyTypeSolver`, except from this it passes
  * everything to `JavaSymbolSolver`
  *
  * @param javaSymbolSolver
  *   solver which do real job
  * @param dummyTypeSolver
  *   dummy type solver, which needs information about processed nodes to better mimic solver, which has information
  *   about external dependecies. Information from nodes should be used to know, what is user's package.
  */
class EDMTSCompatibleSymbolSolver(
  javaSymbolSolver: JavaSymbolSolver,
  dummyTypeSolver: DummyExternalDependenciesTypeSolver
) extends SymbolResolver {

  def resolveDeclaration[T](node: Node, resultClass: Class[T]): T =
    javaSymbolSolver.resolveDeclaration(node, resultClass)

  def toResolvedType[T](javaparserType: Type, resultClass: Class[T]): T =
    javaSymbolSolver.toResolvedType(javaparserType, resultClass)

  def calculateType(expression: Expression): ResolvedType = javaSymbolSolver.calculateType(expression)
}
