package com.virtuslab.semanticgraphs.javaparser.solver

import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.javaparser.solver.declaration.{
  DummyResolvedReferenceTypeDeclaration,
  DummyResolvedTypeParametrizable,
  DummyTypeDeclaration
}
import com.virtuslab.semanticgraphs.javaparser.solver.DummyExternalDependenciesTypeSolver

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.{CompilationUnit, Node, PackageDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.resolution.{MethodUsage, SymbolResolver}
import com.github.javaparser.resolution.declarations.*
import com.github.javaparser.resolution.types.{ResolvedReferenceType, ResolvedType}
import com.github.javaparser.symbolsolver.logic.{AbstractTypeDeclaration, MethodResolutionCapability}
import com.github.javaparser.symbolsolver.model.resolution.SymbolReference
import com.github.javaparser.symbolsolver.resolution.typesolvers.MemoryTypeSolver
import com.github.javaparser.symbolsolver.JavaSymbolSolver

import java.util
import java.util.{Optional, UUID}
import scala.collection.{concurrent, mutable}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/**
  * Dummy solver which behaves like it has information about external packages. The problem is that it has no
  * information which packages are local and which external. So it need to be filled with processed nodes to correctly
  * not create mocks for system classes (from java.lang packages like String). (try to remove localPackages and tests
  * should fail with incorrect package of String)
  */
class DummyExternalDependenciesTypeSolver extends MemoryTypeSolver {
  private val localPackages = new TrieMap[String, String]()
  private val typeArgumentsResolutionCache = TypeArgumentsResolutionCache()

  def updateCurrentNodeInformation(node: Node): Unit = node.findRootNode() match {
    case cu: CompilationUnit =>
      val maybePackageDecl: Optional[PackageDeclaration] = cu.getPackageDeclaration
      maybePackageDecl.toScala.foreach { pd =>
        val packageName = pd.getNameAsString
        localPackages.put(packageName, packageName)
      }
      typeArgumentsResolutionCache.updateCache(cu)
    case unknown => println("Node root is not a compilation unit but: " + unknown.getClass.getName)
  }

  /**
    * Create a mock if package is not local or java.lang
    * @param name
    *   qualified name
    * @return
    *   mock or unresolved reference in case, when package is local or java.lang
    */
  override def tryToSolveType(name: String): SymbolReference[ResolvedReferenceTypeDeclaration] = {
    if localPackages.keySet.exists(name.contains) || name.startsWith("java.lang") then
      SymbolReference.unsolved(classOf[ResolvedReferenceTypeDeclaration])
    else {
      SymbolReference.solved(
        DummyResolvedReferenceTypeDeclaration(name, name.stripPackagePrefix, typeArgumentsResolutionCache)
      )
    }
  }

}
