package com.virtuslab.semanticgraphs.javaparser.solver.declaration

import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.javaparser.solver.declaration.DummyResolvedTypeParameterDeclaration
import com.virtuslab.semanticgraphs.javaparser.solver.TypeArgumentsResolutionCache

import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.resolution.declarations.{ResolvedTypeParameterDeclaration, ResolvedTypeParametrizable}

import java.util
import java.util.{Optional, UUID}
import java.util.concurrent.ConcurrentHashMap
import scala.collection.{concurrent, mutable}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait DummyResolvedTypeParametrizable(name: String, cache: TypeArgumentsResolutionCache)
  extends ResolvedTypeParametrizable {
  private def typeArgumentsCount: Int = cache.typeArgumentsCountForName(name)

  override def getTypeParameters: util.List[ResolvedTypeParameterDeclaration] =
    if typeArgumentsCount == 0 then List.empty.asJava
    else (1 to typeArgumentsCount).map(DummyResolvedTypeParameterDeclaration(_, cache)).toList.asJava

  override def findTypeParameter(name: String): Optional[ResolvedTypeParameterDeclaration] =
    getTypeParameters.asScala.find(_.getName == name).toJava
}
