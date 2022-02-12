package com.virtuslab.semanticgraphs.javaparser.solver

import ch.qos.logback.classic.Logger
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.javaparser.solver.declaration.DummyResolvedTypeParameterDeclaration
import com.virtuslab.semanticgraphs.parsercommon.logger.{GraphBuddyLogging, LoggerFactory}
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

class TypeArgumentsResolutionCache {
  given logger: Logger = LoggerFactory.getLogger(this.getClass)
  private val cache: ConcurrentHashMap[String, Int] = ConcurrentHashMap()

  def updateCache(cu: CompilationUnit): Unit = cu.allNodesOf[ClassOrInterfaceType].foreach { coit =>
    val typeArgumentsCount = coit.typeArguments.length
    val typeName = coit.getNameAsString
    if typeArgumentsCountForName(typeName) < typeArgumentsCount then cache.put(typeName, typeArgumentsCount)
  }

  def clearCache(): Unit = cache.clear()

  def typeArgumentsCountForName(name: String): Int = cache.getOrDefault(name, 0)

}
