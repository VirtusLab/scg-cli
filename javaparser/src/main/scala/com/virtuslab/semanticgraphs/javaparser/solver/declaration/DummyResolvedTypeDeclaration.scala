package com.virtuslab.semanticgraphs.javaparser.solver.declaration

import com.github.javaparser.resolution.declarations.{ResolvedReferenceTypeDeclaration, ResolvedTypeDeclaration}

import java.util
import java.util.{Optional, UUID}
import scala.collection.{concurrent, mutable}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait DummyResolvedTypeDeclaration(qualifiedName: String, name: String) extends ResolvedTypeDeclaration {
  override def getName: String = name

  override def containerType(): Optional[ResolvedReferenceTypeDeclaration] = None.toJava

  override def getClassName: String = name

  override def getPackageName: String = ""

  override def getQualifiedName: String = qualifiedName

  override def internalTypes(): util.Set[ResolvedReferenceTypeDeclaration] = Set.empty.asJava
}
