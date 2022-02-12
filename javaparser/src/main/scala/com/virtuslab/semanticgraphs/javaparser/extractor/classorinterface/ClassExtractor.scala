package com.virtuslab.semanticgraphs.javaparser.extractor.classorinterface

import com.virtuslab.semanticgraphs.javaparser.extractor.{EdgeKind, NodeKind}
import com.virtuslab.semanticgraphs.javaparser.extractor.generics.GenericsExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.initializer.InitializerExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.method.MethodLikeExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.javaparser.extractor.value.VariableExtractor
import com.virtuslab.semanticgraphs.parsercommon.logger.GraphBuddyLogging
import com.virtuslab.semanticgraphs.proto.model.graphnode.{Edge, GraphNode, Location}

import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{
  CallableDeclaration,
  ClassOrInterfaceDeclaration,
  ConstructorDeclaration,
  MethodDeclaration
}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.language.implicitConversions
import scala.util.Try

object ClassExtractor extends GraphBuddyLogging:

  def createClass(coids: Iterable[ClassOrInterfaceDeclaration], uri: String): Iterable[GraphNode] = {
    coids.flatMap(coid => {
      coid.getQualifiedSignature.toSeq
        .map(signature => {
          GraphNode(
            id = signature,
            kind = coid.kind,
            location = coid.simpleNameLocation(uri),
            properties = createProperties(coid) ++ PackageExtractor.getPackage(signature),
            displayName = coid.getNameAsString,
            edges = coid.declarationEdges(uri) ++
              coid.extendEdges(uri) ++
              coid.typeParameterEdges(uri) ++
              coid.extendTypeArgumentEdges(uri) ++
              coid.initializerCallEdges(uri)
          )
        }) ++ MethodLikeExtractor.createNodes(coid.methodLikeDeclarations, uri) ++
        VariableExtractor.createNodes(coid.variableDeclarations, uri, isLocal = false) ++
        GenericsExtractor.createNodes(coid, uri) ++
        InitializerExtractor.createStaticNodes(coid.initializerDeclarations, uri)
    })
  }

  private def createProperties(coid: ClassOrInterfaceDeclaration): Map[String, String] = {
    Map(
      "inner class" -> coid.isInnerClass.toString,
      "isAbstract" -> coid.isAbstract.toString,
      "isFinal" -> coid.isFinal.toString,
      "access" -> coid.getAccessSpecifier.toString.toLowerCase,
      "LOC" -> coid.getLOC
    )
  }

end ClassExtractor
