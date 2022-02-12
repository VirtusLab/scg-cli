package com.virtuslab.semanticgraphs.javaparser.extractor.initializer

import com.virtuslab.semanticgraphs.javaparser.extractor.generics.GenericsExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.method.MethodLikeParameterExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.MethodLikeDeclaration
import com.virtuslab.semanticgraphs.javaparser.extractor.value.VariableExtractor
import com.virtuslab.semanticgraphs.parsercommon.logger.GraphBuddyLogging
import com.virtuslab.semanticgraphs.proto.model.graphnode.GraphNode

import com.github.javaparser.ast.body.{ConstructorDeclaration, InitializerDeclaration, MethodDeclaration}

object InitializerExtractor extends GraphBuddyLogging {

  def createStaticNodes(declarations: Iterable[InitializerDeclaration], uri: String): Seq[GraphNode] = {
    declarations.filter(_.isStatic).toSeq.flatMap { initializer =>
      VariableExtractor.createNodes(initializer.variableDeclarations, uri, isLocal = false)
    }
  }

}
