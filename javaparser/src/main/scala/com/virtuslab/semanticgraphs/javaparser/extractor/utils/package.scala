package com.virtuslab.semanticgraphs.javaparser.extractor

import com.virtuslab.semanticgraphs.proto.model.graphnode.Location

import com.github.javaparser.ast.body.{
  ClassOrInterfaceDeclaration,
  ConstructorDeclaration,
  EnumDeclaration,
  MethodDeclaration
}

package object utils {
  type MethodLikeDeclaration = MethodDeclaration | ConstructorDeclaration
  type ClassLikeDeclaration = EnumDeclaration | ClassOrInterfaceDeclaration

  implicit val locationOrdering: Ordering[Location] =
    Ordering.by[Location, Int](_.startLine).orElseBy(_.startCharacter).orElseBy(_.endLine).orElseBy(_.endCharacter)

  extension [T](seq: Seq[T]) def hasDuplicates = seq.distinct.length != seq.length
}
