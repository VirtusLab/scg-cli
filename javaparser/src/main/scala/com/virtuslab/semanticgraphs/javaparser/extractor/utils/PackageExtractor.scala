package com.virtuslab.semanticgraphs.javaparser.extractor.utils

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration

import scala.jdk.OptionConverters._
import java.util.Optional

object PackageExtractor {

  def getPackage(fqcnOpt: Optional[String]): Map[String, String] = {
    fqcnOpt.toScala
      .map(getPackage)
      .getOrElse(Map.empty)
  }

  def getPackage(fqcn: String): Map[String, String] = {
    val _package = fqcn.split("\\(")(0).split("\\.").dropRight(1).filter(_.headOption.forall(_.isLower)).mkString(".")
    Map("package" -> _package)
  }

}
