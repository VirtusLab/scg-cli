package com.virtuslab.semanticgraphs.javaparser.extractor.utils

import scala.annotation.tailrec
import scala.util.matching.Regex

object TooltipUtils {

  private val SPECIAL_KEYWORDS: Map[String, Regex] = List(
    "this",
    "null",
    "short",
    "false",
    "boolean",
    "int",
    "string",
    "float",
    "double",
    "long",
    "short",
    "void",
    "throws",
    "default",
    ","
  ).map(keyword => keyword -> s"(?<!\\w)$keyword(?!\\w)".r).toMap

  extension (token: String) {
    private def addMarkClass(className: String) = s"<mark class=\"$className\">$token</mark>"

    def asModifier: String = token.addMarkClass("modifier")
    def asPrimitiveType: String = token.addMarkClass("primitive")
    def asMethod: String = token.addMarkClass("method")
    def asConstructor: String = token.addMarkClass("constructor")
    def asTypeParameter: String = token.addMarkClass("type-parameter")
    def asClassField: String = token.addMarkClass("class-field")
    def asAnnotation: String = token.addMarkClass("annotation")
  }

  private def isSpecialChar(symbol: Char): Boolean = symbol == '>' || symbol == ' ' || symbol == ',' || symbol == '&'

  def colorGenericTypes(genericTypes: Set[String], typeName: String): String = {

    @tailrec
    def colorGenericType(acc: String, genericType: String, typeName: String): String = {
      val index = typeName.indexOf(genericType)
      if (index != -1 && index + genericType.length < typeName.length) {
        val prefix = typeName.slice(0, index)
        val word = typeName.slice(index, index + genericType.length)
        val suffix = typeName.slice(index + genericType.length, typeName.length)

        val symbol = typeName(index + genericType.length)

        colorGenericType(prefix + (if isSpecialChar(symbol) then word.asTypeParameter else word), genericType, suffix)
      } else {
        acc + typeName
      }
    }

    genericTypes.foldLeft(typeName)((acc, word) => colorGenericType("", word, acc))
  }

  def colorAnnotations(declarationString: String): String = {

    @tailrec
    def colorAnnotation(acc: String, declarationString: String): String = {
      val index = declarationString.indexOf('@')
      val annotationEndIndex = declarationString.indexOf(' ', index)
      if (index != -1 && annotationEndIndex != -1) {
        val prefix = declarationString.slice(0, index)
        val word = declarationString.slice(index, annotationEndIndex)
        val suffix = declarationString.slice(annotationEndIndex, declarationString.length)
        colorAnnotation(prefix + word.asAnnotation, suffix)
      } else {
        acc + declarationString
      }
    }

    colorAnnotation("", declarationString)
  }

  def colorizeSpecialKeywords(declarationString: String): String = {
    SPECIAL_KEYWORDS.foldLeft(declarationString) { (acc, keyword) =>
      keyword._2.replaceAllIn(acc, keyword._1.asModifier)
    }
  }

}
