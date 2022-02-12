package com.virtuslab.semanticgraphs.javaparser.extractor.utils

import com.virtuslab.semanticgraphs.javaparser.{FileManager, JavaParserWithCaches}
import com.virtuslab.semanticgraphs.javaparser.solver.DummyExternalDependenciesTypeSolver
import com.virtuslab.semanticgraphs.parsercommon.toPath

import com.github.javaparser.ast.PackageDeclaration
import com.github.javaparser.StaticJavaParser

import java.io.File
import java.nio.file.{Path, Paths}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.matching.Regex
import scala.util.Try

extension (file: File)
  def getParentDirectory: Option[File] = Option(file.getParentFile).orElse(Some(file)).filter(_.isDirectory)

  def parseJavaPackageDeclarationAndUpdateResolverCache(implicit
    maybeParser: Option[JavaParserWithCaches] = None,
    maybeDummyTypeSolver: Option[DummyExternalDependenciesTypeSolver] = None
  ): Option[PackageDeclaration] = {
    (maybeParser match {
      case Some(parser) => parser.parse(file.toPath)
      case None         => Try(StaticJavaParser.parse(file)).toOption
    }) flatMap { compilationUnit =>
      maybeDummyTypeSolver.foreach(_.updateCurrentNodeInformation(compilationUnit))
      compilationUnit.getPackageDeclaration.toScala
    }
  }

extension (path: Path)
  def dropNameElements(n: Int): Path = path.nameElements.drop(n).mkString(File.separator).toPath

  def dropNameElementsRight(n: Int): Path = {
    val result = path.nameElements.dropRight(n).mkString(File.separator)
    if path.isAbsolute then path.getRoot.resolve(result)
    else result.toPath
  }

  def stripSuffix(suffix: Path): Path = path.toString.stripSuffix(suffix.toString).toPath

  def nameElements = path.iterator.asScala.toSeq

  def isValidJavaIdentifier: Boolean = FileUtils.JavaIdentifierRegex.matches(path.toString)
  def endsWithAValidJavaIdentifier: Boolean = path.nameElements.lastOption.exists(_.isValidJavaIdentifier)

  def lastValidJavaIdentifierDirectory: Option[Path] = {
    @tailrec
    def dropWhileInvalid(current: Seq[Path]): Option[Path] = {
      if current.isEmpty then None
      if current.last.isValidJavaIdentifier then Some(current.mkString(File.separator).toPath)
      else dropWhileInvalid(current.dropRight(1))
    }
    val result = dropWhileInvalid(path.nameElements)
    if path.isAbsolute then result.map(path.getRoot.resolve)
    else result
  }

  /**
    * @return
    *   Returns directory path, which contains java file and is the first directory, (and all after it are so), which is
    *   valid Java Indentifier Directory, so can be root directory for Java parser
    */
  def getFirstValidJavaIdentifierDirectory: Option[Path] = {
    val subPathSeq = path.iterator.asScala.toSeq
    if !subPathSeq.lastOption.exists(_.toString.endsWith(".java")) then None
    else {
      Try {
        val parentDirSubPathSeq = subPathSeq.dropRight(1)
        for {
          lastNonJavaIdentifierIndex <-
            parentDirSubPathSeq.findLast(!_.isValidJavaIdentifier).map(subPathSeq.indexOf).orElse(Some(0))
          firstValidNameAfterwardsIndex <- subPathSeq
            .slice(lastNonJavaIdentifierIndex, subPathSeq.length - 1)
            .find(_.isValidJavaIdentifier)
            .orElse(subPathSeq.lastOption)
            .map(subPathSeq.indexOf)
        } yield subPathSeq.slice(0, firstValidNameAfterwardsIndex + 1).mkString(File.separator).toPath
      }.toOption.flatten
    }
  }

  def relativizeToPackageLevel: Path = FileManager.relativizeSrcToPackageLevel(path.toString).toPath

object FileUtils {
  val JavaIdentifierRegex: Regex = "\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*".r
}
