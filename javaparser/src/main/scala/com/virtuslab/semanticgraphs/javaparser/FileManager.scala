package com.virtuslab.semanticgraphs.javaparser

import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.parsercommon.toPath
import com.virtuslab.semanticgraphs.proto.model.graphnode.SemanticGraphFile

import java.io.{File, FileOutputStream}
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters.IteratorHasAsScala

object FileManager:
  case class FileToBeSaved(projectPath: String, semanticGraphFile: SemanticGraphFile, filePath: String)

  def dumpFile(dumpableFile: FileToBeSaved): Unit =
    dumpFile(dumpableFile.projectPath, dumpableFile.semanticGraphFile, dumpableFile.filePath)

  /**
    * Save `SemanticGraphFile` to file under path: `projectPath`/.semanticgraphs/package/filename.semanticsgraphdb
    *
    * @param projectPath
    *   path to currently opened project, like `/Users/user/IdeaProjects/semantic-graphs`
    * @param semanticGraphFile
    *   file to save
    * @param filePath
    *   path in project of file, which is described by `semanticGraphFile`, like
    *   `src/main/scala/com/virtuslab/Option.scala`
    */
  def dumpFile(projectPath: String, semanticGraphFile: SemanticGraphFile, filePath: String): Unit = {
    val packageLevelRelativeFilePath = projectPath.toPath.relativize(filePath.toPath).relativizeToPackageLevel.toString
    val fileUri =
      projectPath.toPath.resolve(".semanticgraphs").resolve(s"$packageLevelRelativeFilePath.semanticgraphdb")

    val file = fileUri.toFile
    file.getParentFile.mkdirs()
    file.createNewFile()
    val outputStream = new FileOutputStream(file, false)
    outputStream.write(semanticGraphFile.toByteArray)
    outputStream.close()
  }

  /**
    * Detect and strip prefixes like `src/main/java` from file path. Handled cases:
    *   - src
    *   - src/main
    *   - src/main/java
    *   - src/main/scala
    *
    * @param filePath
    *   path to source file, like `src/main/scala/com/virtuslab/Option.scala`
    */
  def relativizeSrcToPackageLevel(filePath: String): String = {
    val fileSeparator = File.separator
    val pathParts = filePath.toPath.nameElements.map(_.toString)
    val cutoffPosition = pathParts match {
      case pp if pp.contains("src")  => pathParts.indexOf("src")
      case pp if pp.contains("main") => pathParts.indexOf("main")
      case pp if pp.contains("test") => pathParts.indexOf("test")
      case pp if pp.contains("java") => pathParts.indexOf("java")
      case _                         => pathParts.indexOf("scala")
    }

    (pathParts.drop(cutoffPosition) match {
      case "src" :: "main" :: "java" :: pathWithPackage  => Some(pathWithPackage)
      case "src" :: "main" :: "scala" :: pathWithPackage => Some(pathWithPackage)
      case "src" :: "main" :: pathWithPackage            => Some(pathWithPackage)
      case "src" :: "test" :: "java" :: pathWithPackage  => Some(pathWithPackage)
      case "src" :: "test" :: "scala" :: pathWithPackage => Some(pathWithPackage)
      case "src" :: "test" :: pathWithPackage            => Some(pathWithPackage)
      case "src" :: "java" :: pathWithPackage            => Some(pathWithPackage)
      case "src" :: "scala" :: pathWithPackage           => Some(pathWithPackage)
      case "src" :: pathWithPackage                      => Some(pathWithPackage)
      case "main" :: "java" :: pathWithPackage           => Some(pathWithPackage)
      case "main" :: "scala" :: pathWithPackage          => Some(pathWithPackage)
      case "main" :: pathWithPackage                     => Some(pathWithPackage)
      case "test" :: "java" :: pathWithPackage           => Some(pathWithPackage)
      case "test" :: "scala" :: pathWithPackage          => Some(pathWithPackage)
      case "test" :: pathWithPackage                     => Some(pathWithPackage)
      case "java" :: pathWithPackage                     => Some(pathWithPackage)
      case "scala" :: pathWithPackage                    => Some(pathWithPackage)
      case _                                             => None
    }).map(_.mkString(fileSeparator)).getOrElse(filePath)
  }

end FileManager
