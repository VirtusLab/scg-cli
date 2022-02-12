package com.virtuslab.semanticgraphs.parsercommon.versioning

import com.virtuslab.semanticgraphs.parsercommon.FileOperations

import java.io.File
import java.nio.file.Path
import scala.io.Source

case class FileVersionFromFile(file: File) extends FileVersion {
  lazy val path: String = file.getPath

  lazy val lastModified: Long = file.lastModified()

  lazy val size: Long = file.length()

  lazy val contentHashCode: Int = {
    val source = Source.fromFile(file)
    try source.mkString.hashCode
    finally source.close()
  }

  def toStamp: FileVersionStamp = FileVersionStamp(path, lastModified, size, contentHashCode)
}

object FileVersionFromFile {
  private val SEMANTIC_GRAPHS_DIR = ".semanticgraphs"

  def getForFilesInDirectory(directory: File, fileSuffix: String): List[FileVersionFromFile] = FileOperations
    .getFileTree(directory)
    .filter(fileObj => fileObj.isFile && fileObj.getName.endsWith(fileSuffix))
    .map(FileVersionFromFile.apply)

  def saveVersionOfFile(projectPath: Path, filePath: Path): Unit =
    FileVersionStamp.upsertState(projectPath, Seq(FileVersionFromFile(filePath.toFile).toStamp))
}
