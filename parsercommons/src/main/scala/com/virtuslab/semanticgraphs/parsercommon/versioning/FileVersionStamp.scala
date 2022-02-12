package com.virtuslab.semanticgraphs.parsercommon.versioning

import com.virtuslab.semanticgraphs.parsercommon.versioning.FileVersionStamp.*
import com.virtuslab.semanticgraphs.parsercommon.FileOperations
import upickle.default.*

import java.io.File
import java.nio.file.{Files, Path}
import scala.util.Try

case class FileVersionStamp(path: String, lastModified: Long, size: Long, contentHashCode: Int) extends FileVersion derives ReadWriter

object FileVersionStamp {

  def writeSeq(stamps: Seq[FileVersionStamp]): String = write(stamps)

  def readSeq(stampsAsJsonString: String): Seq[FileVersionStamp] =
    Try(read[Seq[FileVersionStamp]](stampsAsJsonString)).getOrElse(Seq.empty)

  def upsertState(projectPath: Path, versions: Seq[FileVersionStamp]): Unit = {
    val file = getProjectStateFile(projectPath)
    val jsonToWrite = {
      if (file.exists()) {
        val oldState = readState(projectPath)
        val mergedState = mergeOldAndNewState(oldState, versions)
        FileVersionStamp.writeSeq(mergedState)
      } else {
        if (!file.getParentFile.exists()) {
          file.getParentFile.mkdirs()
        }
        file.createNewFile()
        FileVersionStamp.writeSeq(versions)
      }
    }
    Files.write(file.toPath, jsonToWrite.getBytes)
  }

  private def mergeOldAndNewState(
    oldState: Seq[FileVersionStamp],
    newState: Seq[FileVersionStamp]
  ): Seq[FileVersionStamp] = {
    val oldStateMap = oldState.map(fileState => fileState.path -> fileState).toMap
    val newStateSet = newState.map(fileState => fileState.path -> fileState).toMap
    val oldPathsSetNotPresentInNewSet = oldStateMap.keySet.diff(newStateSet.keySet)
    val oldPathsNotPresentInNewSet = oldPathsSetNotPresentInNewSet.flatMap(oldStateMap.get)
    newState ++ oldPathsNotPresentInNewSet.toSeq
  }

  def readState(projectPath: Path): Seq[FileVersionStamp] = {
    val file = getProjectStateFile(projectPath)
    if (file.exists()) {
      val fileContent = FileOperations.readFile(file)
      readSeq(fileContent)
    } else {
      Seq()
    }
  }

  private def getProjectStateFile(projectPath: Path): File =
    projectPath.resolve(".semanticgraphs").resolve("versions.json").toFile
}
