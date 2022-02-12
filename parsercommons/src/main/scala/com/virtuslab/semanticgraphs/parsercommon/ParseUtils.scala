package com.virtuslab.semanticgraphs.parsercommon

import java.nio.file.{Files, Paths}
import scala.concurrent.duration.*

object ParseUtils {
  val DEFAULT_BYTES_PER_SECOND_RATIO: Int = 2048
  val MINIMAL_TIMEOUT: FiniteDuration = 2.seconds

  def estimateOperationTime(path: String, fileTypeSuffix: String = ".java"): FiniteDuration = {
    val javaFiles = FileOperations.getFileTree(Paths.get(path).toFile).filter(_.getName.endsWith(fileTypeSuffix))
    javaFiles
      .map(file => Files.size(file.toPath))
      .map { fileSize =>
        val time = fileSize / DEFAULT_BYTES_PER_SECOND_RATIO
        time max MINIMAL_TIMEOUT.toSeconds
      }
      .sum
      .seconds
  }

}
