package com.virtuslab.semanticgraphs.parsercommon.versioning

import com.virtuslab.semanticgraphs.parsercommon.FileOperations

import java.io.File
import java.nio.file.Paths
import scala.io.Source

trait FileVersion {
  def path: String
  def lastModified: Long
  def size: Long
  def contentHashCode: Int

  override def equals(obj: Any): Boolean = obj != null && (obj match {
    case other: FileVersion =>
      path == other.path && (
        lastModified == other.lastModified || (
          size == other.size &&
            contentHashCode == other.contentHashCode
        )
      )
    case _ => false
  })

  override def hashCode(): Int = path.hashCode
}
