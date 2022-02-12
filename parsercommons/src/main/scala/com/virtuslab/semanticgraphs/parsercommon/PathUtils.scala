package com.virtuslab.semanticgraphs.parsercommon

import java.io.File
import java.nio.file.{Path, Paths}

extension (string: String) {
  def toPath: Path = Paths.get(string)
  def toFile: File = string.toPath.toFile
}
