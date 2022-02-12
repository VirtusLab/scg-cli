package org.virtuslab.semanticgraphs.analytics.utils

import java.nio.file.Paths
import java.nio.file.Path

object PathHelpers:
  implicit class StringResolver(path: String):
    def resolve(other: String): Path =
      Paths.get(path).normalize().resolve(other)
