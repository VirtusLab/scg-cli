package com.virtuslab.semanticgraphs.javaparser

import com.virtuslab.semanticgraphs.javaparser.matches
import com.virtuslab.semanticgraphs.parsercommon.toPath

import org.eclipse.jgit.fnmatch.FileNameMatcher

import java.nio.file.{FileSystems, Path, PathMatcher}
import scala.io.Source
import scala.util.matching.Regex
import scala.util.Try

/**
  * Uses [[FileNameMatcher]] for patterns to detect ignored path
  * @param patterns
  *   patterns for [[FileNameMatcher]]
  */
case class GraphBuddyIgnore(patterns: Seq[String]) {
  lazy val pathMatchers: Seq[FileNameMatcher] =
    patterns.flatMap(pattern => Try(FileNameMatcher(pattern, null)).toOption)
  def isIgnored(pathInProject: Path): Boolean = pathMatchers.exists(_.matches(pathInProject.toString))
}

object GraphBuddyIgnore {

  def get(projectPath: Path): GraphBuddyIgnore = {
    val lines: Seq[String] = Try {
      val source = Source.fromFile(projectPath.resolve(".graphbuddy.ignore").toFile)
      val lines = source.getLines().toSeq
      source.close()
      lines
    }.toOption.getOrElse(Seq())

    GraphBuddyIgnore(lines)
  }

  def isIgnored(projectPath: Path, filePath: Path): Boolean = get(projectPath).isIgnored(filePath)

}
