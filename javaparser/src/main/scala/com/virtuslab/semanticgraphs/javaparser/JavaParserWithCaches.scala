package com.virtuslab.semanticgraphs.javaparser

import com.github.javaparser.{JavaParser, ParserConfiguration}
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.symbolsolver.cache.{Cache, GuavaCache}
import com.github.javaparser.symbolsolver.resolution.typesolvers.JavaParserTypeSolver
import com.github.javaparser.ParseStart.COMPILATION_UNIT
import com.github.javaparser.Providers.provider
import com.google.common.cache.CacheBuilder

import java.io.IOException
import java.nio.file.{Files, Path}
import java.util.Optional
import scala.jdk.OptionConverters.*

class JavaParserWithCaches() {
  private def buildCache[K, V](): Cache[K, V] = GuavaCache(CacheBuilder.newBuilder().softValues().build())
  private val parserConfiguration =
    new ParserConfiguration().setLanguageLevel(ParserConfiguration.LanguageLevel.BLEEDING_EDGE)

  val filesCache: Cache[Path, Optional[CompilationUnit]] = buildCache()

  val directoriesCache: Cache[Path, java.util.List[CompilationUnit]] = buildCache()

  val javaParser: JavaParser = new JavaParser(
    parserConfiguration
  )

  def parse(srcFile: Path): Option[CompilationUnit] = {
    try { // Copied from JavaParserTypeSolver
      val cachedParsedFile = filesCache.get(srcFile.toAbsolutePath)
      // If the value is already cached
      if (cachedParsedFile.isPresent) return cachedParsedFile.get.toScala
      // Otherwise load it
      if (!Files.exists(srcFile) || !Files.isRegularFile(srcFile)) {
        filesCache.put(srcFile.toAbsolutePath, Optional.empty)
        return None
      }
      // JavaParser only allow one parse at time.
      javaParser.synchronized {
        val compilationUnit = javaParser
          .parse(COMPILATION_UNIT, provider(srcFile))
          .getResult
          .map((cu: CompilationUnit) => cu.setStorage(srcFile))
        filesCache.put(srcFile.toAbsolutePath, compilationUnit)
        compilationUnit.toScala
      }
    } catch {
      case e: IOException =>
        throw new RuntimeException("Issue while parsing while type solving: " + srcFile.toAbsolutePath, e)
    }
  }

  def buildJavaParserTypeSolver(path: Path): JavaParserTypeSolver =
    new JavaParserTypeSolver(path, javaParser, filesCache, directoriesCache, buildCache())
}
