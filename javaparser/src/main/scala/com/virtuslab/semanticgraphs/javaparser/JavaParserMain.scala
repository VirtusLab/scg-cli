package com.virtuslab.semanticgraphs.javaparser

import com.github.javaparser.StaticJavaParser

import java.io.File
import scala.util.Try

object JavaParserMain:

  // val path = "/Users/kborowski/phd/java-scala-interop"
  // val path = "/Users/kborowski/phd/java-scala-interop/examples"
  val interop = "/Users/kborowski/phd/semantic-code-graph-jvm-interop"

  val path = "/Users/kborowski/phd/akka-new/akka"

  val commons_io = "/Users/kborowski/phd/commons-io"

  val spring_boot = "/Users/kborowski/phd/spring-boot"

  // generateSemanticGraphFiles(commons_io)

  def generateSemanticGraphFiles(
    rootPathString: String
  ): Unit =
    val files: Seq[File] = JavaParser.getSourceFiles(rootPathString)
    val compilationConfiguration = JavaParser.CompilationConfiguration(rootPathString)
    val filePaths = files.map(_.toPath)
    StaticJavaParser.setConfiguration(compilationConfiguration.parserConfiguration)

    val results = filePaths
      .filterNot { path =>
        {
          // path.endsWith("DescriptorProtos.java") ||
          path.toString().contains("src/test") ||
          // path.toString().contains("protobuf") ||
          path.toString.contains(".metals")
        }
      }
      .map { path =>
        println(path)
        Try(
          JavaParser.extractParsedResult(StaticJavaParser.parse(path.toFile), compilationConfiguration.projectPath)
        ).toEither.left
          .map(e => e -> path)
      }

    results.foreach {
      case Right(parsedResult) =>
        println(s"Success ${parsedResult.get.filePath}")
        FileManager.dumpFile(parsedResult.get)
      case Left((e, path)) => println(s"Failure $path; ${e.getMessage()}")
    }

    val failed = results.count(_.isLeft)
    val total = results.size

    println(s"Failure rate: ${failed * 100 / total}")
