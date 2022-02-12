package com.virtuslab.semanticgraphs.javaparser

import com.virtuslab.semanticgraphs.javaparser.extractor.`enum`.EnumExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.classorinterface.ClassExtractor
import com.virtuslab.semanticgraphs.javaparser.extractor.utils.*
import com.virtuslab.semanticgraphs.javaparser.solver.{DummyExternalDependenciesTypeSolver, EDMTSCompatibleSymbolSolver}
import com.virtuslab.semanticgraphs.javaparser.FileManager.*
import com.virtuslab.semanticgraphs.javaparser.JavaParserWithCaches
import com.virtuslab.semanticgraphs.parsercommon.{toPath, FileOperations}
import com.virtuslab.semanticgraphs.parsercommon.logger.GraphBuddyLogging
import com.virtuslab.semanticgraphs.parsercommon.versioning.{FileVersion, FileVersionFromFile, FileVersionStamp}
import com.virtuslab.semanticgraphs.proto.model.graphnode.{GraphNode, Location, SemanticGraphFile}

import com.github.javaparser.{JavaParser, ParserConfiguration, StaticJavaParser}
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, EnumDeclaration}
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.symbolsolver.resolution.typesolvers.{
  CombinedTypeSolver,
  JavaParserTypeSolver,
  ReflectionTypeSolver
}
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import com.github.javaparser.utils.SourceRoot
import com.github.javaparser.ParserConfiguration.LanguageLevel

import java.io.{File, InputStream, StringReader}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicInteger
import java.util.regex.Pattern
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.{Failure, Success, Try}

object JavaParser extends GraphBuddyLogging {

  private val filesystemPathSeparator: String = File.separator

  private def createParserConfiguration(rootPathString: String): ParserConfiguration = {
    val parserWithCaches = JavaParserWithCaches()

    val combinedTypeSolver = CombinedTypeSolver()

    val reflectionTypeSolver = ReflectionTypeSolver()
    combinedTypeSolver.add(reflectionTypeSolver)

    val dummyTypeSolver = DummyExternalDependenciesTypeSolver()

    val javaParserTypeSolvers = getSourceDirectories(rootPathString)(Some(parserWithCaches), Some(dummyTypeSolver))
      .map(parserWithCaches.buildJavaParserTypeSolver)
    javaParserTypeSolvers.foreach(solver => combinedTypeSolver.add(solver))

    combinedTypeSolver.add(dummyTypeSolver)

    val symbolSolver = JavaSymbolSolver(combinedTypeSolver)
    val wrappedSymbolSolver = EDMTSCompatibleSymbolSolver(symbolSolver, dummyTypeSolver)

    new ParserConfiguration().setSymbolResolver(wrappedSymbolSolver).setLanguageLevel(LanguageLevel.CURRENT)
  }

  /**
    * Extract and resolve CompilationUnit given in parseResult. Extracted class is wrapped in FileToBeSaved
    * @param parseResult
    *   result of compilation returned by java parser
    * @param projectPath
    *   project path to relativize path
    * @param filePathOption
    *   if not empty, will be used as file path. Otherwise, path will be obtained from compilation unit
    * @return
    *   Extracted result wrapped in `FileToBeSaved`, in format ready to be dumped on disk
    */
  def extractParsedResult(
    parseResult: CompilationUnit,
    projectPath: String,
    filePathOption: Option[String] = None
  ): Option[FileToBeSaved] = for {
    filePath <- filePathOption.orElse(parseResult.getStorage.toScala.map(storage => storage.getPath.toString))
    uri = filePath.stripPrefix(projectPath)
    nodes = List(createFileNode(uri, parseResult)) ++ ClassExtractor
      .createClass(parseResult.allNodesOf[ClassOrInterfaceDeclaration], uri) ++
      EnumExtractor.createNodes(parseResult.allNodesOf[EnumDeclaration], uri)
    semanticGraphFile = SemanticGraphFile(uri, nodes)
  } yield FileToBeSaved(projectPath, semanticGraphFile, filePath)

  /**
    * Just to have total LOC of each file
    *
    * @param file
    * @param parseResult
    * @return
    */
  private def createFileNode(
    file: String,
    parseResult: CompilationUnit
  ): GraphNode = {
    GraphNode(
      id = file,
      kind = "FILE",
      location = Some(Location(uri = file)),
      properties = Map(
        "LOC" -> parseResult.getRange.toScala.map(range => range.end.line - range.begin.line).getOrElse(0).toString
      ),
      displayName = file
    )
  }

  /**
    * Get all Java files in directory and subdirectories
    * @param rootPathString
    *   directory to search
    * @return
    *   all `.java` files in directory and subdirectories
    */
  def getSourceFiles(rootPathString: String): Seq[File] = {
    val projectPath = rootPathString.toPath
    val graphBuddyIgnore = GraphBuddyIgnore.get(projectPath)
    FileOperations
      .getFileTree(projectPath.toFile)
      .filter(path =>
        path.getName.endsWith(".java") && !graphBuddyIgnore.isIgnored(projectPath.relativize(path.toPath))
      )
  }

  /**
    * @param rootPathString
    *   the path for which you want to generate source roots
    * @return
    *   a Seq of source root paths
    */
  private def getSourceDirectories(
    rootPathString: String
  )(implicit
    maybeParser: Option[JavaParserWithCaches] = None,
    maybeDummyTypeSolver: Option[DummyExternalDependenciesTypeSolver] = None
  ): Seq[Path] = getSourceFiles(rootPathString)
    .map { file =>
      val parentPath = file.toPath.dropNameElementsRight(1)
      val packageLevelPath = parentPath.stripSuffix(
        file.parseJavaPackageDeclarationAndUpdateResolverCache.map(_.asPath).getOrElse("".toPath)
      )
      if packageLevelPath.endsWithAValidJavaIdentifier then packageLevelPath
      else parentPath
    }
    .filter(_.toFile.isDirectory)
    .distinct

  case class CompilationConfiguration(projectRootPathString: String) {
    val parserConfiguration: ParserConfiguration = createParserConfiguration(projectRootPathString)
    val projectPath: String = s"${projectRootPathString.stripSuffix(filesystemPathSeparator)}$filesystemPathSeparator"
  }

  def generateSemanticGraphFile(
    fileContent: String,
    filePathString: String,
    rootPathString: String
  ): Option[FileToBeSaved] = {
    if (GraphBuddyIgnore.isIgnored(rootPathString.toPath, filePathString.toPath)) return None
    val compilationConfiguration = CompilationConfiguration(rootPathString)
    StaticJavaParser.setConfiguration(compilationConfiguration.parserConfiguration)
    val parsedFile: CompilationUnit = StaticJavaParser.parse(fileContent)
    extractParsedResult(parsedFile, compilationConfiguration.projectPath, Some(filePathString))
  }

//  def generateSemanticGraphFiles(
//    rootPathString: String,
//    maybeProgressListener: Option[ProgressListener] = None
//  ): Task[Seq[FileToBeSaved]] = {
//    maybeProgressListener.foreach(_.setIndeterminate(true))
//    val files: Seq[File]         = getSourceFiles(rootPathString)
//    val compilationConfiguration = CompilationConfiguration(rootPathString)
//    val filePaths                = files.map(_.toPath)
//    StaticJavaParser.setConfiguration(compilationConfiguration.parserConfiguration)
//
//    maybeProgressListener.foreach(_.setIndeterminate(false))
//    val progressReporter = ProgressReporter(
//      maybeProgressListener,
//      filePaths.length
//    )
//
//    Task
//      .parSequence {
//        filePaths.zipWithIndex.flatMap { (filePath, index) =>
//          Try(StaticJavaParser.parse(filePath.toFile)) match {
//            case Failure(exception) =>
//              progressReporter.reportNewElement()
//              None
//            case Success(parsedResult) => Some(
//                Task {
//                  val result = extractParsedResult(parsedResult, compilationConfiguration.projectPath)
//                  progressReporter.reportNewElement()
//                  result
//                }
//              )
//          }
//        }
//      }
//      .map(_.flatten)
//  }

  def generateAndSaveSemanticGraphFile(
    fileContent: String,
    filePath: String,
    projectPath: String
  ): Option[FileToBeSaved] = {
    val maybeFile = generateSemanticGraphFile(fileContent, filePath, projectPath).map { file =>
      FileManager.dumpFile(file)
      file
    }
    maybeFile.foreach { fileToBeSaved =>
      val version = FileVersionFromFile(filePath.toPath.toFile).toStamp.copy(contentHashCode = fileContent.hashCode)
      FileVersionStamp.upsertState(projectPath.toPath, Seq(version))
    }
    maybeFile
  }

//  def generateAndSaveSemanticGraphFiles(
//    path: String,
//    maybeProgressListener: Option[ProgressListener] = None
//  ): Task[Seq[String]] = generateSemanticGraphFiles(path, maybeProgressListener)
//    .map(_.map { fileToDump =>
//      FileManager.dumpFile(fileToDump)
//      fileToDump.filePath
//    })
//    .tapEval { processedFiles =>
//      Task {
//        val fileVersions = processedFiles.map(file => FileVersionFromFile(file.toPath.toFile).toStamp)
//        FileVersionStamp.upsertState(path.toPath, fileVersions)
//      }
//    }

}
