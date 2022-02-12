package com.virtuslab.semanticgraphs.javaparser.extractor.utils

import ch.qos.logback.classic.Logger

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.{Failure, Success, Try}

enum LogLevel {

  def getLogMethod(using
    logger: Logger
  ): String => Unit = {
    this match {
      case TRACE => logger.trace
      case DEBUG => logger.debug
      case INFO  => logger.info
      case WARN  => logger.warn
      case ERROR => logger.error
    }

  }

  case TRACE, DEBUG, INFO, WARN, ERROR
}

extension [T](t: Try[T])(using
  logger: Logger
) {
  def toSeq: Seq[T] = t.toOption.toSeq

  def withLoggingOption(
    context: String = "",
    stackTraceFramesToSkip: Int = 1,
    logLevel: LogLevel = LogLevel.DEBUG
  ): Option[T] = t.withLogging(context, stackTraceFramesToSkip, logLevel).toOption

  def withLogging(
    context: String = "",
    stackTraceFramesToSkip: Int = 1,
    logLevel: LogLevel = LogLevel.DEBUG
  ): Try[T] = {
    t match {
      case failure @ Failure(exception) =>
        val errorLog = s"${if (context.nonEmpty) s"context\n" else ""}${exception.toString}"
        StackWalker
          .getInstance()
          .walk(frames => {
            frames
              .skip(stackTraceFramesToSkip)
              .findFirst() // correctly shows the exception source in the code
              .toScala
              .foreach { frame =>
                logLevel.getLogMethod {
                  s"$errorLog\n${frame.getClassName}:${frame.getMethodName} (line ${frame.getLineNumber})"
                }
              }
          })
        failure
      case success @ Success(_) => success
    }
  }

}
