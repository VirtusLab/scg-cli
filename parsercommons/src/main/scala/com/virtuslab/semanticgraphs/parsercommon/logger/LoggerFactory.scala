package com.virtuslab.semanticgraphs.parsercommon.logger

import ch.qos.logback.classic.{Level, Logger, LoggerContext}
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.{Appender, ConsoleAppender}
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.rolling.{RollingFileAppender, TimeBasedRollingPolicy}
import com.typesafe.config.{Config, ConfigFactory}

object LoggerFactory {
  private var fileAppenders: Map[String, Appender[ILoggingEvent]] = Map()

  private val loggerConfig: Config = // I am also surprised, that it must be so complicated. But it didn't work in simpler configurations
    ConfigFactory.parseURL(getClass.getClassLoader.getResource("logger.conf")).getConfig("logger")

  private val isDevelop = Option(System.getenv("GRAPHBUDDY_DEVELOPMENT")).contains("true")
  private val logsBaseFileName = loggerConfig.getString("file")
  private val logLevel: Level = Level.toLevel(loggerConfig.getString("level"), Level.ERROR)
  private val logToFile = loggerConfig.getBoolean("logToFile")
  private val logToConsole = loggerConfig.getBoolean("logToConsole")
  private val logsFileName = if (isDevelop) s"dev-$logsBaseFileName" else logsBaseFileName

  def getLogger(clazz: Class[_], fileName: String = logsFileName): Logger = {
    val logger = org.slf4j.LoggerFactory.getLogger(clazz.getName).asInstanceOf[Logger]
    if (logToFile) {
      logger.addAppender(getFileAppender(fileName))
    }
    if (logToConsole) {
      logger.addAppender(consoleAppender)
    }
    logger.setLevel(logLevel)
    logger
  }

  private lazy val consoleAppender: Appender[ILoggingEvent] = {
    val logCtx = org.slf4j.LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val logEncoder = PatternLayoutEncoder()
    logEncoder.setContext(logCtx)
    logEncoder.setPattern("%-12date{YYYY-MM-dd HH:mm:ss.SSS} %-5level - %msg%n")
    logEncoder.start()

    val logConsoleAppender = ConsoleAppender()
    logConsoleAppender.setContext(logCtx)
    logConsoleAppender.setName("console")
    logConsoleAppender.setEncoder(logEncoder.asInstanceOf[Encoder[Nothing]])
    logConsoleAppender.setTarget("System.err")
    logConsoleAppender.start()

    logConsoleAppender.asInstanceOf[Appender[ILoggingEvent]]
  }

  private def getFileAppender(filename: String): Appender[ILoggingEvent] = {
    if (fileAppenders != null && fileAppenders.contains(filename)) {
      return fileAppenders(filename)
    }

    val logCtx = org.slf4j.LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val logEncoder = PatternLayoutEncoder()
    logEncoder.setContext(logCtx)
    logEncoder.setPattern("%d{yyyy-MM-dd HH:mm:ss.SSS} [%-5level] [%t] %c{1} - %msg%n")
    logEncoder.start()

    val logFileAppender = RollingFileAppender()
    logFileAppender.setContext(logCtx)
    logFileAppender.setFile(s"${System.getProperty("user.home")}/.graphbuddy/$filename")
    logFileAppender.setName(filename)
    logFileAppender.setEncoder(logEncoder.asInstanceOf[Encoder[Nothing]])
    logFileAppender.setAppend(true)
    logFileAppender.setImmediateFlush(true)

    val logFilePolicy = TimeBasedRollingPolicy()
    logFilePolicy.setContext(logCtx)
    logFilePolicy.setParent(logFileAppender)
    logFilePolicy.setFileNamePattern(
      s"${System.getProperty("user.home")}/.graphbuddy/logs/$filename-%d{yyyy-MM-dd_HH}.log"
    )
    logFilePolicy.setMaxHistory(7)
    logFilePolicy.start()

    logFileAppender.setRollingPolicy(logFilePolicy)
    logFileAppender.start()
    if (fileAppenders == null) {
      fileAppenders = Map(filename -> logFileAppender.asInstanceOf[Appender[ILoggingEvent]])
    } else {
      fileAppenders = fileAppenders ++ Map(filename -> logFileAppender.asInstanceOf[Appender[ILoggingEvent]])
    }

    logFileAppender.asInstanceOf[Appender[ILoggingEvent]]
  }

}
