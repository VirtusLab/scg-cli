package com.virtuslab.semanticgraphs.parsercommon

import java.io.File
import scala.io.Source

object FileOperations {

  /**
    * @return
    *   a list of files in given directory recursively
    *
    * @param file
    *   file which is root of search
    */
  def getFileTree(file: File): List[File] =
    file :: (if (file.isDirectory) Option(file.listFiles).map(_.toList.flatMap(getFileTree)).getOrElse(List.empty)
             else List.empty)

  /**
    * Read file content
    * @param file
    *   file to be read, must exist
    * @return
    *   content of file as String
    */
  def readFile(file: File): String = {
    val s = Source.fromFile(file)
    try s.mkString
    finally s.close()
  }

}
