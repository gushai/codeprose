package org.codeprose.util

import java.io.File

/**
 * File utilities.
 */
object FileUtil {

  /**
   * Indicates if file exists and ends with ending.
   * @param file    File to test.
   * @param ending  File ending to test. 
   * @return        Boolean indicator.
   */
  private def isFileWithEnding(file: File, ending: String) : Boolean = {
    return file.exists && file.getName.endsWith(ending)
  }
  
  /**
   * Indicates if file is a Scala file.
   * @param   file  File to check.
   * @return        Boolean indicator for Scala file.
   */
  private def isScalaFile(file: File) : Boolean = {
    isFileWithEnding(file, ".scala")
  }
  
  /**
   * Returns an array of all Scala files in a directory (incl. all sub directories).
   * @param   directory Directory to search for scala files.
   * @return            Array of scala files found.    
   */
  def getAllScalaFilesIncludingSubDir(directory: File) : Array[File] = {    
    return recursiveListFiles(directory,".scala")
  }
 
  /**
   * Returns an array of all files in a directory (incl. all sub directories) that end with ending.
   * @param   directory Directory to search for scala files.
   * @return            Array of files found ending in ending.    
   */
  private def recursiveListFiles(f: File, ending: String): Array[File] = {
    val these = f.listFiles
    val good = f.listFiles.filter(f => isFileWithEnding(f,ending))
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_,ending))
  }
 
  /**
   * Write content to file.
   * @param   file    File to write to.
   * @param   content Content to write to file.
   */
  def writeToFile(file: File, content: String): Unit = {    
    val pw = new java.io.PrintWriter(file)
    try pw.write(content) finally pw.close()
  }
  
  /**
   * Loads content from file to one string.
   * 
   * Reads "utf-8" encoded content.
   * 
   * @param file  File to load.
   * @return      File content.  
   */
  def loadSrcFileContent(file: File) : String = {
    scala.io.Source.fromFile(file.getAbsolutePath(), "utf-8").getLines.mkString("\n")
  }
  
  /**
   * Creates a directory.
   * @param path  Directory to create.
   */
  def createDirectory(path: File) : Unit = {
    path.mkdir()
  }
  
}

