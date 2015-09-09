package org.codeprose.consumer.util

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import org.codeprose.util.FileUtil

/**
 * Helper to set the output context. 
 * 
 * Includes:
 *  - Setting folders
 *  - Copying resources
 *  - Loading resources 
 * 
 */
class OutputContextSetter(val outputMain : File) {
   
  /**
   * Copies resources to output folder.
   * @param relPathToFile Resource file to be copied.
   * @param target        Name of the file to copy the resource to. 
   */
  def copyResource(relPathToFile: String, target: File) : Unit = {
    val content = loadResourceText(relPathToFile)
    FileUtil.writeToFile(target, content.mkString("\n"))
  }
  
  /**
   * Loads a text resource.
   * @param relPathToFile Path to resource file.
   * @return              List lines in the resource file.
   */
  def loadResourceText(relPathToFile: String) : List[String] = {
     scala.io.Source.fromURL(getClass.getResource(relPathToFile),"utf-8").getLines().toList
  }
  
  /**
   * Sets out folders in outputMain.
   * 
   * @param directories List of filenames relative to outputMain.
   */
  def setFolderStructure(directories: List[String] ): Unit = {
    val existingDirectories = outputMain.listFiles().filter {f => f.isDirectory() }
    directories.foreach { folderName => {
        val idx = existingDirectories.indexWhere { e => e.getAbsolutePath().endsWith(folderName) }
        if(idx == -1){
          FileUtil.createDirectory(new File(outputMain,folderName))
        }
      } 
    } 
  } 
}

/**
 * Output setter for WriterHtml.
 * @param outputMain  Main output folder.
 */
class OutputContextSetterHtml(outputMain : File) extends OutputContextSetter(outputMain)