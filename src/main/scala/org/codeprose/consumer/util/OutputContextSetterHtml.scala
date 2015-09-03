package org.codeprose.consumer.util

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import org.codeprose.util.FileUtil

/**
 * @author gus
 */




class OutputContextSetter(val outputMain : File) extends LazyLogging {
   
  def copyResource(relPathToFile: String, target: File) : Unit = {
    val content = loadResource(relPathToFile)
    FileUtil.writeToFile(target, content.mkString("\n"))
  }
  def loadResource(relPathToFile: String) : List[String] = {
     val content = scala.io.Source.fromURL(getClass.getResource(relPathToFile),"utf-8").getLines().toList
     content
  }
  
  def setFolderStructure(directories: List[String] ): Unit = {
    logger.info("Setting ouput folder structure ...")
    // Get list of existing folders
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

class OutputContextSetterHtml(outputMain : File) extends OutputContextSetter(outputMain) {
   
}