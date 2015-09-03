package org.codeprose.consumer


import java.io.File

class HtmlOutputContext(
   val outputMainPath: File,
   val srcFiles: List[File]){
    
  import org.codeprose.util.StringUtil
  val filenamesShortened = StringUtil.getUniqueShortFileNames(srcFiles.map(e => e.getAbsolutePath).toList)
  
  val outputFilenames = filenamesShortened.map(s => outputMainPath.getAbsolutePath + "/content/" + s.replace("/","_") + ".html")
  
  val filenamesOriginalToOutput =  srcFiles.map(e => e.getAbsolutePath).zip(outputFilenames.map(e=> new File(e))).toMap
 
  val filenamesOriginalToShortened = srcFiles.zip(filenamesShortened).toMap
  
  val relativeOutputFilenames = filenamesShortened.map( s=> "/content/" + s.replace("/","_") + ".html")
  
  val filenamesOriginalToRelOutput = srcFiles.zip(relativeOutputFilenames).toMap
  
  
  def getShoretendFilename(srcFile: File) : Option[String] = {
    filenamesOriginalToShortened.get(srcFile)
  } 
  def getShoretendFilename(srcFile: String) : Option[String] = {
    filenamesOriginalToShortened.get(new File(srcFile))
  } 
  
  def getRelativeOutputFilename(srcFile: File) : Option[String] = {
    filenamesOriginalToRelOutput.get(srcFile)
  }
  
  def getRelativeOutputFilename(srcFile: String) : Option[String] = {
    getRelativeOutputFilename(new File(srcFile))
  }
   

}