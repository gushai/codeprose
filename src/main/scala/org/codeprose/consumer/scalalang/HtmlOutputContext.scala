package org.codeprose.consumer.scalalang


import java.io.File
import org.codeprose.util.StringUtil


/**
 * Helper with information output filenames and shortened file names.
 * @param   outputMainPath  Main output path.
 * @param   srcFiles        List of source files to be processed by the associated Consumer.
 */
class HtmlOutputContext(
   val outputMainPath: File,
   val srcFiles: List[File]){
    
  import org.codeprose.util.StringUtil
  
  /**
   * Shortened filename. Longest common prefix removed from filenames.
   */
  val filenamesShortened = StringUtil.getUniqueShortFileNames(srcFiles.map(e => e.getAbsolutePath).toList)
  
  /**
   * Absolute output filenames.
   */
  val outputFilenames = filenamesShortened.map(s => outputMainPath.getAbsolutePath + "/content/" + s.replace("/","_") + ".html")
  
  /**
   * Map from source filename to output file.
   */
  val filenamesOriginalToOutput =  srcFiles.map(e => e.getAbsolutePath).zip(outputFilenames.map(e=> new File(e))).toMap
 
  /**
   * Map from source filename to shortened file name.
   */
  val filenamesOriginalToShortened = srcFiles.zip(filenamesShortened).toMap
  
  /**
   * List of relative output filenames. 
   */
  val relativeOutputFilenames = filenamesShortened.map( s=> "/content/" + s.replace("/","_") + ".html")
  
  /**
   * Map source file to relative output filename.
   */
  val filenamesOriginalToRelOutput = srcFiles.zip(relativeOutputFilenames).toMap
  
  /**
   * Optionally returns the shortened filename.
   * @param   srcFile Source file.
   * @return          Option shortened filename.
   */
  def getShortenedFilename(srcFile: File) : Option[String] = {
    filenamesOriginalToShortened.get(srcFile)
  } 
  
  /**
   * Optionally returns the shortened filename.
   * @param   srcFile Source filename.
   * @return          Option shortened filename.
   */
  def getShortenedFilename(srcFile: String) : Option[String] = {
    filenamesOriginalToShortened.get(new File(srcFile))
  } 
  
  /**
   * Optionally returns the output filename.
   * @param   srcFile Source file.
   * @return          Option shortened filename.
   */
  def getRelativeOutputFilename(srcFile: File) : Option[String] = {
    filenamesOriginalToRelOutput.get(srcFile)
  }
  
  /**
   * Optionally returns the output filename.
   * @param   srcFile Source filename.
   * @return          Option output filename.
   */
  def getRelativeOutputFilename(srcFile: String) : Option[String] = {
    getRelativeOutputFilename(new File(srcFile))
  }

}