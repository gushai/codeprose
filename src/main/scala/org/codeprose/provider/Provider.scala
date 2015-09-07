package org.codeprose.provider

import java.io.File
import org.codeprose.api.ProjectInfo

/*
 * Central component in the codeprose structure.
 * 
 * Providers are used to gather information about a project's 
 * soure code. 
 * 
 * How to call?
 *  
 *  1.    initialize() 
 *  2.a.  If starting from a list of files: getProjectInformation()
 *  2.b   If enriching an exisiting project information provided 
 *        by an other Provider: enrichProjectInformation()
 *  3.    close()
 * 
 */
trait Provider {
	def initialize() : Unit 
	def getProjectInformation(files: List[File]) : ProjectInfo
  def enrichProjectInformation(projectInfo: ProjectInfo) : ProjectInfo
  def close() : Unit      
}


trait ProviderContext { val verbose: Boolean }