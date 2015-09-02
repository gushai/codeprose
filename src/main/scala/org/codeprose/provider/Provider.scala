package org.codeprose.provider

import java.io.File
import org.codeprose.api.ProjectInfo

/*
 * Central component in the codeprose strucutre.
 * 
 * Provider are used to gather information about a project/src code. 
 * 
 * How to call?
 *  
 *  1.  initialize() 
 *  2.  getProjectInformation()
 *  3.  close()
 * 
 */
trait Provider {
	def initialize() : Unit 
	def getProjectInformation(files: List[File]) : ProjectInfo 
  def close() : Unit      
}



trait ProviderContext { val verbose: Boolean }