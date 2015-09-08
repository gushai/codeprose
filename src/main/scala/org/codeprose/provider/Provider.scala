package org.codeprose.provider

import java.io.File
import org.codeprose.api.ProjectInfo

/*
 * Central component in the codeprose structure.
 * 
 * Providers are used to gather information about a project's 
 * source code. 
 * 
 * How to call?
 *  
 *  1.    initialize() 
 *  2.a.  If starting from a list of files: getProjectInformation()
 *  2.b   If enriching an existing projectInfo provided 
 *        by an other Provider: enrichProjectInformation()
 *  3.    close()
 * 
 */
trait Provider {
  /**
   * Initializes the Provider. Use to acquire needed resources.
   */
	def initialize() : Unit
  /**
   * Generate information about a project.
   * 
   * Please note which information the Provider collects and what properties of 
   * the returned projectInfo can be assumed to hold.
   * 
   * @param files List of files to process. 
   * @return      ProjectInfo containing the information collected by the Provider.
   */
	def getProjectInformation(files: List[File]) : ProjectInfo
   /**
   * Enriches a projectInfo.
   * 
   * Please note:
   * - The information the Provider requires to inside the input projectInfo.
   * - The information the Provider collects and what properties of 
   *  the returned projectInfo can be assumed to hold.
   * 
   * @param projectInfo ProjectInfo provided by another Provider. 
   * @return            ProjectInfo containing the information collected by the Provider.
   */
  def enrichProjectInformation(projectInfo: ProjectInfo) : ProjectInfo
   /**
   * Closes Provider. Use to free resources. 
   */
  def close() : Unit      
}

/**
 * ProviderContext are used to provide information to Providers.
 */
trait ProviderContext { 
  def verbose: Boolean 
}