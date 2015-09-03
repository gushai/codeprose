package org.codeprose.consumer

import org.codeprose.api.ProjectInfo

/*
 * Consumer use the information produced by Providers.
 * 
 * How to call?
 *  
 *  1.  initialize()
 *  2.  generateOutput()  
 *  3.  close()
 */
trait Consumer {
  def initialize() : Unit 
  def generateOutput(projectInfo: ProjectInfo) : Unit
  def close() : Unit 
}


class ConsumerContext(
    verbose: Boolean){}