package org.codeprose.consumer

import org.codeprose.api.ProjectInfo

/**
 * Consumer use the information produced by Providers.
 * 
 * How to call?
 *  
 *  1.  initialize()
 *  2.  generateOutput()  
 *  3.  close()
 */
trait Consumer {
  /**
   * Initializes the Consumer. Use to acquire needed resources.
   */
  def initialize() : Unit
  /**
   * Consumer the provided information.
   * @param projectInfo Project information gathered by Providers.
   */
  def generateOutput(projectInfo: ProjectInfo) : Unit
  /**
   * Closes Consumer. Use to free resources. 
   */
  def close() : Unit 
}

/**
 * Provides information to a Consumer.
 */
trait ConsumerContext{
  def verbose: Boolean 
}