package org.codeprose.broker

import org.codeprose.api.ProjectInfo


/*
 * Central point in the application. Requests information on project from
 * Provider and hands them off to the consumers.
 * 
 * How to call?
 * 
 *  1.  initialize()
 *  2.  analyzeSourceCode()  
 *  3.  generateOutput()    
 *  4.  close()
 *
 *
 * How to structure a codeprose application?
 * 
 *  The Broker acts as a entry point in a codeprose application.
 *  A Broker uses n>0 Provider to gather information about a project, 
 *  which is consumed by k>0 Consumer.  
 * 
 *  Examples:
 *  
 *  - a. Simple Provider and Consumer
 *  
 *    Broker -> Provider
 *    Broker -> Consumer
 *   
 *  - b. Piped Provider and Consumer
 *  
 *    Broker -> Provider 1 -> -> ... -> Provider n
 *    Broker -> Consumer 1
 *    Broker -> Consumer k
 *  
 * How to exchange information? 
 *  
 *  Information between Provider <-> Broker <-> Consumer is exchanged via 
 *  a org.codeprose.api.ProjectInfo object, which is very versatile and can  
 *  be tailored to many different objectives.
 *  
 * 
 *  
 */
trait Broker {
  def initialize() : Unit
  def analyzeSourceCode() : ProjectInfo  
  def generateOutput(projectInfo: ProjectInfo) : Unit    
  def close() : Unit
}