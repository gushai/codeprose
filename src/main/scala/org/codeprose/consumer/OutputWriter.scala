package org.codeprose.consumer

import java.io.File
import scalariform.lexer.Token
//
///** The contract required to handle a specific format */
//trait OutputWriter {
//
//  /** Generates initial content. */
//  def writeStart(): Unit
//
//  /** Generates content for a given source file. */
//  def writeUnit(sourceFile: File, relativeSourcePath: String, tokenList: List[Token]): Unit
//
//  /** Generates final content. */
//  def writeEnd(): Unit
//}
//
//
///** The initial data passed at writer construction */
//class OutputWriterContext(val sourceFiles: List[File],
//  val outputDirectory: File,
//  val encoding: String,
//  val localIndex: MapIndex)
//
//class OutputInfo(val outputDirectory: File, val outputFileExtension: String)
//{
//  def getOutputFile(relativeSourcePath: String) =
//    new File(outputDirectory, relativeSourcePath + outputFileExtension)
//}