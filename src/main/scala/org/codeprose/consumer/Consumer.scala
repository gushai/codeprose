package org.codeprose.consumer

import scalariform.lexer.Token

trait Consumer {
  def generateOutput(info:
      scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.Map[Int,(Token,List[(String, String)] )])]
  ) : Unit
}