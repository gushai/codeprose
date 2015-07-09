package org.codeprose.consumer

import org.codeprose.api.Token

trait Consumer {
  def generateOutput(info:
      scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.ArrayBuffer[Token])]
  ) : Unit
}