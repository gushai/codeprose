package org.codeprose.consumer

import org.codeprose.api.ProjectInfo

trait Consumer {
  def generateOutput(projectInfo: ProjectInfo) : Unit
}


class ConsumerContext(
    verbose: Boolean){}