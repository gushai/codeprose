package org.codeprose.api.scalalang

case class SourceSample(
    srcPos: ERangePositionWithTokenId,
    srcCodeLines: List[String]){}