package org.codeprose.util

import spray.json._

import org.codeprose.api.TypeInformation
import org.codeprose.api.ERangePositionWithTokenIds

object CodeproseJsonFormat extends DefaultJsonProtocol {
  implicit val typeInformationFormat = jsonFormat3(TypeInformation)
  implicit val ERangePositionWithTokenIdsFormat = jsonFormat5(ERangePositionWithTokenIds)
    
}