package org.codeprose.util

import spray.json._

import org.codeprose.api.TypeInformation

object CodeproseJsonFormat extends DefaultJsonProtocol {
  implicit val typeInformationFormat = jsonFormat3(TypeInformation)
}