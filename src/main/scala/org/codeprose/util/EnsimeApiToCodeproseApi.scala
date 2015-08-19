package org.codeprose.util

import org.codeprose.api.TypeInformation




object EnsimeApiToCodeproseApi {
  
  // TODO: Translate EnsimeApi outgoing to internal format.
  
  def TypeInspectInfoToTypeInformation(typeInspectInfo: Option[org.ensime.api.TypeInspectInfo]) : Option[TypeInformation] = {

    typeInspectInfo match {
      case Some(tII) => {
      val typeId = tII.`type`.typeId
      val fullname = tII.`type`.fullName
      val interfaces = tII.supers.map ( e => { e.tpe.fullName } ).toList
    
    Some(new TypeInformation(typeId,fullname,interfaces))    
      }
      case None => None
    }
    
  
  }
  
  
  
}