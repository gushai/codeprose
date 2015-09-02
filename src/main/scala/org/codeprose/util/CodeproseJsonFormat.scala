package org.codeprose.util

import spray.json._
import org.codeprose.api._
import org.codeprose.api.scalalang._
import com.typesafe.scalalogging.LazyLogging

object CodeproseJsonFormat extends DefaultJsonProtocol with LazyLogging {
  
  
  implicit val SourcePositionLinkWithCodeSampleFormat = jsonFormat4(SourcePositionLinkWithCodeSample)
  
  implicit val OffsetSourcePositionWithTokenIdFormat = jsonFormat3(OffsetSourcePositionWithTokenId)
  
  
  implicit object DeclaredAsFormat extends RootJsonFormat[DeclaredAs] {
    def write(declAs: DeclaredAs) = {

      import org.codeprose.api.scalalang.DeclaredAs._
      
      declAs match {
        case Method => JsString("method") 
        case Trait => JsString("trait") 
        case Interface => JsString("interface") 
        case Object => JsString("object") 
        case Class => JsString("class") 
        case Field => JsString("field") 
        case Nil   => JsString("nil") 
      }
      
      
      
    }
    def read(value: JsValue) =  ??? 
  }
  
  
  
  implicit object EntityInfoFormat extends RootJsonFormat[EntityInfo] {
    def write(entity: EntityInfo) = { 
      
      entity match {
        case pI : PackageInfo => {
          CodeproseJsonFormat.PackageInfoFormat.write(pI)
        }
        case namedTypeMem : NamedTypeMemberInfo => {
          CodeproseJsonFormat.NamedTypeMemberInfoFormat.write(namedTypeMem)
        }
        case typeInfo: TypeInfo => {
          CodeproseJsonFormat.TypeInfoFormat.write(typeInfo)
        }
        case _ => {
          logger.error("Unknown EntityInfo type!")
          val v = entity.members.map(e=>CodeproseJsonFormat.EntityInfoFormat.write(e)).toList
          val members =JsArray(v)
          JsObject("name" -> entity.name.toJson,
          "members" -> members)
        }   
      }
      
      
      
    }
    def read(value: JsValue) =  ??? 
  }
  
   implicit object PackageInfoFormat extends RootJsonFormat[PackageInfo] {
    def write(pi: PackageInfo) = {
      
      val members = if(pi.members == List.empty){
        JsArray()
      } else {
        val v = pi.members.map(e=>CodeproseJsonFormat.EntityInfoFormat.write(e)).toList
        JsArray(v)
      }
      
      JsObject("_infoType" -> pi._infoType.toJson, 
               "name" -> pi.name.toJson,
               "fullName" -> pi.fullName.toJson,
               "members" -> members)
      
    }
    def read(value: JsValue) =  ??? 
  }
  
   implicit object NamedTypeMemberInfoFormat extends RootJsonFormat[NamedTypeMemberInfo] {
    def write(namedTypeMemInfo:  NamedTypeMemberInfo) = {
     
      val typeInfo = CodeproseJsonFormat.TypeInfoFormat.write(namedTypeMemInfo.tpe)
      val pos = namedTypeMemInfo.pos.toJson
      val declAs = CodeproseJsonFormat.DeclaredAsFormat.write(namedTypeMemInfo.declAs)
      JsObject("_infoType" -> namedTypeMemInfo._infoType.toJson, 
               "name" -> namedTypeMemInfo.name.toJson,
               "tpe" -> typeInfo,
               "pos" -> pos,
               "signatureString" -> namedTypeMemInfo.signatureString.toJson,
               "declAs" -> declAs
               )
      
    }
    def read(value: JsValue) =  ??? 
  }
   
   implicit object SourcePositionFormat extends RootJsonFormat[SourcePosition] {
    def write(srcPos: SourcePosition) = {
      
      srcPos match {
        case pos: OffsetSourcePositionWithTokenId => {
          pos.toJson
        }
        case _ => {
          logger.error("Unknown SourcePosition!")
          JsObject() }
      }
    }
    def read(value: JsValue) =  ??? 
  }
   
   
   
  implicit object TypeInfoFormat extends RootJsonFormat[TypeInfo] {
    def write(ti: TypeInfo) = {
     ti match {
       case basic : BasicTypeInfo => {
         CodeproseJsonFormat.BasicTypeInfoFormat.write(basic)
       }
       case arrow : ArrowTypeInfo => {
         CodeproseJsonFormat.ArrowTypeInfoFormat.write(arrow)
      }
     }
      
  }
    def read(value: JsValue) =  ??? 
  }
  
  implicit object BasicTypeInfoFormat extends RootJsonFormat[BasicTypeInfo] {
    def write(basic: BasicTypeInfo) = {
      val typeArgs = if(basic.typeArgs == List.empty){
           JsArray()
         } else {
           val v = basic.typeArgs.map(e=>CodeproseJsonFormat.TypeInfoFormat.write(e)).toList
           JsArray(v)
         }
         
         val members = if(basic.members == List.empty){
           JsArray()
         } else {
           val v = basic.members.map( e => CodeproseJsonFormat.EntityInfoFormat.write(e)).toList
           JsArray(v)
         }
         val declAs = CodeproseJsonFormat.DeclaredAsFormat.write(basic.declAs)
         val pos = basic.pos.toJson 
         JsObject("_infoType" -> basic._infoType.toJson, 
                  "name" ->   basic.name.toJson,
                  "typeId" -> basic.typeId.toJson,
                  "declAs" -> declAs,
                  "fullName" -> basic.fullName.toJson,
                  "typeArgs" -> typeArgs,
                  "members" -> members,
                  "pos" -> pos,
                  "outerTypeId" -> basic.outerTypeId.toJson
                  )
    }
    def read(value: JsValue) =  ??? 
  }
  
  implicit object ArrowTypeInfoFormat extends RootJsonFormat[ArrowTypeInfo] {
    def write(arrow: ArrowTypeInfo) = {
      val paramSections = if(arrow.paramSections == List.empty){
           JsArray()
         } else {
           val v = arrow.paramSections.map( e => CodeproseJsonFormat.ParamSectionInfoFormat.write(e)).toList
           JsArray(v)
         }
         
         val resultType = CodeproseJsonFormat.TypeInfoFormat.write(arrow.resultType)
         
         JsObject("_infoType" -> arrow._infoType.toJson, 
                  "name" ->  arrow.name.toJson,
                  "typeId" -> arrow.typeId.toJson,
                  "resultType" -> resultType,
                  "paramSections" -> paramSections
                  )
       }
    
    def read(value: JsValue) =  ??? 
  }
  
  implicit val ParamSectionInfoFormat = jsonFormat2(ParamSectionInfo)
  implicit val InterfaceInfoFormat = jsonFormat2(InterfaceInfo)
  
  implicit val TypeInspectInfoFormat = jsonFormat3(TypeInspectInfo)
  implicit val SymbolInfoFormat = jsonFormat6(SymbolInfo)
  
  
  implicit object ImplicitInfoFormat extends RootJsonFormat[ImplicitInfo] {
    def write(implInfo: ImplicitInfo) = {
      implInfo match {
        case implConv : ImplicitConversionInfo => {
          JsObject("_infoType"->implConv._infoType.toJson)  
        }
        case implParam : ImplicitParamInfo => {
          JsObject("_infoType"->implParam._infoType.toJson)  
        }
        case _ => JsObject()
      }
           
    }
    def read(value: JsValue) =  ??? 
  }
  
  // OLD  
  // continue with https://github.com/spray/spray-json#jsonformats-for-recursive-types
  // Example: http://stackoverflow.com/questions/16431545/how-can-provide-jsonformats-for-case-class-that-references-itself
  //implicit val TypeInfoFormat : JsonFormat[TypeInfo] = lazyFormat(jsonFormat(TypeInfo, "name","typeId","declAs","fullName","typeArgs","members","pos","outerTypeId"))
  //  implicit val BasicTypeInfoFormat : JsonFormat[BasicTypeInfo] = lazyFormat(jsonFormat(BasicTypeInfo, "name","typeId","declAs","fullName","typeArgs","members","pos","outerTypeId"))
//    implicit val ArrowTypeInfoFormat : JsonFormat[ArrowTypeInfo] = lazyFormat(jsonFormat(ArrowTypeInfo, "name","typeId","resultType","paramSections"))
//  implicit val TypeInspectInfoFormat : JsonFormat[TypeInspectInfo] = lazyFormat(jsonFormat(TypeInspectInfo,"tpe","companionId","interfaces"))
//  implicit val InterfaceInfoFormat : JsonFormat[InterfaceInfo] = lazyFormat(jsonFormat(InterfaceInfo,"tpe","viaView"))
 //   implicit val ParamSectionInfoFormat : JsonFormat[ParamSectionInfo] = lazyFormat(jsonFormat(ParamSectionInfo,"params","isImplicit"))
//  
//  implicit val NamedTypeMemberInfoFormat : JsonFormat[NamedTypeMemberInfo] = lazyFormat(jsonFormat(NamedTypeMemberInfo,"name","tpe","pos","signatureString","declAs"))
//  implicit val EntityInfoFormat : JsonFormat[EntityInfo] = lazyFormat(jsonFormat(EntityInfo,"name","members"))
//  
  
  
  //  implicit val ArrowTypeInfoFormat = jsonFormat4(ArrowTypeInfo)
//  implicit val TypeInspectInfoFormat = jsonFormat3(TypeInspectInfo)
 
  // Recursive json Example
  //case class Foo(i: Int, foo: Foo)
  //implicit val fooFormat: JsonFormat[Foo] = lazyFormat(jsonFormat(Foo, "i", "foo"))

//  implicit object DeclareAsJsonFormat extends RootJsonFormat[DeclaredAs] {
//    def write(dAs: DeclaredAs) : JsValue = {
//      dAs match {
//      case DeclaredAs.Method => JsString('method)
//      case DeclaredAs.Trait => JsString('trait)
//      case DeclaredAs.Interface => JsString('interface)
//      case DeclaredAs.Object => JsString('object)
//      case DeclaredAs.Class => JsString('class)
//      case DeclaredAs.Field => JsString('field)
//      case DeclaredAs.Nil => JsString('nil)
//      } 
//    }
//    
//    def read(value: JsValue) = ???
//  }
  
//  implicit object TypeInfoJsonFormat extends RootJsonFormat[TypeInfo] {
//    def write(ti: TypeInfo) = {
//     val jsObj = ti match {
//       case basic : BasicTypeInfo => {
//         JsObject("name" ->   JsString(basic.name))
//       }
//       case arrow : ArrowTypeInfo => {
//         JsObject("name" ->   JsString(arrow.name))
//       }
//      }
//      jsObj
//  }
//    def read(value: JsValue) =  ??? 
//  }

//  implicit object TypeInfoFormat extends JsonFormat[TypeInfo] {
//    def read(j: JsObject) : TypeInfo = ???
//
//    def write(ti: TypeInfo): JsObject = {
//      ti match {
//      case basic: BasicTypeInfo => {
//        JsObject(
//            "name" -> JsString(basic.name)
//            )
//      }
//      case arrow: ArrowTypeInfo => {
//        JsObject(
//            "name" -> JsString(arrow.name)
//            )
//      }
//     }
//    } 
//  }
  
 
  
}



// Working test 
/*

 trait JsTest {
    def name: String
    def connected: List[JsTest]
  }
  
  case class BasicJsTest(name: String, connected: List[JsTest], id: Int) extends JsTest
  case class ArrowJsTest(name: String, connected: List[JsTest], text: String) extends JsTest

 object TestJsonFormat extends DefaultJsonProtocol { 
  
   // implicit val JsTestFormat : JsonFormat[JsTest] = lazyFormat(jsonFormat(JsTest, "name","connected"))
      
  implicit object JsTestFormat extends RootJsonFormat[JsTest] {
    def write(ti: JsTest) = {
     val jsObj = ti match {
       case basic : BasicJsTest => {
         
         val connected = if(basic.connected == List.empty){
           JsArray()
         } else {
           val v = basic.connected.map( e => TestJsonFormat.JsTestFormat.write(e))
           JsArray(v)
         }
         
         JsObject("name" ->   JsString(basic.name),
                  "connected" -> connected,
                  "id" -> JsNumber(basic.id))
       }
       case arrow : ArrowJsTest => {
         
          val connected = if(arrow.connected == List.empty){
           JsArray()
         } else {
           val v = arrow.connected.map( e => TestJsonFormat.JsTestFormat.write(e))
           JsArray(v)
         }
         
         JsObject("name" ->   JsString(arrow.name),
                  "connected" -> connected,
                  "text" -> JsString(arrow.text))
       }
      }
      jsObj
  }
    def read(value: JsValue) =  ??? 
  }
    
    
    implicit val BasicJsTestFormat : JsonFormat[BasicJsTest] = lazyFormat(jsonFormat(BasicJsTest, "name","connected","id"))
    implicit val ArrowJsTestFormat : JsonFormat[ArrowJsTest] = lazyFormat(jsonFormat(ArrowJsTest, "name","connected","text"))
  }
  
  
  
  object TEST {
    def main(args: Array[String]): Unit = {
      val basic = BasicJsTest("basic",List(),1)
      val basic2 = BasicJsTest("basic",List(),2)
      val arrow = ArrowJsTest("arrow",List(),"one")
      val arrow2 = ArrowJsTest("arrow",List(),"two")
      
      val basicStacked = BasicJsTest("basicStacked",List(basic,arrow,arrow2,basic2),1)
      
      import org.codeprose.util.TestJsonFormat._
      
      println("Basic: " + basic.toJson.compactPrint )
      println("Arrow: " + arrow.toJson.compactPrint )
      println("Stacked: " + basicStacked.toJson.compactPrint )
      
      
    }
  }
  
  */
