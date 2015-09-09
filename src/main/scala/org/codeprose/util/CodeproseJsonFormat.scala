package org.codeprose.util

import spray.json._
import org.codeprose.api._
import org.codeprose.api.scalalang._
import org.codeprose.consumer.util.XmlEscape
import com.typesafe.scalalogging.LazyLogging

/**
 * Includes the spray.json Format for TokenProperties and SummaryProperties.
 * 
 * Note: 
 *  - Formats implemented via RootJsonFormat are missing the implementation of read().
 *  - Members that might be display in hmtl are escaped with import org.codeprose.consumer.util.XmlEscape. 
 */
object CodeproseJsonFormat extends DefaultJsonProtocol with LazyLogging {

	/**
	 * Json format SourcePositionLinkWithCodeSample.
	 */  
	implicit val SourcePositionLinkWithCodeSampleFormat = jsonFormat4(SourcePositionLinkWithCodeSample)

			/**
			 * Json format OffsetSourcePositionWithTokenId.
			 */
			implicit val OffsetSourcePositionWithTokenIdFormat = jsonFormat3(OffsetSourcePositionWithTokenId)

			/**
			 * Json format DeclaredAs.
			 */
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


	/**
	 * Json format EntityInfo.
	 */
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
				val name = XmlEscape.escape(entity.name).toJson
				JsObject("name" -> name,
						"members" -> members)
			}   
			}
		}

		def read(value: JsValue) =  ??? 
	}

	/**
	 * Json format PackageInfo.
	 */
	implicit object PackageInfoFormat extends RootJsonFormat[PackageInfo] {

		def write(pi: PackageInfo) = {
			val members = if(pi.members == List.empty){
				JsArray()
			} else {
				val v = pi.members.map(e=>CodeproseJsonFormat.EntityInfoFormat.write(e)).toList
						JsArray(v)
			}

			val name = XmlEscape.escape(pi.name).toJson
					val fullName = XmlEscape.escape(pi.fullName).toJson

					JsObject("typeName" -> pi.typeName.toJson, 
							"name" -> name,
							"fullName" -> fullName,
							"members" -> members)
		}

		def read(value: JsValue) =  ??? 
	}

	/**
	 * Json format NamedTypeMemberInfo.
	 */
	implicit object NamedTypeMemberInfoFormat extends RootJsonFormat[NamedTypeMemberInfo] {

		def write(namedTypeMemInfo:  NamedTypeMemberInfo) = {
			val name = XmlEscape.escape(namedTypeMemInfo.name).toJson
					val typeInfo = CodeproseJsonFormat.TypeInfoFormat.write(namedTypeMemInfo.tpe)
					val pos = namedTypeMemInfo.pos.toJson
					val signatureString = XmlEscape.escape(namedTypeMemInfo.signatureString.getOrElse("")).toJson
					val declAs = CodeproseJsonFormat.DeclaredAsFormat.write(namedTypeMemInfo.declAs)
					JsObject("typeName" -> namedTypeMemInfo.typeName.toJson, 
							"name" -> name,
							"tpe" -> typeInfo,
							"pos" -> pos,
							"signatureString" -> signatureString,       
							"declAs" -> declAs
							)
		}

		def read(value: JsValue) =  ??? 
	}

	/**
	 * Json format SourcePosition. 
	 */
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

	/**
	 * Json format TypeInfo.
	 */
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

	/**
	 * Json format BasicTypeInfo.
	 */
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

			val name = XmlEscape.escape(basic.name).toJson
					val declAs = CodeproseJsonFormat.DeclaredAsFormat.write(basic.declAs)
					val fullName = XmlEscape.escape(basic.fullName).toJson         
					val pos = basic.pos.toJson 
					JsObject("typeName" -> basic.typeName.toJson, 
							"name" -> name,
							"typeId" -> basic.typeId.toJson,
							"declAs" -> declAs,
							"fullName" -> fullName,
							"typeArgs" -> typeArgs,
							"members" -> members,
							"pos" -> pos,
							"outerTypeId" -> basic.outerTypeId.toJson
							)
		}

		def read(value: JsValue) =  ??? 
	}

	/**
	 * Json format ArrowTypeInfo.
	 */
	implicit object ArrowTypeInfoFormat extends RootJsonFormat[ArrowTypeInfo] {

		def write(arrow: ArrowTypeInfo) = {
			val paramSections = if(arrow.paramSections == List.empty){
				JsArray()
			} else {
				val v = arrow.paramSections.map( e => CodeproseJsonFormat.ParamSectionInfoFormat.write(e)).toList
						JsArray(v)
			}
			val resultType = CodeproseJsonFormat.TypeInfoFormat.write(arrow.resultType)
					val name = XmlEscape.escape(arrow.name).toJson

					JsObject("typeName" -> arrow.typeName.toJson, 
							"name" ->  name,
							"typeId" -> arrow.typeId.toJson,
							"resultType" -> resultType,
							"paramSections" -> paramSections
							)
		}

		def read(value: JsValue) =  ??? 
	}

	/**
	 * Json format ParamSectionInfo.
	 */
	implicit val ParamSectionInfoFormat = jsonFormat2(ParamSectionInfo)

			/**
			 * Json format InterfaceInfo.
			 */
			implicit val InterfaceInfoFormat = jsonFormat2(InterfaceInfo)

			/**
			 * Json format TypeInspectInfo.
			 */
			implicit val TypeInspectInfoFormat = jsonFormat3(TypeInspectInfo)

			/**
			 * Json format SymbolInfo.
			 */
			implicit object SymbolInfoFormat extends RootJsonFormat[SymbolInfo] { 

		def write(symbolInfo: SymbolInfo) = {
			val name = XmlEscape.escape(symbolInfo.name).toJson
					val localName = XmlEscape.escape(symbolInfo.localName).toJson
					val declPos = symbolInfo.declPos.toJson
					val tpe = symbolInfo.tpe.toJson

					JsObject(
							"name" ->  name,
							"lobalName" -> localName,
							"declPos" -> declPos,
							"tpe" -> tpe,
							"isCallable" -> symbolInfo.isCallable.toJson,
							"ownerTypeId" -> symbolInfo.ownerTypeId.toJson
							)
		}

		def read(value: JsValue) =  ??? 
	}

	/**
	 * Json format ImplicitInfo.
	 */
	implicit object ImplicitInfoFormat extends RootJsonFormat[ImplicitInfo] {

		def write(implInfo: ImplicitInfo) = {
			implInfo match {
			case implConv : ImplicitConversionInfo => {
				JsObject("typeName"->implConv.typeName.toJson)  
			}
			case implParam : ImplicitParamInfo => {
				JsObject("typeName"->implParam.typeName.toJson)  
			}
			case implConv : ImplicitConversionInfoSummary => {
				implConv.toJson
			}
			case implParam : ImplicitParamInfoSummary => {
				implParam.toJson
			}
			case _ => JsObject()
			}
		}

		def read(value: JsValue) =  ??? 
	}

	/**
	 * Json format ImplicitConversionInfoSummary.
	 */
	implicit object ImplicitConversionInfoSummaryFormat extends RootJsonFormat[ImplicitConversionInfoSummary] {

		def write(implInfo: ImplicitConversionInfoSummary) = { 
			JsObject(
					"typeName"->implInfo.typeName.toJson,
					"fun" -> implInfo.fun.toJson     
					)
		}

		def read(value: JsValue) =  ???
	} 

	/**
	 * Json format ImplicitParamInfoSummary.
	 */
	implicit object ImplicitParamInfoSummaryFormat extends RootJsonFormat[ImplicitParamInfoSummary] {

		def write(implInfo: ImplicitParamInfoSummary) = { 
			val params = if(implInfo.params==List.empty){
				JsArray()
			} else {
				val v = implInfo.params.map( e => CodeproseJsonFormat.SymbolInfoFormat.write(e)).toList
						JsArray(v)
			}

			JsObject(
					"typeName"->implInfo.typeName.toJson,
					"fun" -> implInfo.fun.toJson,
					"params" -> params,
					"funIsImplicit" -> implInfo.funIsImplicit.toJson 
					)
		}

		def read(value: JsValue) =  ???
	} 

}

