package org.codeprose.util

import java.io.File
import scala.collection.mutable.ArrayBuffer

import org.codeprose.api.Token
import org.codeprose.api._
import org.codeprose.api.scalalang._


class EnsimeApiToCodeproseApi(
    enrichedTokensPerFile: ArrayBuffer[(File,ArrayBuffer[Token])], 
    findTokenId: (String, Int, ArrayBuffer[(File,ArrayBuffer[Token])]) => Int) extends CodeproseApiCreator {
  
    
  def convertToSymbolInfo(symInfo: org.ensime.api.SymbolInfo) : SymbolInfo = {
    val tpe = convertToTypeInfo(symInfo.`type`)
    val declPos = symInfo.declPos match {
          case Some(p) => { convertToSourcePosition(p) }
          case None => { None }
        }
    SymbolInfo(symInfo.name, symInfo.localName, declPos, tpe, symInfo.isCallable, symInfo.ownerTypeId)
  }
  
  
  def convertToTypeInfo(typeInfo: org.ensime.api.TypeInfo) : TypeInfo = {
    
    val tpeInfo = typeInfo match {
      case typeInfo: org.ensime.api.BasicTypeInfo => {
        
        val srcPos = typeInfo.pos match {
          case Some(p) => { convertToSourcePosition(p) }
          case None => { None }
        }
       
        val typeArgs = if(typeInfo.typeArgs == List.empty){
          List[TypeInfo]()
        } else {
          typeInfo.typeArgs.map(t => convertToTypeInfo(t))
        }
          
                                               
        val members =  if(typeInfo.members == List.empty){
          List[EntityInfo]()
        } else {
          typeInfo.members.map(e => convertToEntityInfo(e))
        }
        
        val declaredAt = convertDelaredAs(typeInfo.declAs)
        
        BasicTypeInfo(typeInfo.name,typeInfo.typeId,declaredAt,typeInfo.fullName,typeArgs,members,srcPos,typeInfo.outerTypeId)
      }
      case tree : org.ensime.api.ArrowTypeInfo => {
        
        val resultType = convertToTypeInfo(tree.resultType)
        val paramSections = if(tree.paramSections == List.empty) {
          List[ParamSectionInfo]()
        } else {
          tree.paramSections.map(e=>convertToParamSectionInfo(e))
        }
        
        ArrowTypeInfo(tree.name,tree.typeId,resultType,paramSections) 
      }
    } 
    tpeInfo
  }
   
  def convertToSourcePosition(srcPos: org.ensime.api.SourcePosition) : Option[OffsetSourcePositionWithTokenId] = {
    srcPos match {
      case pos : org.ensime.api.OffsetSourcePosition => {
        val tokenId = findTokenId(pos.file.getAbsolutePath,pos.offset,enrichedTokensPerFile)
        Some(OffsetSourcePositionWithTokenId(pos.file.getAbsolutePath,pos.offset,tokenId))
      } 
      case _ => { None }
    }
  }
 
  def convertToParamSectionInfo(paramSection: org.ensime.api.ParamSectionInfo) : ParamSectionInfo = {
    val params = if(paramSection.params == List.empty){
      List[(String,TypeInfo)]()
    } else {
      paramSection.params.map(e => (e._1,convertToTypeInfo(e._2)))
      }
    ParamSectionInfo(params, paramSection.isImplicit)
    
  }
  
  def convertToTypeInspectInfo(typeInspectInfo: org.ensime.api.TypeInspectInfo) : TypeInspectInfo = {
    val tpe = convertToTypeInfo(typeInspectInfo.`type`)
    val interfaces = if(typeInspectInfo.interfaces == List.empty){
      List[InterfaceInfo]()
    } else {
      typeInspectInfo.interfaces.map(e=>convertToInterfaceInfo(e))
      }
    TypeInspectInfo(tpe, typeInspectInfo.companionId, interfaces)
  }
  
  def convertToInterfaceInfo(interfaceInfo: org.ensime.api.InterfaceInfo) : InterfaceInfo = {
    val tpe = convertToTypeInfo(interfaceInfo.`type`)
    InterfaceInfo(tpe, interfaceInfo.viaView)
  }
  
  def convertDelaredAs(declAs: org.ensime.api.DeclaredAs) : DeclaredAs = {
    declAs match {
      case org.ensime.api.DeclaredAs.Method => DeclaredAs.Method
      case org.ensime.api.DeclaredAs.Trait => DeclaredAs.Trait
      case org.ensime.api.DeclaredAs.Interface => DeclaredAs.Interface
      case org.ensime.api.DeclaredAs.Object => DeclaredAs.Object
      case org.ensime.api.DeclaredAs.Class => DeclaredAs.Class
      case org.ensime.api.DeclaredAs.Field => DeclaredAs.Field
      case org.ensime.api.DeclaredAs.Nil => DeclaredAs.Nil
    }
  }
  
  def convertToPackageInfo(packInfo: org.ensime.api.PackageInfo) : PackageInfo = {
    val members = if(packInfo.members == List.empty){
      List[EntityInfo]()
    } else {packInfo.members.map(e=>convertToEntityInfo(e))
      }
    PackageInfo(packInfo.name, packInfo.fullName, members)
  }
  
  def convertToNamedTypeMemberInfo(namedTypeMemInfo: org.ensime.api.NamedTypeMemberInfo) : NamedTypeMemberInfo = {
    val tpe = convertToTypeInfo(namedTypeMemInfo.`type`)
    val pos = namedTypeMemInfo.pos match {
          case Some(p) => { convertToSourcePosition(p) }
          case None => { None }
        }
    val declAs = convertDelaredAs(namedTypeMemInfo.declAs)
    NamedTypeMemberInfo(namedTypeMemInfo.name,tpe, pos, namedTypeMemInfo.signatureString, declAs)
  }
  
  
  def convertToEntityInfo(entityInfo: org.ensime.api.EntityInfo) : EntityInfo = {
   val eInfo = entityInfo match {
      case namedTypeMemInfo : org.ensime.api.NamedTypeMemberInfo => {
        convertToNamedTypeMemberInfo(namedTypeMemInfo)
      }
      case packInfo : org.ensime.api.PackageInfo => {
       convertToPackageInfo(packInfo) 
      }
      case typeInfo : org.ensime.api.TypeInfo => {
        convertToTypeInfo(typeInfo)
      }
      
    }
   eInfo
  }
  
  def convertToImplicitInfo(implInfo: org.ensime.api.ImplicitInfo) : ImplicitInfo = {
    val implicitInfo = implInfo match {
      case implInfo : org.ensime.api.ImplicitConversionInfo => {
        val fun = convertToSymbolInfo(implInfo.fun)
        ImplicitConversionInfo(implInfo.start, implInfo.end, fun)
      }
      case implInfo : org.ensime.api.ImplicitParamInfo => {
        val fun = convertToSymbolInfo(implInfo.fun)
        val params = if(implInfo.params == List.empty){
          List[SymbolInfo]()
        } else {
          implInfo.params.map(e=>convertToSymbolInfo(e))
        }
        ImplicitParamInfo(implInfo.start, implInfo.end, fun, params, implInfo.funIsImplicit)
      }
    }
    implicitInfo
  }
 
  def convertToImplicitConversionInfoSummary( implConvInfo: org.ensime.api.ImplicitConversionInfo) : ImplicitConversionInfoSummary = {
    val fun = convertToSymbolInfo(implConvInfo.fun)
    ImplicitConversionInfoSummary(fun)
  }
  
  def convertToImplicitParamInfoSummary( implParamInfo: org.ensime.api.ImplicitParamInfo) : ImplicitParamInfoSummary = {
    val fun = convertToSymbolInfo(implParamInfo.fun)
    val params = if(implParamInfo.params == List.empty){
      List.empty
    } else {
      implParamInfo.params.map(e=>convertToSymbolInfo(e))
    }
    ImplicitParamInfoSummary(fun,params,implParamInfo.funIsImplicit)
  }
  
}





trait CodeproseApiCreator {
  
  protected def BasicTypeInfo(name: String,typeId: Int,declAs: DeclaredAs,fullName: String,typeArgs: Iterable[TypeInfo],members: Iterable[EntityInfo],pos: Option[SourcePosition],outerTypeId: Option[Int]) : TypeInfo = {
    new BasicTypeInfo(name,typeId,declAs,fullName,typeArgs,members,pos,outerTypeId)
  }
  
  protected def ArrowTypeInfo(name: String,typeId: Int,resultType: TypeInfo,paramSections: Iterable[ParamSectionInfo]) : TypeInfo = {
    new ArrowTypeInfo(name,typeId,resultType,paramSections)
  }
  
  protected  def ParamSectionInfo(params: Iterable[(String, TypeInfo)],isImplicit: Boolean) : ParamSectionInfo = {
    new ParamSectionInfo(params, isImplicit)
  }
  
  protected def TypeInspectInfo(tpe: TypeInfo,companionId: Option[Int],interfaces: Iterable[InterfaceInfo]) : TypeInspectInfo = {
    new TypeInspectInfo(tpe,companionId,interfaces)
  }
  
  def InterfaceInfo(tpe: TypeInfo,viaView: Option[String]) : InterfaceInfo = {
    new InterfaceInfo(tpe,viaView)
  }
  
  def SymbolInfo(name: String,localName: String,declPos: Option[SourcePosition],tpe: TypeInfo,isCallable: Boolean,ownerTypeId: Option[Int]) : SymbolInfo = {
    new SymbolInfo(name,localName,declPos,tpe,isCallable,ownerTypeId)
  }
  
  
  def PackageInfo(name: String, fullName: String, members: Seq[EntityInfo]) : PackageInfo = {
    new PackageInfo(name,fullName, members)
  }
  
  def NamedTypeMemberInfo(name: String,tpe: TypeInfo,pos: Option[SourcePosition],signatureString: Option[String],declAs: DeclaredAs) : NamedTypeMemberInfo = {
      new NamedTypeMemberInfo(name, tpe, pos, signatureString, declAs)
  }
  
  def ImplicitConversionInfo(start: Int, end: Int, fun: SymbolInfo) : ImplicitConversionInfo = {
    new ImplicitConversionInfo(start,end,fun)
  }
  
  def ImplicitConversionInfoSummary(fun: SymbolInfo) : ImplicitConversionInfoSummary = {
    new ImplicitConversionInfoSummary(fun)
  }
  
  def ImplicitParamInfo(start: Int, end: Int, fun: SymbolInfo,params: List[SymbolInfo],funIsImplicit: Boolean) : ImplicitParamInfo = {
    new ImplicitParamInfo(start,end,fun,params,funIsImplicit)
  }
  
  def ImplicitParamInfoSummary(fun: SymbolInfo,params: List[SymbolInfo],funIsImplicit: Boolean) : ImplicitParamInfoSummary = {
    new ImplicitParamInfoSummary(fun,params,funIsImplicit)
  }
}




  


 


 

  
  


  
