// Codeprose
var tooltip_Debug = true;
	/*
       * Create the tooltip entry html content for a token.
       */
      function createTooltipHtmlFromDataAttr(elem) { 
    
        // Fullname
        var fullname = "<b>" + $(elem).data("cp-fullname") + "</b>";

        // TypeId
        var typeId = $(elem).data("cp-typeid")
  
        // Declaration
       linkToDeclaration = tooltipSrcFile_getLinkToDeclaration(elem);

  	// Where used in project
    	var linkToWhereUsedInProject = tooltipSrcFile_getLinkToWhereTypeUsedInProject(typeId)

        // Type summary
        var typeInfo = typeInformation[typeId];
        var typeSummary = tooltipSrcFile_getTypeInformation(typeInfo);

        // Type overview    
        var typeOverview = tooltipSrcFile_getLinkToTypeOverview(typeId);
  
        // Type owner
        var typeOwner = tooltipSrcFile_getOwnerTypeInformation(elem);
  
        // Implicit summary
        var implicitSummary = tooltipSrcFile_getImplicitInformation(elem);
      
        // Output structure 
        html = "<div class='cp-tooltip'>" + fullname + "" +
        linkToDeclaration +
        typeSummary +
        typeOverview + 
        typeOwner + 
        implicitSummary + 
        linkToWhereUsedInProject +
        "</div>";
      
        return html;
      }

// Tooltip helpers



function tooltipSrcFile_getLinkToDeclaration(elem){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getLinkToDeclaration"); }

	var rawLinkToDeclaration = $(elem).data("cp-declaration");
        var linkToDeclaration = ""
        if(rawLinkToDeclaration){
          console.log( rawLinkToDeclaration);
          linkToDeclaration = "<div style='margin-top:0.5em;'><a href='" + rawLinkToDeclaration + "'>Declaration</a>" + "<br/></div>";
        }
	return linkToDeclaration;
}

function tooltipSrcFile_getLinkToTypeOverview(typeId){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getLinkToTypeOverview"); }
	var retString = "";
	var typeInspectInfo = typeInformation[typeId];

	if(typeInspectInfo != null){
		var typeInfo = typeInspectInfo.tpe;
		if(typeInfo._infoType === "BasicTypeInfo"){
			var rawLink = "../" + "typeInformationSummary.html#TYPEID" + typeId;
			retString = "<div style='margin-top:0.5em;'>" + "<a href='" + rawLink + "'>" + "Type overview" + "</a>" + "</div>";
		}
	}
	return retString;
}

function tooltipSrcFile_getLinkToWhereTypeUsedInProject(typeId){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getLinkToWhereTypeUsedInProject"); }
  	var linkToWhereUsedInProject = "";
        if(typeId != null){
          rawLinkToWhereUsedProject = "../whereUsedSummary.html"+ "#TYPEID" + typeId
          linkToWhereUsedInProject = "<div style='margin-top:0.5em;'>" + "<a href='" + rawLinkToWhereUsedProject + "'>Type where used in project</a>" + "</div>";
        }    

	return linkToWhereUsedInProject;
}



/*
 * Return a type Inspect summary unordered list.
 *
 */
function tooltipSrcFile_getTypeInformation(typeInspectInfo){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getTypeInformation"); }

	var retString = "";
	if(typeInspectInfo ==  null){
		return retString;
	}
	retString += "<div style='margin-top:0.5em;'>";
	if(typeInspectInfo.tpe._infoType === "BasicTypeInfo"){
		retString += tooltipSrcFile_getTypeInformation_BasicTypeInfo(typeInspectInfo);
        } else {
		retString += tooltipSrcFile_getTypeInformation_ArrowTypeInfo(typeInspectInfo);
	}
	retString += "</div>";	
    
	return retString;
}

function tooltipSrcFile_getTypeInformation_ArrowTypeInfo(typeInspectInfo){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getTypeInformation_ArrowTypeInfo"); }

	var retString ="";
	
	retString += "Arrow information:";
	retString += "<ul style='padding-left:2em;'>"
	// Parameters
	if(typeInspectInfo.tpe.paramSections != null){
		var parameterList = tooltipSrcFile_getListOfParameters(typeInspectInfo.tpe.paramSections);
		if(parameterList != ""){
	 		retString += "<li>Parameters:" + parameterList + "</li>";
		}
	}
	
	// Return type
	if(typeInspectInfo.tpe.resultType != null){

		var resultType = tooltipSrcFile_getResultType(typeInspectInfo.tpe.resultType);

		if(resultType != ""){
	 		retString += "<li style='margin-top:0.5em;'>Result type:" + resultType + "</li>";
		} else {
		}


	}

	retString += "</ul>"
	return retString;
}

function tooltipSrcFile_getResultType(typeInfo){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getResultType"); }
	var retString = "";
	var retTypeName = getTypeInfoName(typeInfo);
	var retTypeId = typeInfo.typeId;
	var retTypeInspectInfo = typeInformation[retTypeId];
	console.log(retTypeInspectInfo);
	if(retTypeInspectInfo != null && retTypeInspectInfo.tpe._infoType === "BasicTypeInfo"){
		
		var rawLink = getRawLinkToTypeDefinition(retTypeInspectInfo.tpe); 
		var declAs = typeInfo.declAs;
		if(rawLink!=""){
			retString += "<ul style='margin-top:0.5em;padding-left:1em;'>" + "<li style='margin-top:0.2em;'>" + declAs + " - " + "<a href='.." + rawLink + "'>" + retTypeName + "</a>" + "</li>" + "</ul>";
		} else {
			retString += "<ul style='margin-top:0.5em;padding-left:1em;'>" + "<li style='margin-top:0.2em;'>" + declAs + " - " + retTypeName + "</li>" + "</ul>";
}
	} else {
		retString += "<ul style='margin-top:0.5em;padding-left:1em;'>" + "<li style='margin-top:0.2em;'>" + retTypeName + "</li>" + "</ul>";

	}

return retString;

}

function tooltipSrcFile_getListOfParameters(paramSections){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getListOfParameters"); }
	var retString = "<ul style='margin-top:0.5em;padding-left:1em;'>";
	if(paramSections.length==0){
		retString += "<li style='margin-top:0.2em;'>" + "-" + "</li>";
	} else {
		for(var i=0;i<paramSections.length;i++){
			var params = paramSections[i];
		
	 		var implicitInd = "";
			if(params.isImplicit == true){ 
			  implicitInd = " (impl)";
			}

			if(params.params.length==0){
				retString += "<li style='margin-top:0.2em;'>" + "-" + "</li>";
			} else {

				for(var k=0;k<params.params.length;k++){
					var paramName = params.params[k][0];
					var paramTypeInfo = params.params[k][1];
			 		
			  		var paramTypeName = getTypeInfoName(paramTypeInfo);
	
					var rawLinkToDef = "";
					if(paramTypeInfo._infoType === "BasicTypeInfo"){
						var typeInspectInfo = typeInformation[paramTypeInfo.typeId];
						if(typeInspectInfo != null){
							rawLinkToDef = getRawLinkToTypeDefinition(typeInspectInfo.tpe);
						}
					}
					if(rawLinkToDef!= ""){
						retString += "<li style='margin-top:0.2em;'>" + paramName + ": " + "<a href='.." + rawLinkToDef + "'>" + paramTypeName + "</a>" + implicitInd +"</li>";
					} else {
						retString += "<li style='margin-top:0.2em;'>" + paramName + ": " + paramTypeName + implicitInd +"</li>";
					}
	
				
				}
			}
		}
	}
	retString += "</ul>";
	return retString;
}


function tooltipSrcFile_getTypeInformation_BasicTypeInfo(typeInspectInfo){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getTypeInformation_BasicTypeInfo"); }		

		var retString = "";
		retString += "Type information:" + "<ul style='margin-left:0em;padding-left:2em;'>";

		var declaredAs = typeInspectInfo.tpe.declAs;
		if(declaredAs != null){
		//	retString += "<li> Declared as: " + declaredAs + "</li>";
		}

		var rawLinkToTypeDef = getRawLinkToTypeDefinition(typeInspectInfo.tpe);	

		if(rawLinkToTypeDef != ""){
			retString += "<li>" + declaredAs + " - " + "<a href='.." + rawLinkToTypeDef + "'>Definition</a>"  + "<li/>";
		} else {
			retString += "<li>" + declaredAs + " - Def. outside project" + "</li>"; 
		}
	
		// Implemented interfaces:
		var interfaceList = tooltipSrcFile_getListOfInterfaces(typeInspectInfo.interfaces);
		if(interfaceList != ""){
			retString += "<li style='margin-top:0.5em;'>" + "Implements:" + interfaceList + "</li>";	
		}
			
		

		// Outer type
		if(typeInspectInfo.tpe.outerTypeId!=null){
			var outerTypeId = typeInspectInfo.tpe.outerTypeId;
			var outerTypeInspectInfo = typeInformation[outerTypeId];
			if(outerTypeInspectInfo.tpe != null && outerTypeInspectInfo.tpe._infoType === "BasicTypeInfo"){
			
				var outerTypeList = tooltipSrcFile_getOuterTypeInformation(outerTypeInspectInfo.tpe);
				retString += "<li style='margin-top:0.5em;'>" + "Outer type: " + outerTypeList + "</li>";
			}
		}

		// Companion 
		if(typeInspectInfo.companionId != null){

			var companionInfo = tooltipSrcFile_getCompanionObjectInformation(typeInspectInfo);
			if(companionInfo != ""){
				retString += "<li style='margin-top:0.5em;'>" + "Companion: " + companionInfo + "</li>";
			}
		} 

		retString += "</ul>";
		
	return retString;
}


function tooltipSrcFile_getCompanionObjectInformation(typeInspectInfo){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getCompanionObjectInformation"); } 	

	var retString = "";
	if(typeInspectInfo.companionId != null){
			
		var companionInspectInfo = typeInformation[typeInspectInfo.companionId];
		if(companionInspectInfo != null && companionInspectInfo.tpe._infoType === "BasicTypeInfo") {
			var typeInfo = companionInspectInfo.tpe;
			var declAs = typeInfo.declAs;
			var n = getTypeInfoName(typeInfo);
			var rawLink = getRawLinkToTypeDefinition(typeInfo);
			
			if(rawLink!=""){
				retString += "<ul style='margin-left:0em;padding-left:1em;'><li style='margin-top:0.5em;'>" + declAs + " - " + "<a href='.." + rawLink + "'>"+ n + "</a></li></ul>";
			} else {
				retString += "<ul style='margin-left:0em;padding-left:1em;'><li style='margin-top:0.5em;'>" + declAs + " - " + n + "</li></ul>";
			}

		}
	} 

	return retString;
}



function tooltipSrcFile_getOuterTypeInformation(basicTypeInfo){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getOuterTypeInformation"); } 
	var retString = "";
	var outerTypeName = getTypeInfoName(basicTypeInfo);
	var outerTypeRawLink = getRawLinkToTypeDefinition(basicTypeInfo);
	var outerTypeDeclAs = basicTypeInfo.declAs;
	
	if(outerTypeRawLink!=""){
		retString += "<ul style='margin-left:0em;padding-left:1em;'><li style='margin-top:0.5em;'>" + outerTypeDeclAs + " - " + "<a href='.." + outerTypeRawLink + "'>"+ outerTypeName + "</a></li></ul>";
	} else {
		retString += "<ul style='margin-left:0em;padding-left:1em;'><li style='margin-top:0.5em;'>" + outerTypeDeclAs + " - " + outerTypeName + "</li></ul>";
	}
	return retString;
}


/*
* Input Array of InterfaceInfo
*/
function tooltipSrcFile_getListOfInterfaces(interfaceInfos){
	if(tooltip_Debug){ console.log("tooltipSrcFile_getListOfInterfaces"); }

	var retString = "";
	if(interfaceInfos!=null && interfaceInfos.length>0){
		retString += "<ul style='margin-left:0em;padding-left:1em;'>";
		for(var i=0; i<interfaceInfos.length;i++){
			var typeInfo=interfaceInfos[i].tpe;
			
			var intFaceName = getTypeInfoName(typeInfo)	
			var intFaceDeclAs = typeInfo.declAs;
			var intFaceDefLink = getRawLinkToTypeDefinition(typeInfo);			
			if(intFaceDefLink != ""){
					retString += "<li style='margin-top:0.2em;'>" + intFaceDeclAs + " - " + "<a href='.." + intFaceDefLink + "'>" +intFaceName + "</a>" +"</li>";
			} else {
				retString += "<li  style='margin-top:0.2em;'>" + intFaceDeclAs + " - " + intFaceName + "</li>";
			}


		}
		retString += "</ul>";
	}

	return retString;

}

/*
 * Returns information on implicit conversions and parameters. 
 */
function tooltipSrcFile_getImplicitInformation(elem){

	if(tooltip_Debug){ console.log("tooltipSrcFile_getImplicitInformation"); }

	var hasImplicitConversions = $(elem).data("cp-implicitconversion");
 	var hasImplicitParameters = $(elem).data("cp-implicitparameter");
	var retString = "";

	if(hasImplicitConversions){
		var implicitConversionids = $(elem).data("cp-implicitconversionids");

		if(implicitConversionids != null){
			
			var implicitConversionList = tooltipSrcFile_implicitConversion(implicitConversionids);

			if(implicitConversionids!=null){
				retString += "<div style='margin-top:0.5em;'>" + "Impl. Conversion(s):" + implicitConversionList + "</div>";
			}
		}
	}


	if(hasImplicitParameters){
		var implicitParameterids = $(elem).data("cp-implicitparameterids");

		if(implicitParameterids != null){
			var implicitParamList = tooltipSrcFile_implicitParameter(implicitParameterids);
			if(implicitParamList!=null){
				retString += "<div style='margin-top:0.5em;'>" + "Impl. Parameter(s):" + implicitParamList + "</div>";
			}
		}
	}

	return retString;
}


function tooltipSrcFile_implicitConversion(implicitConversionIds){

	if(tooltip_Debug){ console.log("tooltipSrcFile_implicitConversion"); }

	var retString = "<ul style='margin-top:0.3em;margin-left:0em;padding-left:2em;'>";
	console.log(implicitConversionIds);
	for(var i=0;i<implicitConversionIds.length;i++){
		
		var implConvInfo = implicitConversions[implicitConversionIds[i]];
		console.log(implConvInfo);		
		var n  = implConvInfo.fun.name
		var rawLink = "";
		if(implConvInfo.fun.declPos != null){
			rawLink = getRawLinkToDeclaration(implConvInfo.fun.declPos);
		}
		if(rawLink!=""){
			retString += "<li style='margin-top:0.3em;'>" + "<a href='.." + rawLink + "'>" + n + "</a>" + "</li>";	
		} else {
			retString += "<li style='margin-top:0.3em;'>" + n + "</li>";	
		}
		
	}


	retString += "</ul>";
	return retString;
}

function tooltipSrcFile_implicitParameter(implicitParameterIds){
	if(tooltip_Debug){ console.log("tooltipSrcFile_implicitParameter"); }
	console.log(implicitParameterIds);
	
	var retString = "<ul style='margin-top:0.3em;margin-left:0em;padding-left:2em;'>";

	for(var i=0;i<implicitParameterIds.length;i++){
		
		var implParaInfo = implicitParameters[implicitParameterIds[i]];

		console.log(implParaInfo);
		var params = implParaInfo.params;
		for(var k=0;k<params.length;k++) {
			var n = params[k].name;
			var rawLinkToParamDecl = "";
			if(params[k].declPos!=null){
				rawLinkToParamDecl = getRawLinkToDeclaration(params[k].declPos);					
			}
			
			var typeInfo = params[k].tpe;
			var typeName = "";
			var typeDeclAs = "";
			var rawLinkToTypeDef = "";

			if(typeInfo!=null){
				typeName = getTypeInfoName(typeInfo);
				if(typeInfo._infoType === "BasicTypeInfo"){
					typeDeclAs = typeInfo.declAs;
					if(typeInfo.pos!=null){
						rawLinkToTypeDef = getRawLinkToDeclaration(typeInfo.pos);
					}
				} else {
					
				}
			}
			

			var entry = ""; 
			
			if(rawLinkToParamDecl!=""){
				entry += "<a href='.." + rawLinkToParamDecl + "'>" + n + "</a>";
			} else {
				entry += n;
			}

			if(rawLinkToTypeDef!=""){
				entry += " - <a href='.." + rawLinkToTypeDef + "'>" + typeName + "</a>";
			} else {
				entry += " - " + typeName;
			}

			if(typeDeclAs!=""){
				entry += " - " + typeDeclAs;
			}

			retString += "<li>" + entry + "</li>";
		}

	}
	retString += "</ul>";
	return retString;
} 


function tooltipSrcFile_getOwnerTypeInformation(elem){

	if(tooltip_Debug){ console.log("tooltipSrcFile_getOwnerTypeInformation"); }

	var ownerTypeId = $(elem).data("cp-ownertypeid");
	var retString = "";
	if(ownerTypeId!=null){
		var typeInspectInfo = typeInformation[ownerTypeId];
		if(typeInspectInfo != null){
 
			retString += "<div style='margin-top:0.5em;'>" + "Owner type:";
			var typeInfo = typeInspectInfo.tpe;
			var fullname = getTypeInfoName(typeInfo);
			var rawLinkToDef = "";

			if(typeInfo._infoType === "BasicTypeInfo"){
				rawLinkToDef = getRawLinkToTypeDefinition(typeInfo);			
			}
				
			if(rawLinkToDef != ""){
				retString += "<div style='margin-top:0.5em;padding-left:2em;'><a href='.." + rawLinkToDef + "'>" + fullname + "</a>" + "</div>";
			} else {
				retString += "<div style='margin-top:0.5em;padding-left:2em;'>" + fullname + "</div>";
			}
			
			retString += "</div>";			 		
		

		}
	} 

	return retString;
}

/*
* Returns 
*	if type defintion in project: "/content/srcFile.scala.html#T<ID>"
*	else "" 
* 
*
*/
function getRawLinkToTypeDefinition(basicTypeInfo){

	var rawLink = "";
	if(basicTypeInfo!=null && basicTypeInfo.pos != null){
		var srcFileLink = srcFileToRelLink[basicTypeInfo.pos.filename];
	        var tokenId = basicTypeInfo.pos.tokenId;
		
		if(srcFileLink != null && tokenId != -1){
			rawLink = srcFileLink + "#T" + tokenId;		
		}
	}

	return rawLink;
}

/*
* Returns link based on OffsetSourcePosition with Token id.
* Assumes declPos != null
*/
function getRawLinkToDeclaration(declPos){
	var retString = "";
	var filename = declPos.filename;
	var tokenId = declPos.tokenId;
	if(filename != null && tokenId != -1){
		var rawLink = srcFileToRelLink[filename];
		if(rawLink!=null){
			retString = rawLink + "#T" + tokenId;
		}
	}
	return retString;
}

function getTypeInfoName(typeInfo){
	var n = "";
	if(typeInfo._infoType === "BasicTypeInfo"){
		n=typeInfo.fullName;	
	} else {
		n=typeInfo.name;
	}
	return n;
}
