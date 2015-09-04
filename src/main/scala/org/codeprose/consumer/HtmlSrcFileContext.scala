package org.codeprose.consumer

import java.io.File



class HtmlSrcFileContext(
    filename: String, 
    packagName: String, 
    val tokenToOutputEntry: TokenToOutputEntry) {

    // Helpers  
	  private val fileNameWithoutPath =  filename.slice(filename.lastIndexOf("/")+1, filename.length())  
		private	val fileNameWithoutPathAndEnding = fileNameWithoutPath.slice(0,fileNameWithoutPath.lastIndexOf("."))		
		private	val fileNameEnding = fileNameWithoutPath.slice(fileNameWithoutPath.lastIndexOf(".")+1,fileNameWithoutPath.length)

    def htmlDataAttributePrefix  = "cp-"   
      /**
       * Returns the upper part of an output html file. This includes:
       *  - html headers
       *  - js scripts (per token and per File)
       *  - header div
       * @return    String containing the above information.  
       */
			def getBegin() : String = {
      
        val perTokenScripts = tokenToOutputEntry.scriptElements.mkString("\n", "\n\n", "\n")
            
        val perFileScripts = List(
        s"""
           /*
         * Highlights other uses of the token under the mouse within the file.
         */
        $$("[id^='T']").hover( 
          function(){ toHighlight = $$(this).data("cp-whereusedinfile"); $$(toHighlight).toggleClass("highlightWhereUsedWithinFile"); },
          function(){ toHighlight = $$(this).data("cp-whereusedinfile"); $$(toHighlight).toggleClass("highlightWhereUsedWithinFile"); }
        );


        /*
         * Highlights/unhighlights implicit conversions and implicit 
         * parameters within the file.
         */
      function highlightImplicitConversionsAndParameters(){

        // Implicit conversions
        $$('*[data-cp-implicitconversion=true]').toggleClass("highlightImplicitConversion");

        // Implicit parameters
        // Color to use highlightImplicitParameter
        $$('*[data-cp-implicitparameter=true]').toggleClass("highlightImplicitParameter");
      }


      /*
       * Central function handling key events.
       * Mappings:
       *  -  'i' -> highlightImplicitConversionsAndParameters()
       */
      // Key events     
      $$(document).keypress(function(e){
        // i
        if(e.keyCode == 105){ highlightImplicitConversionsAndParameters() }     
      });


      /*
       * Create the tooltip entry html content for a token.
       */
      function createTooltipHtmlFromDataAttr(elem) { 
    
        // Fullname
        var fullname = "<b>" + $$(elem).data("cp-fullname") + "</b>";

        // TypeId
        var typeId = $$(elem).data("cp-typeid")
  
        // Declaration
        var rawLinkToDeclaration = $$(elem).data("cp-declaration");
        var linkToDeclaration = ""
        if(rawLinkToDeclaration){
          console.log( rawLinkToDeclaration);
          linkToDeclaration = "<a href='" + rawLinkToDeclaration + "'>Declaration</a>" + "<br/>";
        }

  // Definition
        var typeInfo = typeInformation[typeId];
        var linkToDefintion = "";
        if(typeInfo !=  null && typeInfo.tpe._infoType === "BasicTypeInfo"){
    if(typeInfo.tpe.pos != null){
      var srcFileLink = srcFileToRelLink[typeInfo.tpe.pos.filename];
      var tokenId = typeInfo.tpe.pos.tokenId;
      if(srcFileLink != null && tokenId != -1){
                linkToDefintion = "<a href='.." + srcFileLink + "#" + tokenId + "'>Type definition</a>" + "<br/>";
      } 
    } else {
      linkToDefintion = "Type definition outside of project.</br>"
    }    
        }       

  // Function information
  linkToFunctionArgs = ""
  linkToFunctionRetType = ""
  if(typeInfo !=  null && typeInfo.tpe._infoType === "ArrowTypeInfo"){

    // Parameters
    if(typeInfo.tpe.paramSections != null){
      var argsToAppend = ""     
      var paramSections = typeInfo.tpe.paramSections;
      for( var i=0;i<paramSections.length;i++){
        var params = paramSections[i];
       
        var implicitInd = "";
        if(params.isImplicit == true){ 
          implicitInd = " (impl)";
        }

        for(var k=0;k<params.params.length;k++){
          var n = params.params[k][0];
          var nType = params.params[k][1];
         
          var nTypeName = "";
          if(nType._infoType === "BasicTypeInfo"){
            nTypeName = nType.fullName;
          } else {
            nTypeName = nType.name;
          }
          argsToAppend += "<li>" + n + ": " + nTypeName + implicitInd +"</li>";
        }
      
      } 
    
    
    
      linkToFunctionArgs = "Args:" + "<ul>" + argsToAppend + "</ul>";
  
    
    }
    // Return type
    if(typeInfo.tpe.resultType != null ){
      var retTypeInfo = typeInfo.tpe.resultType;
    
      // basic
      if(retTypeInfo._infoType === "BasicTypeInfo"){

        linkToFunctionRetType = "Return type:<ul><li>" + retTypeInfo.fullName + "</li></ul><br/>";

      } else {
      // Arrow
      
        linkToFunctionRetType = "Return type:</br>" + "Function" + "<br/>";
      }
    }
  }

        // Where used in project
  var linkToWhereUsedInProject = "";
        if(typeId != null){
          rawLinkToWhereUsedProject = "../whereUsedSummary.html"+ "#TYPEID" + typeId
          linkToWhereUsedInProject = "<a href='" + rawLinkToWhereUsedProject + "'>Where used in project</a>" + "<br/>";
        }      
    
        // Implicit conversion 
        var linkToImplicitConversion =  "";
        var isImplicitConversion = $$(elem).data("cp-implicitconversion");
        if(isImplicitConversion){
    
          
        }

        // Implicit Parameter
        var linkToImplicitParameter = ""
  var isImplicitParameter = $$(elem).data("cp-implicitparameter");
        if(isImplicitParameter){
        
        }
      
        // Output structure 
        html = "<div class='cp-tooltip'>" + fullname + "<br/><br/>" +
        linkToDeclaration +
        linkToDefintion +
  linkToFunctionArgs + 
  linkToFunctionRetType + 
        linkToWhereUsedInProject +
        linkToImplicitConversion +
        linkToImplicitParameter + 
        "</div>";
      
        return html;
      }
      


      /*
       * Assigns the tooltip information to tokens.
       */
       // Tooltip
       $$('*[data-cp-tooltipdisplay=true]').tooltip({
          content: function () {           
          return createTooltipHtmlFromDataAttr(this);
        },
          show: null,
          disabled: false,
          // position: {of: $$("#LCOM0"), at: "left"},
          position: { my: 'left-center', at: "right+7 right"},
          close: function (event, ui) {
            ui.tooltip.hover(
            function () { $$(this).stop(true).fadeTo(400, 1); },
            function () { $$(this).fadeOut("100", function () { $$(this).remove(); }) });
        }
      });
      


      """
      ).mkString("\n", "\n\n", "\n")
      
      
      // Return string
      s"""
<!doctype HTML>
<html lang="en">
<head>
<meta http-equiv="content-type" content="text/html; charset=utf-8" />
<link rel="stylesheet" type="text/css" href="../style/style.css" media="screen" />
<link rel="stylesheet" type="text/css" href="http://ajax.aspnetcdn.com/ajax/jquery.ui/1.10.0/themes/black-tie/jquery-ui.css" media="screen" />
<title>$fileNameWithoutPath</title>
<script src="../js/codeprose.global.js"></script>
<script src="https://code.jquery.com/jquery-1.10.2.js"></script>
<script src="http://ajax.aspnetcdn.com/ajax/jquery.ui/1.10.0/jquery-ui.js"></script>
<script src="../js/codeprose.typeinformation.js"></script>
<script src="../js/codeprose.whereusedinformation.js"></script>   
<script src="../js/codeprose.packageinformation.js"></script>
<script src="../js/codeprose.helper.js"></script>
<script src="../js/codeprose.implicitinformation.js"></script>          
            
                
<script type="text/javascript">

$$(document).ready(function(){ """ +
perFileScripts +
perTokenScripts + 
s"""\n});
</script>
</head>
<body>
<div align="center">
<div class="header" id="header" align="center"> 
<div style="float:left;"> 
<span style="font-size:1.5em;font-weight:bold;" title="$filename">$fileNameWithoutPath&nbsp;</span><span style="font-size:1em;">$packagName</span></div>
<div style="float:right;"><a style="color:black;font-weight:bold;" href="../index.html">overview</a></div>
</div>
<div class="content">
<noscript><div style="margin:2em;"><b>Activate JavaScript for more features</b>.</div></noscript>\n"""
      }
			
      /**
       * Return the lower part of the html output. This includes:
       *  - html footer 
       * @return  String containing the lower part of the html output.
       */
      def getEnd() : String  = {
         s"""\n\n          
<div class="footer">generated by codeprose. help and support on <a href="http://github.com/gushai/codeprose" target="blank">github</a>.</div>\n</div>\n</div>\n</body>\n</html>"""
        }

      
      /**
       * Returns the beginning of a text table.
       * @return  String containing the beginning of a text table. 
       */
      def textTable_getBegin() : String = { s"""<table class="table-text">""" } 

      /**
       * Returns the end of a text table.
       * @return  String containing the end of a text table. 
       */
      def textTable_getEnd() : String = { s"""</table>\n\n""" } 
      
      /**
       * Returns the a row element of a text table with 2 columns comment and text.
       * @param comment String with a comment on the text.
       * @param text    String with the text.      
       * @return        String containing a row in text table. 
       */
      def textTable_getEntry(comment: String, text: String) : String = {
        s"""<tr class="table-text-line">\n<td class="table-text-comment">$comment</td>\n<td class="table-text-text">$text</td>\n</tr>\n"""
      } 

      /**
       * Returns the beginning of a code table.
       * @return  String containing the beginning of a code table. 
       */
      def codeTable_getBegin() : String = { """<table class="table-code">""" }
      
      /**
       * Returns the end of a code table.
       * @return  String containing the end of a code table. 
       */
      def codeTable_getEnd() : String = { s"""</table>\n\n""" }
      
      /**
       * Returns the a row element of a code table with 3 columns comment, lineNumber and code.
       * @param comment     String with a comment on the text.
       * @param lineNumber  Int line in source code.
       * @param code        String with code section.      
       * @return            String containing a row in code table. 
       */
      def codeTable_getEntry(comment: String, lineNumber: Int, code: String) : String = {
        val lineNumStr = lineNumber.toString()
        s"""<tr id="LCONT$lineNumStr" class="table-code-line">\n\t<td id="LCOM$lineNumStr" class="table-code-comment">$comment</td>\n\t<td id="L$lineNumStr" class="table-code-linenumber" data-line-number="$lineNumStr">$lineNumStr</td>\n\t<td id="LC$lineNumStr" class="table-code-code"><pre>$code</pre></td>\n</tr>\n""" 
      }
}