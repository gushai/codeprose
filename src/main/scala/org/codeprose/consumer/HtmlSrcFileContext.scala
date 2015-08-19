package org.codeprose.consumer


class HtmlSrcFileContext(filename: String, packag: String, 
    val filenamesOriginalToOutputNames : Array[(String,String)]) {

    val tokenToHtmlEntry = new TokenToOutputEntryHtml(filenamesOriginalToOutputNames)

  
	    val fileNameWithoutPath =  filename.slice(filename.lastIndexOf("/")+1, filename.length())  
			val fileNameWithoutPathAndEnding = fileNameWithoutPath.slice(0,fileNameWithoutPath.lastIndexOf("."))		
			val fileNameEnding = fileNameWithoutPath.slice(fileNameWithoutPath.lastIndexOf(".")+1,fileNameWithoutPath.length)

      
			def getBegin() : String = {
      
      val perTokenScripts = tokenToHtmlEntry.scriptElements.mkString("\n", "\n\n", "\n")
      
      
      val perFileScripts = List(
      s"""
        // Highlight where used within file
        $$("[id^='T']").hover( 
          function(){ toHighlight = $$(this).data("cp-whereusedinfile"); $$(toHighlight).toggleClass("highlightWhereUsedWithinFile"); },
          function(){ toHighlight = $$(this).data("cp-whereusedinfile"); $$(toHighlight).toggleClass("highlightWhereUsedWithinFile"); }
        );""",    
      s"""
        // Highlight implicit conversions and parameters
      function highlightImplicitConversionsAndParameters(){

        // Implicit conversions
        $$('*[data-cp-implicitconversion=true]').toggleClass("highlightImplicitConversion");

        // Implicit parameters
        // Color to use highlightImplicitParameter
      }""",
      s"""
      // Key events     
      $$(document).keypress(function(e){
        // i
        if(e.keyCode == 105){
          highlightImplicitConversionsAndParameters()
        }     
      });""",
      s"""
            // Create tooltip entries
      function createTooltipHtmlFromDataAttr(elem) { 
    
        // Fullname
          fullname = "<b>" + $$(elem).data("cp-fullname") + "</b>";
        
    // TypeId
    typeId = $$(elem).data("cp-typeid")

    // Declaration
    rawLinkToDeclaration = $$(elem).data("cp-declaredat");
    linkToDeclaration = ""
          if(rawLinkToDeclaration){
      console.log( rawLinkToDeclaration);
            linkToDeclaration = "<a href='" + rawLinkToDeclaration + "'>Declaration</a>" + "<br/>";
          }

      // Definition
      rawLinkToTypeDef = mappingToTypeDefinition(typeId)
        
      linkToDefintion = "";
      if(rawLinkToTypeDef.length !=  0){
        linkToDefintion = "<a href='" + rawLinkToTypeDef + "'>Definition</a>" + "<br/>";
      }       

      // Where used in project
      rawLinkToWhereUsedProject = mappingToWhereUsedInProject(typeId)
          
      linkToWhereUsedInProject = "" 
      if(rawLinkToWhereUsedProject.length  !=  0){
        linkToWhereUsedInProject = "<a href='" + rawLinkToWhereUsedProject + "'>Where used in project</a>" + "<br/>";
      }      
    
      // Implicit conversion 
      linkToImplicitConversion =  "";
      isImplicitConversion = $$(elem).data("cp-implicitconversion");
      if(isImplicitConversion){
    
      implicitConversionFullname = $$(elem).data("cp-implicitconversionfullname");
      implicitConversionDeclaredAt = $$(elem).data("cp-implicitconversiondeclaredat");   
  if(implicitConversionDeclaredAt != null){
        linkToImplicitConversion = "<a href='" + implicitConversionDeclaredAt + "'>Impl. conv: " + implicitConversionFullname + "</a>" + "<br/>";
  } else {
    linkToImplicitConversion = "Impl. conv: " + implicitConversionFullname + "<br/>";
  }
  }

  // Implicit Parameter
          linkToImplicitParameter = ""
  isImplicitParameter = $$(elem).data("cp-implicitparameter");
  if(isImplicitParameter){
        implicitParameterFullname = $$(elem).data("cp-implicitparameterfullname");
       implicitParameterDeclaredAt = $$(elem).data("cp-implicitparameterdeclaredat");   
    if(implicitParameterDeclaredAt != null){
    linkToImplicitParameter = "<a href='" + implicitParameterDeclaredAt + "'>Impl. para: " + implicitParameterFullname + "</a>" + "<br/>";
    } else {
      linkToImplicitParameter = "Impl. conv: " + implicitConversionFullname + "<br/>";
    } 
  };
      
  // Output structure 
          html = "<div class='cp-tooltip'>" + fullname + "<br/><br/>" +
            linkToDeclaration +
            linkToDefintion +
            linkToWhereUsedInProject +
            linkToImplicitConversion +
            linkToImplicitParameter + 
            "</div>";
      
          return html;
      }
""",
      s"""
        // Tooltip
        // $$("[id^='T']").tooltip({
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

            function () {
                $$(this).stop(true).fadeTo(400, 1);
            },

            function () {
                $$(this).fadeOut("100", function () {
                    $$(this).remove();
                })
            });
        }
    });"""
      ).mkString("\n", "\n\n", "\n")
      
      
      s"""<!doctype HTML>
			<html lang="en">
			<head>
			<meta http-equiv="content-type" content="text/html; charset=utf-8" />
			<link rel="stylesheet" type="text/css" href="../style/style.css" media="screen" />
      <link rel="stylesheet" type="text/css" href="http://ajax.aspnetcdn.com/ajax/jquery.ui/1.10.0/themes/black-tie/jquery-ui.css" media="screen" />
			<title>$fileNameWithoutPath</title>
      <script src="../js/codeprose.global.js"></script>
      <script src="https://code.jquery.com/jquery-1.10.2.js"></script>
      <script src="http://ajax.aspnetcdn.com/ajax/jquery.ui/1.10.0/jquery-ui.js"></script>
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
			<span style="font-size:1.5em;font-weight:bold;" title="$filename">$fileNameWithoutPath&nbsp;</span><span style="font-size:1em;">$packag</span></div>
			<div style="float:right;"><a style="color:black;font-weight:bold;" href="../index.html">overview</a></div>
			</div>
			<div class="content">\n"""
      }
			
      def getEnd() : String  = {
        s"""
          
      <div class="footer">generated by codeprose. help and support on <a href="http://github.com/sth/codeprose" target="blank">github</a>.</div>
			</div></div></body></html>"""
        }

      
      def textTable_getBegin() : String = { s"""<table class="table-text">""" } 

      def textTable_getEnd() : String = { s"""</table>\n\n""" } 
      
      def textTable_getEntry(comment: String, mainText: String) : String = {
        s"""<tr class="table-text-line">
<td class="table-text-comment">$comment</td>
<td class="table-text-text">$mainText</td>
</tr>"""
      } 

      def codeTable_getBegin() : String = { """<table class="table-code">""" }
      
      
      def codeTable_getEntry(lineNumber: Int, comment: String, code: String) : String = {
        val lineNumStr = lineNumber.toString()
        s"""<tr id="LCONT$lineNumStr" class="table-code-line" >
<td id="LCOM$lineNumStr" class="table-code-comment">$comment</td>
<td id="L$lineNumStr" class="table-code-linenumber" data-line-number="$lineNumStr">$lineNumStr</td>
<td id="LC$lineNumStr" class="table-code-code">
<pre>$code</pre>
</td>
</tr>""" 
      }
      
      def codeTable_getEnd() : String = { s"""</table>\n\n""" }
      
}