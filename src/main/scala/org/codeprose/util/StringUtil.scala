package org.codeprose.util

object StringUtil {

  /*
   * Finds the longest common prefix in a list of strings.
   */
  def findLongtestCommonPrefix(s: List[String]) : String = {
     if(s.isEmpty)
       return ""
     else if (s.length==1){
       return s(0)
     }
     else {        
       val shortestStringLength = s.map(s=>s.length).foldLeft(Int.MaxValue){(min,a)=>Math.min(min, a)}      
       var k=0
       var matches = true
       while(k < shortestStringLength && matches ){         
         var i=0
         while(i< (s.length-1) && matches){
           if(s(i).charAt(k) != s(i+1).charAt(k)){
             matches = false              
           }
           i+=1
         }
         if(matches)
           k+=1
       }
     
       if(k>0)
         return s(0).take(k)
       else
         return ""
     }
  }
  
   /*
   * Removes the longest common path of all elements.
   */
  def getUniqueShortFileNames(s: List[String]) : List[String] = {      
    if(s.size<=1){return s }
    val prefixRaw = findLongtestCommonPrefix(s)
    val prefixLength = Math.min(prefixRaw.length,prefixRaw.lastIndexOf("/")) 
    val sliceFirstIndex = List.fill(s.length)(prefixLength)  
    (sliceFirstIndex zip s).map(e => (e._2.slice(e._1,e._2.length)))
  }
    
  /*
   * Groups files by path after removing the most common path
   */
  def getUniqueShortFileNamesGroupedByDirectory(s: List[String]) : Map[String,List[String]] = {      
    if(s.size==0) { 
      return Map[String,List[String]]()
    } 
    val files = s.toArray
    scala.util.Sorting.quickSort(files)
    val shortened = getUniqueShortFileNames(files.toList)
   
    shortened.map(e => {
     val idx = e.lastIndexOf("/")
     (e.slice(0,idx),e.slice(idx+1,e.length))
    }).groupBy(e => e._1).mapValues(e => e.map(e => e._2))    
  }
  
  
  
  
}