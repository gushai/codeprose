package org.codeprose.util

object StringUtil {

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
  
  
}