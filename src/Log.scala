import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

//MyLog olio ylläpitää ns. battlelogia, joka näkyy pelaajalle peliruudussa
//Log on käytännössä ArrayBuffer täynnä String-rivejä, joita päivitetään pelin aikana.

class MyLog extends ArrayBuffer[String]{
  var linesVisible = 20
  
  implicit def AsString(x:MyLog):String = x.toString()

  def := (a:String){
    this+=a
  }
  
  override def toString():String = {
    val total = math.min(this.size-1,linesVisible)
    var answer = ""
    if(this.size == 0) return ""
    for(n<-this.length-total-1 to this.length-1){
      answer += apply(n)+"\n"
    }  
    answer   
  }
}