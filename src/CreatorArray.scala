import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class CreatorArray[T<:GameObject] extends ArrayBuffer[T] {
  
  val rand = new Random(System.nanoTime)
  def getRandom:T = {
    val probs = this.map {_.spawnProbability}.scan(0)(_+_)
    val select = rand.nextInt(probs.last)
    this(probs.count {_<=select }-1)   
  }
  
  def createRandom=getRandom.createNew  
}


class SelectorArray[T<:GameObject] extends ArrayBuffer[T] {
  val rand = new Random(System.nanoTime())
  def getRandom:T = {
    val select = rand.nextInt(this.size)
    this(select)
  }
  
  //palauttaa satunnaisen tuoksen, jolla "f"-ehto on voimassa
  def getRandom(f:T=>Boolean):T = {
    
    var select=this(rand.nextInt(this.size))
    while(!f(select)){
      select=this(rand.nextInt(this.size))
    }
    select
  } 
  
  def filtered(f:T=>Boolean):SelectorArray[T]={
    new SelectorArray()++=this.filter(f)
  }
  
  def getMany(unique:Boolean=false,count:Int,f:T=>Boolean):ArrayBuffer[T]={
    val a=filtered(f)
    val value=new ArrayBuffer[T]
    while(value.size<count) {
      var temp=a.getRandom
      if(!unique || !value.contains(temp)){
        value+=temp
      }
         
    }     
    return value
  }
}

object Util{
  def distance(a:Tuple2[Int,Int],b:Tuple2[Int,Int]):Int = {
    Math.abs(a._1-b._1)+Math.abs(a._2-b._2)
  }
  
  def distance(a:GameObject,b:GameObject):Int = distance(a.location,b.location)
}