import scala.util.Random

class MapTile(position:Tuple2[Int,Int]) extends GameObject{
  var producedLight = 0
  override def produceLight = {
    producedLight = this.lightSource+this.contains.map(_.produceLight).sum      
    producedLight     
  }
  
  location = position
  blocking = true
  var water = false
  val r = new Random
  protected var sprite = (45,12)  
  override def tileLocation = sprite
  override def blocks:Boolean = {
    if(this.blocking){
      true
    }else if(this.contains.isEmpty){
      false
    }else{
      this.contains.last.blocking
    }
  }   
}

//Alla olevat luokat määritelevät eri tyyppisiä tiiliä.
//Jotta Maa ja seinä eivät olisi identtiset, arvoimme joka tiilen spriten sarjasta spritejä
//Vesitiili jäi kiireessä käyttämättä, mutta olemme säilyttäneet sen tulevaisuuden varalle.

class WaterTile(position:Tuple2[Int,Int]) extends MapTile(position){
  water = true  
  blocking = true
  
  sprite = (2,3)
}

class WallTile(position:Tuple2[Int,Int]) extends MapTile(position){
  water = false
  blocking = true
  lightBlock = true
  
  val spriteA: Array[Tuple2[Int, Int]] = Array((45,12),(46,12),(41,12),(47,12))
  sprite = spriteA(r.nextInt(4))
}

class GroundTile(position:Tuple2[Int,Int]) extends MapTile(position){
  water = false  
  blocking = false
  
  val spriteB: Array[Tuple2[Int, Int]] = Array((14,13),(15,13),(16,13),(17,13),(18,13),(19,13),(20,13),(21,13)) 
  sprite = spriteB(r.nextInt(8))
}