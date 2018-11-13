
import scala.collection.mutable.ArrayBuffer
class GameMap(size:Int) {
  var tiles = Array.tabulate[MapTile](size,size) {(i,j)=>new WallTile((i,j))}  // Luo pelikentän täynnä seiniä.
  
  val bgTile = new MapTile(-1,-1)  //taustatiili "reuna-alueita" varten
  
  this.compileRooms(0,0)  // (0,0) on tämän kartan ylävasen koordinaatti. Sen voi vaihtaa, jos kartan 
                          //sijoittaa osaksi isompaa karttakokonaisuutta. Huoneet sijaitsevat suhteessa tähän koordinaattiin

  val spawnable = new SelectorArray[MapTile]
  tiles.foreach(_.foreach(a=>if(!a.blocking) spawnable+=a))
  tiles.foreach(_.foreach(t=>t.inMap=this))
  
  
  // room1 ja room2 sijoittavat maa-tiiliä pelikentälle annettujen koordinaattien ja leveyksien perusteella.
  private def room1(sizeX:Int ,sizeY:Int, coordX: Int, coordY:Int): Unit = {     
    for(x<- coordX until (coordX + sizeX)){
      for(y <- coordY until (coordY + sizeY)){
        if(!((y == coordY || y == coordY+sizeY-1) && (x == coordX || x == coordX+sizeX-1)))
        tiles(x)(y) = new GroundTile((x,y)) 
      }
    }
  }
  
  private def room2(sizeX:Int ,sizeY:Int, coordX: Int, coordY:Int):Unit = {    //käytävä"huoneet" tulee tähän
    for(x<- coordX until (coordX + sizeX)){
      for(y <- coordY until (coordY + sizeY)){
        tiles(x)(y) = new GroundTile((x,y))
      }
    }  
  }
  
  def computeLigh{
    this.tiles.foreach(_.foreach { x =>x.produceLight})
  }

  //komento kutsuu room1 ja room2 -metodit kaikille määritellyille huoneille
  def compileRooms(x: Int, y: Int) ={
    this.room1(x+ 12, y+ 8,   20,  2)  //vasemmalta, ylhäältä alkaen
    this.room1(x+ 20, y+ 9,   16,  9)
    this.room1(x+ 8,  y+ 21,  29,  22)
    this.room1(x+ 5,  y+ 10,  25,  25)
    this.room1(x+ 6,  y+ 5,   20,  30)
    this.room1(x+ 10, y+ 16,  5,   37)
    this.room1(x+ 20, y+ 6,   8,   49)
    
    this.room1(x+ 9,  y+ 33,  9,   63) // vasemalla alin
    this.room1(x+ 8,  y+ 10,  17,  83)
    
    this.room1(x+ 10, y+ 10,  30,  76)
    this.room1(x+ 21, y+ 7,   34,  85)
    
    this.room1(x+ 10, y+ 8,   45,  30) // keskellä ylhäällä
    this.room1(x+ 11, y+ 20,  52,  45)
    this.room1(x+ 7,  y+ 10,  46,  48)
    
    this.room1(x+ 13, y+ 19,  58,  5) // oikea ylä
    this.room1(x+ 33, y+ 12,  65,  20) 
    this.room1(x+ 10, y+ 5,   65,  69)
    this.room1(x+ 5,  y+ 10,  65,  69)
    this.room1(x+ 9,  y+ 28,  88,  47)
    this.room1(x+ 10, y+ 10,  84,  70)
    
    this.room1(x+ 20, y+ 5,   70,  86)
    this.room1(x+ 15, y+ 10,  70,  86)
    
    //tästä alkaa käytävät
    
    this.room2(x+ 3,  y+ 4,   31,  18)
    this.room2(x+ 11, y+ 3,   9,   31)
    this.room2(x+ 3,  y+ 3,   9,   34)
    this.room2(x+ 18, y+ 3,   28,  51)
    this.room2(x+ 3,  y+ 8,   11,  55)
    this.room2(x+ 3,  y+ 9,   52,  37)
    this.room2(x+ 3,  y+ 8,   53,  65)
    this.room2(x+ 12, y+ 3,   53,  73)
    this.room2(x+ 10, y+ 3,   75,  70)
    this.room2(x+ 3,  y+ 15,  91,  32)
    this.room2(x+ 3,  y+ 6,   86,  80)
    this.room2(x+ 15, y+ 3,   55,  87)
  }
  

  def getNeighbours(t:Tuple2[Int,Int]):Array[MapTile] = {
    getNeighbours(t._1,t._2)
  }
  def getNeighbours(x:Int,y:Int):Array[MapTile] = {
    Array(getTile(x+1,y),getTile(x-1,y),getTile(x,y+1),getTile(x,y-1))
  }
  
  /*https://en.wikipedia.org/wiki/Bresenham's_line_algorithm
   Pixeleiden piirron sijaan poimitaan vastaavat karttaruudut*/
  def getLine(x1:Int,y1:Int,x2:Int,y2:Int):Array[MapTile] = { 
    var y = y1
   
    if(x1 == x2){
      if(tiles.isDefinedAt(x1)){       
        tiles(x1).slice(y1,y2)
      }
    }
   
    val answer =new ArrayBuffer[MapTile]
    val xdiff:Double = x1-x2                                  
    val ydiff:Double = y1-y2
    val errDelta = Math.abs(ydiff/xdiff)
    var err = errDelta-0.5
    for(x<- x1 to x2){
      answer += getTile(x,y)
      err += errDelta
      if(err >= 0.5){
        y = y+1
        err-= 1
      }
    }   
    answer.toArray
  }
  
  
  def getTile(x:Int,y:Int):MapTile = {
    if(x<0|y<0|x>=tiles.size||y>=tiles(x).size){
      bgTile
    }else tiles(x)(y)
  }
  def getTile(t:Tuple2[Int,Int]):MapTile = {
    getTile(t._1,t._2)
  }
}