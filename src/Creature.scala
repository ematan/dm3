import scala.math._

// Creature-luokkaan kuuluvat kaikki elävät olennot pelimaailmassa.
class Creature extends GameObject{
  isCreature = true
  blocking   = true
  str  = D6+D6+D6
  dex  = D6+D6+D6
  size = D6+D6+D6
  con  = D6+D6+D6
  int  = D6+D6+D6
  
  maxhp = size*con+level+50
  hp    = maxhp
  
  maxmana = int*10
  mana    = maxmana 
  
  nextAction  = DM3.currentTime+D100+slowness
  DM3.actors += this                                          //lisää luodun olennon ArrayBufferiin, jossa kaikki (elävät) olennot ovat
   
  override def attack = (level+str+size)/10*D4
 
  
  // Määrittää monsterien liikkeitä. Käytännössä, jos monsteri on tarpeeksi lähellä ihmistä, monsteri pyrkii siirtymään kohti ihmistä
  // Monsteri voi siis pyrkiä "seinän läpi" ja näin ollen pelaaja kykenee käyttämään tätä strategisesti hyväkseen.
  // Mikäli ihminen on kaukana monsteri liikkuu sattumanvaraisesti ympäriinsä.
  override def selectAction{                                 
    if(!hitPlayer){
      if(hypot(DM3.player.location._1-this.location._1, DM3.player.location._2-this.location._2)<5){
        if(DM3.player.location._1>this.location._1){
          moveRight
        }else if (DM3.player.location._2>this.location._2){
          moveDown
        }else if (DM3.player.location._1<this.location._1){
          moveLeft
        }else{
          moveUp
        }
      }else{
        randomMove()
      }
    }
  }
   
  def randomMove(){
    var a = D4.toInt
    a match{
       case  1 => moveUp
       case  2 => moveDown
       case  3 => moveLeft
       case  4 =>  moveRight
       }
  }
  
  def waiting(){
    timeCost = 2
  }
  
  // Testaa, onko viereisessä ruudussa ihminen ja siirtyy tätä kohti (siis hyökkää)
  def hitPlayer():Boolean={
    val player=inMap.getNeighbours(location).find(_.contains.exists(_.playerControl))
    if(player.isDefined){
      moveTo(player.get)
    }
      player.isDefined
  }
}

