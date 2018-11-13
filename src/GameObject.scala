import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.util.Random
import scala.math._

class GameObject(var xLocation:Int,var yLocation:Int) {
    
    //Olioiden yleisiä ominaisuuksia
    var name = this.getClass.getName
    var isItem     = false
    var isPotion   = false
    var isWeapon   = false
    var isDummy    = false
    var isCreature = false
    var playerControl = false
    var playerVision = false
    var inMap:GameMap = null
    var lightLevel   = 0
    var lightSource  = 0  
    def produceLight = 0
    var lightBlock = false
    var blocking = false 
    var died = false
    var level = 1
    def levelCalc = str+int
    
    //Erityisesti Olentojen hyödyntämät
    var str  = 0
    var size = 1
    var dex  = 0
    var int  = 0
    var con  = 0    
    var maxhp = 0
    var maxmana = 0
    var hp = 0
    var mana = 0
    var exp = 0
    var difficulty = 10
    def killExp = level*difficulty                     //exp-määrä, jonka olento luovuttaa kuollessaan
    
    //Erityisesti Esineet:
    var itemClass = ""
    var manaCost = 0
    
    //Aijan kuluun liittyvät:
    var slowness = 1000  // aikayksikköjen määrä, jotka kestävät kunnes olio "palautuu" edellisestä siirostaan. Default 1000
    var nextAction = -1  // seuraavan vuoron tapahtumishetki. Negative luku = ei koskaan.
    var timeCost = 0 //summa joka nollaantuu actionien välillä.  mahdollistaa summauksen eri funktioissa.
    Counter += 1
    
    var damage = new ArrayBuffer[Damage](0)  //list of overtime Damage
   
    def tileLocation = (0,0)   //sijaintikoordinaatit tileset.png-kuvasta josta se poimitaan.
    var location = (xLocation,yLocation)
    
    //Esineistä:
    var contains = new ArrayBuffer[GameObject](0)                  //pelaajan inventory
    var activeWeapons: Map[String, Item]  = Map[String, Item]()    //slotit pelaajan "aktiivisille aseille" (2kpl)   
    var equippedWeapon: Item = null                                //tällä hetkellä aktivoitu ase

    var uses = new ArrayBuffer[Item](0)                            //potentiaalinen toinen bufferi esineiden säilytykseen
    
    
    
    def spawnProbability = if(this.isCreature && !this.playerControl){
      (1000.0 / abs((if(DM3.initialized)DM3.player.level else 1)*10  - this.difficulty)).toInt
    }else 1
    
    
    //Olentojen toiminta:   
    def selectAction(){
      timeCost = 20
      doneAction
    }
    
    def doAction(){
      selectAction()
    }
       
    def logIfClose(text:String,dist:Int=8) = {     //Jos olento on tarpeeksi lähellä ihmistä, sen toimet kirjautuvat battlelogiin
      if(Util.distance(this,DM3.player)<dist){
        DM3.log+=text
      }
    }
    
    //Taisteluun liittyviä:
    var armour = 0                                                 //toistaiseksi armoria ei hyödynnetä
    def reduction(recieved:DamageType) = armour/recieved.divider   //ei myöskään siitä saatavaa vähennystä
    var attackType:DamageType = Smash
    
    def attack = 0
    def immunity(hit:DamageType)=uses.exists(_.givesImmunity(hit))
    def givesImmunity(hit:DamageType) = false  
    def hitprobability = 0 
    def avoidance = 0
    def damageDuration = 0
    
    def die() = {                                                  //kun hahmo kuolee
      if(this.playerControl) {        
        //println("Hiparit: "+this.hp)  //debuggausta varten
        if(DM3.musicOn) DM3.soundDie.trigger()                     //jos oli ihminen, peli päättyy
          DM3.end()
      }else{
        this.died = true                                           //jos oli olento, poista hahmo pelistä ja pudota
        logIfClose(""+this+" DIED")                                //hahmon mahdolliseet esineet maahan
        DM3.actors -= this
        val tile = inMap.getTile(location._1, location._2)
        tile.contains -= this
        this.contains.foreach { tile.contains += _ }
        this.contains.clear()
      }
    }
    
    def doneAction{
      DM3.currentTime=this.nextAction
      timeCost = Math.max(timeCost,0)
      this.nextAction += timeCost*slowness/100  //tämä tarkoittaa että 10 kokoinen actioni default slownessilla = 1000
      DM3.actors = DM3.actors.sortBy( a => a.nextAction) //tehoton, juuri nyt ei väliä ja mahdollistaa triviaalisti stunnauksen.
      timeCost=0
    }
   
   def blocks:Boolean = this.blocking                              //testaa, voiko ruutuun kävellä
   def this() = this(0,0)
   
   def slowdown(target:GameObject) = {                             //tappelumetodi, joka hidastaa vastusta
     target.slowness += this.equippedWeapon.int*100
   }
   
   def dealDamage(target:GameObject) = {                          // Teoriassa:Summaa damaget tyypettäin ja antaa ne kohteelle tyypeittäin.
     var result = new HashMap[DamageType,Damage]                  // Nyt: tekee attack-arvon verran damagea
     result += Tuple2(attackType, new Damage(this.attack,attackType,0))      //(muu koodi jätetty ennalleen tulevaisuuden varalle)
     for(item <- uses){
       if(item.attack>0){
         if(result.contains(item.attackType)){
           var temp = result(item.attackType)
           temp.amount += item.attack
           temp.duration = Math.max(temp.duration,item.damageDuration)
         }else result += Tuple2(item.attackType,new Damage(item.attack,item.attackType,item.damageDuration))      
       }
     }
     
     var total=max(result.toArray.map(a=>target.recieveDamage(a._2)).sum, 1)
     
     var damageText=target.maxhp/total match{
       case 3 => " DAMAGES "
       case 2 => " EVISCERATES "
       case 1 => " DESTROYS "
       case 0 => " OBLITERATES "
       case default=>" HIT "
     }
     
     var resultText = ""
     if(this.playerControl){
       resultText = this.name+damageText+target.name +" for "+ total + " DMG"
       DM3.log += resultText 
     }else if(target.playerControl){
       resultText = this.name+damageText+target.name +" for "+ total + " DMG"
       DM3.log += resultText 
     }
      
     // logIfClose(this.name+damageText+target.name +" for "+ total + " DAMAGE")   //<--- tämä metodi käyttöön, jos halutaan tieto myös monsterien väisestä tappelusta  
     
     if(target.died) this.expUp(target.killExp)     
   }
   
   def recieveDamage(recieved:Damage):Int={
     if(immunity(recieved.tyyppi)) return 0
     recieved -= this.reduction(recieved.tyyppi)
     for(item<-uses){
       recieved -= item.reduction(recieved)
     }
     if(recieved.toInt>0) this.hp -= recieved.toInt
     if(this.hp<1) die()
     recieved.amount
   }
   
 def moveUp:Boolean=moveTo(this.location._1,this.location._2-1)
 def moveDown:Boolean=moveTo(this.location._1,this.location._2+1)
 def moveLeft:Boolean=moveTo(this.location._1-1,this.location._2)
 def moveRight:Boolean=moveTo(this.location._1+1,this.location._2)
 
  def fight(other:GameObject){
    dealDamage(other)
    if(this.playerControl && this.mana>= this.equippedWeapon.manaCost && this.equippedWeapon.manaCost !=0){
     slowdown(other)
     this.mana=max(this.mana-this.equippedWeapon.manaCost,0)
     DM3.log += "YOU SLOWED THE ENEMY DOWN"
    }
    timeCost += 1
    if(playerControl && DM3.musicOn){
      DM3.soundSwing.trigger()
    }
 }
    def moveTo(x:Int,y:Int):Boolean={  
      var returnvalue=true
      if(inMap != null){
        val target = inMap.getTile(x, y)
        if(target.blocks){
          returnvalue = false
          if(target.contains.exists { _.isCreature}){
            fight(target.contains.find { _.isCreature}.get)
          } else {
            timeCost -= 1
            if(playerControl && DM3.musicOn){
              DM3.soundWall.trigger()
            }
          }
        } else{
        target.contains += this       
        inMap.tiles(location._1)(location._2).contains -= this  
        location = (x,y)
        this.pickItemUp(inMap.getTile(location._1, location._2))
        this.manaRegen
        if(playerControl && DM3.musicOn){
          DM3.soundStep.trigger()
        }
        }    
      }  
      timeCost+=8
      doneAction
      returnvalue
    }
    def moveTo(tile:MapTile):Boolean = {
      moveTo(tile.location._1,tile.location._2)
    }
   def setLocation(x:Int,y:Int,map:GameMap){
     
     this.inMap=map
     if(map!=null){
        val target=map.tiles(x)(y)
     
         target.contains+=this       
     }
      location = (x,y)
   }
    def setLocation(t:MapTile){
      if(inMap != null) inMap.getTile(location).contains-=this
      t.contains += this
      this.inMap = t.inMap
      this.location = t.location
    }
   
    def createNew = this.getClass.getConstructor().newInstance()  
    
    def methods = this.getClass.getMethods()
    def getPlayerControl() = {
      playerControl = true
      playerVision = true
    }
    val arr = this.contains

    
    def manaRegen = this.mana = min(mana+1, maxmana)
    
    def addItem(item:GameObject) = contains += item
    def deleteItem(item: GameObject) = {this.contains -= item}
    def equipItem(slot:String) = if(activeWeapons.get(slot) != None) equippedWeapon = activeWeapons(slot)
    def inventory = contains
    
    def pickItemUp(tile: MapTile) = {                             //poimii esineet ylös maasta
      if(playerControl && tile.contains.exists(_.isItem)){                
        val items = ArrayBuffer[GameObject]()
        for (x <- 0 until tile.contains.length-1){
          if(tile.contains(x).isItem){
            items += tile.contains(x)
          }
        }
        items.foreach(tile.deleteItem(_))
        items.foreach(n => if(!n.isWeapon){
          DM3.log += ""
          DM3.log += "YOU FOUND A POTION"
          DM3.log += ""
          this.addItem(n)})       // kaikki nonWeaponit (eli käytännössä potionit) menee inventoryyn
        
        items.foreach(n => if(n.checkAndEquip(n.itemClass))DM3.player.equippedWeapon =DM3.player.activeWeapons(n.itemClass))
        
        if(DM3.musicOn) DM3.soundPick.trigger()
        
      }
    }
    
    def checkAndEquip(slot:String) ={
      if(this.isWeapon){
        if(this.isBetterThan(DM3.player.activeWeapons(slot))){ 
          var old = DM3.player.activeWeapons(slot).name
          var route = if(DM3.player.activeWeapons(slot).isDummy) 1 else 2
          
          DM3.player.activeWeapons(slot) = this.asInstanceOf[Item]
          
          if (route ==2){
            DM3.log += ""
            DM3.log += "THIS "+ this.name + " iS" 
            DM3.log += "BETTER THAN THE OLD ONE, SO" 
            DM3.log += "YOU DISCARD YOUR PREVIOUS "
            DM3.log += old
            DM3.log += "LIKE THE TRASH IT WAS."
            DM3.log += ""         
          }else{
            DM3.log += ""
            DM3.log += "WOW, YOU KNEW YOU WERE"
            DM3.log += "MISSING SOMETHING!"
            DM3.log += "YOU EQUIP " + this.name
            DM3.log += ""
          }
          true
        }else false
      }else false
    }
    
    def isBetterThan(targetWeapon: GameObject): Boolean ={
      this.levelCalc > targetWeapon.levelCalc
    }
    
    def drink= {
      if(this.contains.exists(_.isPotion)){
        useItem(this.contains.find(_.isPotion).get)
        DM3.timeMoves=true
      }else {
        DM3.showString += "\nYOU REACH FOR A POTION, BUT THERE ARE NONE IN YOUR INVENTORY\n\n(MAYBE YOU SHOLD KILL SOME LIZARDS?)\n"
        DM3.timeMoves=false
      }
    }
    def useItem(item: GameObject) = {                                // vain potionin juomiseen
      if(this.contains.contains(item)){
        hp = min(maxhp, hp+item.hp) 
        this.contains -= item
      } 
    }
    
    def lvlUp = {
      this.level += 1
      var strup = (D6/2).toInt
      this.str += strup
      var dexup = (D6/2).toInt
      this.dex += dexup
      var conup = (D6/2).toInt
      this.con += conup
      var intup = (D6/2).toInt
      this.int += intup
      this.maxhp =this.con*10+this.size*5
      this.maxmana = this.int*10
      this.hp += conup*10        // tason noustessa saa uuden conin verran lisää hp
      if(this.playerControl) DM3.log += "\nYOU LEVELED UP TO: " + this.level+ "\nYOU GAIN STR:"+strup+" DEX:"+dexup+" INT:"+intup+" CON:"+conup
    }
    
    def expUp(expInc: Int)={
    if(this.playerControl) DM3.log += "YOU GAINED " + expInc + " EXPERIENCE"
    if(exp + expInc <(100 + (this.level-1)*50)){
        exp += expInc
        
    }else{
        this.lvlUp
       
        exp = exp + expInc - (100 + (this.level-2)*50)
    }
    }
  override def toString=this.getClass.getName
}


class Damage(var amount:Int,var tyyppi:DamageType,var duration:Int=0){
    def +(other:Int):Damage=new Damage(amount+other,tyyppi,duration)
    def -=(other:Int):Damage={
      this.amount-=other
      this
    }
    def toInt=this.amount
}


class DamageType{
  var divider=1
  var slows=0
}
//object Fire extends DamageType{               //erilaisia Damage-tyyppiä hyödyntäviä luokkia. Ei juuri nyt implementoituja, jooten kommentoitu piiloon.
//  divider=20
//}
//object Cut extends DamageType
object Smash extends DamageType{
  slows=1
}


//Olioiden ominaisuuksien arpomista varten luodut virtuaalinopat

class Dice(max:Int){
  import scala.language.implicitConversions
  val r = new Random(System.nanoTime())
  def apply() = 1+r.nextInt(max)
  def +(other:Int):Int = this()+other
  implicit def diceToInt(D:Dice):Int = D()
}

object Dice{
  def apply(a:Int) = new Dice(a)
}

object D6 extends Dice(6)
object D10 extends Dice(10)
object D100 extends Dice(100)
object D8 extends Dice(8)
object D20 extends Dice(20)
object D4 extends Dice(4)

object Counter{                           //debuggausta varten luotu luokka, jotta voidaan seurata olioiden vuorojen etenemistä
  var count = 0
  def +=(a:Int) = {
    count += a 
  }
 implicit def counterToInt():Int = count 
}

