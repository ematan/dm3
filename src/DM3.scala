
import processing.core._
import scala.collection.mutable.ArrayBuffer
import java.awt.event  // Käytetään näppäin koodi vakioihin.
import ddf.minim.Minim

object  DM3 extends PApplet {
  var initialized=false  //jotta DM3 construktorissa ei liian aikaisin luotujen olioiden konstruktorit käytä DM3:n arvoja
  var currentTime=0
  var actors=new ArrayBuffer[GameObject]() // actors has all the things that do actions in a game world, waiting their turn. 
                                           // Keep it sorted by nextAction. First element=current actor.
  val creatures = new CreatorArray[Creature]()
  creatures++=Array[Creature](new Human, new Pig, new Lizard, new Spider, new Dragon, new Harpy, new Shadow, new Elephant)
  actors--=creatures
 
  val tileset      = loadImage("tileset.png")
  val messageStart = loadImage("DM3-start.png")
  val messageWin   = loadImage("DM3-win.png")
  val messageLose  = loadImage("DM3-lose.png")
  val messageCred  = loadImage("DM3-credits.png")
  var message:PImage=null           // Viestiruudut
  
  var myfont = createFont("prstartk.ttf", 12) 

  
  val tileXsize=32  // 
  val tileYsize=32
  
  val mapSize=100
  var map=new GameMap(mapSize)
  
  var viewWidth=25  // pelikartan koko kartta tiilinä laskettuna
  var viewHeight=25 
  val infoWidth=400 // info ikkunan leveys pikseleinä.
  var center=(0,0)     // Kartan keskitys tähän näytössä. Osoite on osoita kartassa ei näytössä.
  
  var tileClicked=(0,0)  //Ei käytössä mutta lähes valmis hiiri kontrolli jos halua valita olion hiirellä kartasta.
  
 
  
  var log=new MyLog   //pelin toiminta logi. joka näytetään pelaajalle tekstikentässä.
  var timeMoves = true
  
  var musicOn = true
  val minim = new Minim(this);
  var soundtrack = minim.loadFile("A.mp3")
  var soundDie = minim.loadSample("die.wav")
  var soundStep = minim.loadSample("step.wav")
  var soundSword = minim.loadSample("sword.wav")
  var soundSwing = minim.loadSample("swing.wav")
  var soundWall = minim.loadSample("wall.wav")
  var soundPick = minim.loadSample("pickmetal.wav")
  var soundWin = minim.loadSample("win.mp3")
  
  val ArrowKeys=Array(event.KeyEvent.VK_UP,event.KeyEvent.VK_DOWN,event.KeyEvent.VK_LEFT,event.KeyEvent.VK_RIGHT)
  var minMonsters=20  // How many monsters the game
  var showString=""   // Säilyttää stringiä mikä näytetään näytöllä
  
  var player=creatures(0).createNew
  player.setLocation(20, 10, map)
  player.getPlayerControl()
  player.maxhp=player.con*5+player.size*10
  player.hp=player.maxhp
  player.activeWeapons += ("1h" -> new Dummy, "2h" -> new Dummy)
  player.equippedWeapon = player.activeWeapons("1h")
  player.contains += new Potion
  initialized=true //Tämän jälkeen konstruktorissa luodut monsterit voivat hyödyntää yllä määriteltyjä arvoja.
  
  
   override def setup={
      val args = Array[String]("","")      
      size(tileXsize*viewWidth+infoWidth,tileYsize*viewHeight+1)  
      soundtrack.loop();
      this.message=this.messageStart
	    this.loadPixels    // Tällä rivillä varmistellaan että on processing on allokoinut pixels puskurin.
	    
	  //  this.frameRate(30.toFloat)  // jos hidastuu liikaa processing vuoksi niin puolitetaan nopeus jolla se tapahtuu
	                                  // eli framerate rajoittaa.
  }
  
  
  def createMonster(creature:Creature,target:MapTile){
   creature.createNew.setLocation(target)
   
  }
  
  // Luo 100 monsteria satunnaisiin paikkoihin.
  map.spawnable.getMany(true, 100, m=>true).foreach(x=> creatures.getRandom.createNew.setLocation(x)) 
  actors.sortBy(_.nextAction)  
  
  def end(){
    message=this.messageLose
    soundtrack.pause()
    
  }
  def win(){
   message=this.messageWin
   soundtrack.pause()
   soundWin.trigger()
  }
  def winCondition:Boolean={
    player.level == 10
  }
  def loseCondition: Boolean= {
    player.hp<=0
  }
 
  /* PRrocessingin image käskyn käyttö ohjeiden mukaisella tavalla vuotaa muistia.
   *  piirroskäskyn image sisältävät pixelitaulukoita jotka jäävät pysyvästi muistiin.
   *  Ja ilmeisesti piirtäessä kerääntyvät johonkin kunnes tehot loppuu.
   */ 


  /* s=Source, t=target Lyhenteetmuuttujien edessä
   * 
   * Tämmä funktio kopioi puskurin pixeleitä toiseen puskuriin. vaatii syötteenä leveyden jotta voi laskea
   *  pikselien sijainnit puskureissa. 
   * 
   * 
   * */
  
  def copyRectangle(sX:Int,sY:Int,height:Int,width:Int,source:Array[Int],sWidth:Int,tX:Int,tY:Int,target:Array[Int],tWidth:Int)={
    for(y<-0 to height-1){
      for(x<-0 to width-1){
        target(x+tX+(y+tY)*tWidth)=source(x+sX+(y+sY)*sWidth)
      }
    }
  }
 
  /*Muuten sama kuin copyRectangle paitsi että sijoittamisen sijaan yhdistää ne käyttäen apu funktiota.
   *Korkeamman kertaluvun funktiot täyttävät muistin roskalla ja JVM roskien keruu sotkee.
   */
   def mergeRectangle(sX:Int,sY:Int,height:Int,width:Int,source:Array[Int],sWidth:Int,tX:Int,tY:Int,target:Array[Int],tWidth:Int)={   
    def merger(t:Int,s:Int):Int={  
      if((s>>24&255)==0) t else s  // Jos sourcen alfa kanava on nolla niin ei muuta kohde pikseliä.
   }
    for(y<-0 to height-1){
      for(x<-0 to width-1){
        target(x+tX+(y+tY)*tWidth)=merger( target(x+tX+(y+tY)*tWidth),source(x+sX+(y+sY)*sWidth))
      }
    }
  }
   
   

  /*Tämä piirtää tiileen haluttuun puskuriin. tilesetin perusteella. apu funktio jotta kaikkia argumentteja ei tarvitse
   * sijoitaa käyttöpaikassa, jälkimmäinen on täsmälleen samalla logiikalla  
   */
  
  def drawTile(x:Int,y:Int,loc:Tuple2[Int,Int],buf:Array[Int],bufWidth:Int)={
    copyRectangle(loc._1*tileXsize,loc._2*tileYsize,tileXsize,tileYsize,tileset.pixels,tileset.width,x,y,buf,bufWidth)
  }
  
    def mergeTile(x:Int,y:Int,loc:Tuple2[Int,Int],buf:Array[Int],bufWidth:Int)={
    mergeRectangle(loc._1*tileXsize,loc._2*tileYsize,tileXsize,tileYsize,tileset.pixels,tileset.width,x,y,buf,bufWidth)
  } 
 
    /*Draw funktion alku puoli sisältää ison määrän random work-aroundeja processingin synkronisaatio ongelmiin.
     * Yksinkertaisuudessaan jos viesti kuva on käytössä niin piirtää sen näytölle. Muussa tapauksessa piirtää 
     * eka kartta ruudut ja sen jälkeen karttaruutuun kaksi viimeisenä tullutta asiaa jotka ovat siinä.
     * 
     */
  
  override def draw():Unit={
      
       if(message!=null){
         copyRectangle(0,0,math.min(this.height-1,message.height-1),math.min(this.width-1,message.width-1),message.pixels,message.width,0,0,this.pixels,this.pixels.length/this.height)
         this.updatePixels()
       } else {
       center=player.location
       val left=center._1-viewWidth/2
       val top=center._2-viewHeight/2
       for(x<-0 to viewWidth-1){
         for(y<- 0 to viewHeight-1){
           var tile=map.getTile(left+x,top+y)
          drawTile(x*tileXsize,y*tileYsize,tile.tileLocation,this.pixels,this.width)
          if(!tile.contains.isEmpty){
            if(tile.contains.size>1) mergeTile(x*tileXsize,y*tileYsize,tile.contains(tile.contains.size-2).tileLocation,this.pixels,this.width)
            mergeTile(x*tileXsize,y*tileYsize,tile.contains.last.tileLocation,this.pixels,this.width)
          }
         

          
         }
         }
      
      drawWeapon 
      this.updatePixels()  
     
      fill(0)
      rect(tileXsize*viewWidth, 0, infoWidth, 140)
      rect(tileXsize*viewWidth, 175, infoWidth, tileYsize*viewHeight-175)
      rect(tileXsize*viewWidth, 140, 1030-tileXsize*viewWidth, 35)
      rect(1055, 140, tileXsize*viewWidth+infoWidth-1055, 35)
      
      
      fill(200)
      textFont(myfont)
      text(textA, tileXsize*viewWidth +20 , 30, infoWidth-10, viewHeight*tileYsize-100)
      fill(200)
      rect(tileXsize*viewWidth+10, 275, infoWidth-20, tileYsize*viewHeight-280)
      fill(0)
      rect(tileXsize*viewWidth+20, 285, infoWidth-40, tileYsize*viewHeight-300)
      fill(200)
      text(showString, tileXsize*viewWidth +25 , 290, infoWidth-50, viewHeight*tileYsize-300)
      fill(250)
      //rect(1030, 140, 25, 25)
      
      
      
    }
  }
  
  
  
    override def mouseClicked() : Unit = { 
    
    val x=mouseX
    val y=mouseY
    val tileX=x/tileXsize
    val tileY=y*tileYsize
    if(tileX<viewWidth&tileY<viewHeight){
      tileClicked=(tileX,tileY)
    }
     
  }
   override def keyPressed(){
     showString=log.toString()
     message match{
       case this.messageStart => message=null
       case this.messageLose => {
         if(ArrowKeys.forall { x => x!=keyCode }) message=messageCred
       }
       case this.messageWin =>{
         if(ArrowKeys.forall { x => x!=keyCode }) message=messageCred                 
       }
       case this.messageCred => exit()
         
       case default=>
      keyCode match{
        case event.KeyEvent.VK_UP => toMove(player.moveUp)
        case event.KeyEvent.VK_DOWN => toMove(player.moveDown)
        case event.KeyEvent.VK_LEFT => toMove(player.moveLeft)
        case event.KeyEvent.VK_RIGHT => toMove(player.moveRight)
        case event.KeyEvent.VK_H => toHelp
        case event.KeyEvent.VK_I => toInventory
        case event.KeyEvent.VK_D => player.drink
        case event.KeyEvent.VK_M => mute
        case event.KeyEvent.VK_1 => {player.equipItem("1h")
                                    toSwitch("1h")                   //debuggaus
                                    }
        case event.KeyEvent.VK_2 => {player.equipItem("2h")
                                    toSwitch("2h") 
                                    }
        
    
        case default=> 
         }
      
    if(winCondition) win()
    if(loseCondition) end()      
     if(timeMoves){ 
       AITurn()
     }
     }
    }
   def debug{
     timeMoves=false
     log:=Counter.count.toString
   }
   
   
   def AITurn(){   
    def Spawnfilter(m:MapTile):Boolean={  // tämä funktio argumentti palautta tosi jos ruutu on yli 15 askeleen päässä
       if(m.blocks) return false          // pelaajasta ja tyhjä.
       val dist=Util.distance(m,player)
       dist>15
     }
  
     
     // Varmistaa että monstereita on riittävästi, Arpoo creaturesista satunnaisen olion.
     // Spawnable arpoo pelin alussa luoduista ruuduista joihin voi kävellä.
     // CreateMonster yhdistää edellä mainitut siten että luo kopion. 
    
     while(actors.size<=minMonsters){  
            createMonster(creatures.getRandom,map.spawnable.getRandom {Spawnfilter }) 
      } 
      
      actors.sortBy( _.nextAction ) //actors organisoi tekemis järjestyssä missä kaikki olio toimivat pelimaailmassa.
        while(!this.actors(0).playerControl){
          if(actors(0).nextAction<0){
            actors.remove(0)
          }
          else{
           actors(0).doAction()
           if(player.died) {
             draw
             println("playerDied")
           }
                     
          }  
        }
   }
   
   def toMove(command: Boolean) = {
     timeMoves=true
     command
   }
   
   def main(args: Array[String]) {
     import javax.swing._
     val frame = new JFrame("Dungeon Master 3")  
    
     val content=frame.getContentPane
     content.add(this)  
     frame.setSize(1201,800)  //Magic numerot joilla toimii jostain mystisestä syystä muilla processing hajoaa.
     frame.setResizable(false)  // Kaatuu jos kokoa saisi muuttaa.
     frame.setLocationRelativeTo(null)     
     frame.setUndecorated(true)  // tällä korjataan porttaus ongelmia eri ikkuna managerien välillä.
     frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
     init
     Thread.sleep(100)  //tällä korjataan satunnaisia synkronisaatio ongelmia threadien välillä jotta init ehtii tapahtua.
     frame.setVisible(true)
     this.requestFocusInWindow  //Tämä vaaditana jotta Undecorated windowina saadaan näppäinkomennot heti ilman hiirellä valintaa.
   }
  
   
   
   def textA ={ "LEVEL: " + this.player.level + "        XP: " + this.player.exp + " / " + (100+(this.player.level-1)*50) +
                "\nHP:   " + this.player.hp + " / " + this.player.maxhp +
                "\nMANA: " + this.player.mana + " / " + this.player.maxmana +
                "\n\nSTR   DEX   SIZ   INT   CON\n"  + 
                formatToFour(this.player.str) +"  " + formatToFour(this.player.dex) + "  " + formatToFour(this.player.size) + "  " + 
                formatToFour(this.player.int) + "  " + formatToFour(this.player.con) +
                "\n\nCURRENT WEAPON: " +"\n1 SWORD \n2 STAFF(SLOW)"+
                "\n\nI = INVENTORY"+ "\nD = DRINK POTION" + "\nM = MUTE      " + "H = HELP"
   
   }
   
   
   var helptext = {"MOVE TOWARDS ENEMY TO ATTACK" +
               "\n\nUSE STAFF TO SLOW ENEMY DOWN" +
               "\n\nIF LOW ON HEALTH, USE POTION"+
               "\n\nTHE GOAL IS TO SURVIVE TILL LEVEL 10" + 
               "\n\nGOOD LUCK!"}
   
   def formatToFour(A: Int)= {
     var text = A.toString
     while(text.length <4){ text += " "}
     text
   }
   
  def toHelp = {
    this.showString = helptext
    timeMoves = false
  }
  def toInventory = {
    this.showString = if(!player.contains.isEmpty){
      player.contains.mkString("\n")
    }else "YOUR INVENTORY IS FULL OF EMPTY SPACE"
    timeMoves = false
  }
  
  def toSwitch(slot: String) = {
    //this.player.equippedWeapon=this.player.activeWeapons("1h")
    //tähän jokin millä testaa onnistumisen 
    // aseen vaihtamisesta(ja siinä onnistumisesta tueva teksti tähän)
    if((player.equippedWeapon != null) && (player.equippedWeapon==player.activeWeapons(slot))){
      this.showString ={ "Current weapon: " + player.equippedWeapon.name +
            "\nSTR: " + player.equippedWeapon.str + 
            "\nINT: " + player.equippedWeapon.int +
            "\nMANACOST: " + player.equippedWeapon.manaCost
      }    
      
    }else log := "There is no weapon in this slot!"
    timeMoves = false
  }
  
  def drawWeapon = {
    if(player.equippedWeapon != null){
      var loc=player.equippedWeapon.tileLocation
      copyRectangle(loc._1*tileXsize,loc._2*tileYsize,tileXsize,tileYsize,tileset.pixels,tileset.width,1030,145,this.pixels,this.width)
    }else rect(1030, 140, 25, 35)                              
    
  }                                       
  
  
  def mute = {
    timeMoves = false
    
    if (musicOn) {
      musicOn = false
      soundtrack.mute()
    }else{
      musicOn = true
      soundtrack.unmute()
    } 
  }
}