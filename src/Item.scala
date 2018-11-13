import scala.collection.mutable.ArrayBuffer

//Item luokka periytyy suoraan GameObjectista ja haarautuu edelleen erilaisiin itemeihin, 
//Weaponit vaikuttavat pelaajan hyökätessa, Potionit sen sijaan ovat esineitä, jooita pelaaja voi komennolla käyttää. 


class Item extends GameObject {
  isItem = true
  itemClass = "An Item"
  var reduction = 0                                  // Nämä kolme riviä eivät tällä hetkellä vaikuta pelissä, sillä
  var quality = 1000                                 // aseille ei nyt ole erikseen asetettu toisistaan poikkeavia ominaisuuksia 
  def reduction(recieved:Damage):Int = 0             // olemme kuitenkin jättäneet ne ennalleen tulevaisuuden varalle
}

class Weapon extends Item {
  isWeapon = true
  str   = 0
  int   = 0
  level = 1
}


class Sword extends Weapon{ 
 override def tileLocation = (11,29)
 itemClass = "1h"
 name  = "Regular Sword"
 str   = D8
 level = str+int/2
}

class GoodSword extends Sword{
  override def tileLocation = (20,28)
  name  = "Good Sword"
  str   = D8+8
  level = str+int/2
}

class GreatSword extends Sword{
  override def tileLocation = (21,28)
  name  = "Great Sword"
  str   = D8+16
  level = str+int/2
}

//class FireSword extends Sword{                                      //esimerkki erilaisen damage-tyypin aseesta
//  itemClass="2h Weapon"
// this.attackType=Fire  
//}

class Staff extends Weapon{                                           //Staff-luokan aseiden erikoisominaisuus on, että ne tuplaavat monsterin                   
  itemClass = "2h"                                                    //hitauden loppupelin ajaksi, siis monsteri liikkuu hitaammin.
  //this.attackType = Fire                                              
  override def tileLocation = (0,27)                                  
  name  = "Regular Staff"
  int   = D8
  level = str+int/2
  manaCost = 30                                                       //Ominaisuuden vastapainoksi kukin ase kuluttaa tietyn määrän manaa.
}

class GoodStaff extends Staff{
  override def tileLocation = (1,27)
  name  = "Good Staff"
  int   = D8 +8
  level = str+int/2
  manaCost = 40
}

class GreatStaff extends Staff{
  override def tileLocation = (23,29)
  name  = "Great Staff"
  int   = D8 +16
  level = str+int/2
  manaCost = 50
}


class Dummy extends Weapon{                                           //Valease, joka pelaajalla alussa on player.activeWeaponsin kahdessa slotissa
  override def tileLocation = (0,0)
  isDummy = true
  level = 0
  str   = 0
  int   = 0
  name  = "you have no weapon in this slot"
}

class Armour extends Item{                                            //Tämä luokka jäi meiltä lopussa hyödyntämättä, mutta päätimme
  var durability = 20                                                 //jättää sen näkyville havainnoillistaaksemme alkuperäisiä suunnitelmia,
                                                                      //sekä mahdollistaaksemme lisäkehityksen projektin palautuksen jälkeenkin    
  override def reduction(recieved:Damage)={
    var red= recieved.tyyppi match{                                   //Ajatuksena on tuoda peliin Item-tyyppi, jolla pelaaja saa lisäpisteitä
    //case  Fire  => armour/(10+recieved.duration)                    //erilaisiin ominaisuuksiin.
      case default => armour
    }
    
    if(Dice(durability)+0 == durability) this.quality-=1  
      red*quality/1000
  }
}
  
class Potion extends Item{                                            //Taikajuoma, jolla pelaaja voi palauttaa osan HP:staan
  override def tileLocation = (2,25) 
  isPotion  = true
  itemClass = "potion"
  hp = 100
}
  
  

