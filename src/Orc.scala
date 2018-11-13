import scala.math._
import scala.util.Random

class Orc extends Creature {
  var itemRandomizer = new Random
  override def tileLocation = (3,4)
  override def attack = (level+str+size)/10*D4
}


// Kaikilla pelin monstereillä on omat kaavansa niiden ominaispiirteille, jotka on satunnaistettu
// hyödyntämällä ohjelman Dice-luokkaa, eli erisuuruisia noppia. Lisäksi monstereilla on ominaishitaus, 
// vaikeustaso, hyökkäyksen lujuus, sekä mahdollisuus että monsterin inventoryyn luodaan monsteria
// luodessa tietty esine, jonka monsteri pudottaa maahan kuolleessaan.

class Pig extends Orc{
  override def tileLocation = (3,4)
  str  = D6+D6
  dex  = D6+D6
  size = D8+2
  con  = D6+D6
  int  = D6
  slowness = 2000
  hp = size*con/5
  override def attack= (max(1, (level+str+size)/16.0*D4)).toInt
  //println("possu attack "+attack+" hipat "+hp)                          //debuggausta varten
  difficulty = 11
  if(itemRandomizer.nextInt(4) == 1) contains += new Sword
 
}

class Lizard extends Orc{
  override def tileLocation = (36,2)
  str  = D6+D6+D6
  dex  = D8+10
  size = D6+4
  con  = D6+4
  int  = D6+D6+D6
  slowness = (slowness*((30-dex)/10.0)).toInt
  difficulty = 21
  hp=size*con/5+difficulty
  //println("lisko attack "+attack+" hipat "+hp)                          //debuggausta varten
  if(itemRandomizer.nextInt(2) == 1) contains += new Potion
 
}

class Spider extends Orc{
  override def tileLocation = (45,4)
  str  = D6+D6
  dex  = D6+D6+10
  size = D6+6
  con  = D6+D6+D6
  int  = D6+D6+D6
  slowness = (slowness*((30-dex)/10.0)).toInt
  difficulty = 31
  hp=size*con/5+difficulty
  //println("hämis attack "+attack+" hipat "+hp)                          //debuggausta varten
  var staffChance = itemRandomizer.nextInt(4)
  if(staffChance <=1)contains += new Staff else if (staffChance == 2) contains += new GoodStaff
  
}

class Elephant extends Orc{
  override def tileLocation = (39,3)
  str  = D6+D6+D6+10
  dex  = D6+2
  size = D6+D6+15
  con  = D10+D10+12
  int  = D6+D6+D6
  slowness = (slowness*((30-dex)/10.0)).toInt
  difficulty = 41
  hp=size*con/5+difficulty
  //println("norsu attack "+attack+" hipat "+hp)                        //debuggausta varten
  if(itemRandomizer.nextInt(3) == 1)contains += new GoodSword
}

class Dragon extends Orc{
  override def tileLocation = (2,3)
  str  = D6+D6+D6+20
  dex  = D6+12
  size = D6+D6+40
  con  = D6+D6+D6+20
  int  = D6+D6+D6
  slowness = (slowness*((30-dex)/10.0)).toInt
  difficulty = 81
  hp=size*con/5+difficulty
  //println("lohari attack "+attack+" hipat "+hp)                       //debuggausta varten
  if(itemRandomizer.nextInt(3) == 1)contains += new GreatSword
  
}

class Harpy extends Orc{
  override def tileLocation = (63,1)
  str = D6+D6+10
  dex = D4+15
  size = D6+6
  con = D6+4
  int = D6+D6+D6
  slowness=(slowness*((30-dex)/10.0)).toInt
  difficulty = 61
  hp=size*con/5+difficulty
  override def attack= (level+str+dex)/4*D4
  //println("harpi attack "+attack+" hipat "+hp)                         //debuggausta varten
  contains += new Potion
}

class Shadow extends Orc{
  override def tileLocation = (30,5)
  str = D6+D6+D6
  dex = D6+4
  size = D6+D6+D6
  con = D6+D6+D6
  int = D6+14
  slowness=(slowness*((30-dex)/10.0)).toInt
  difficulty = 91
  hp=size*int/5+difficulty*2
  override def attack = ((level*int*2.0)*max(1, D6/3.0)).toInt
  //println("varjo attack "+attack+" hipat "+hp)                           //debuggausta varten
  if(itemRandomizer.nextInt(2) == 1) contains += new GreatStaff
}