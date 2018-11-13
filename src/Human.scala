class Human extends Creature {

  override def tileLocation = (32,1)
  override def spawnProbability = 0
  level = 1
  name = "Player"
  
  override def attack={
    //println("aseen lvl "+ this.equippedWeapon.level)                    //rivi debuggausta varten
    var a = (level+str+size)/10*D4+this.equippedWeapon.level
    a
  }
}