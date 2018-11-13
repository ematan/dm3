package kierros3

/**
 * 
 */

object Color {
  
   def clamp(i : Int) : Int = {
    if (i > 255) 255
    else if (i < 0) 0
    else i
  }
  
   
}

class Color(var a: Int, var r: Int, var g: Int, var b: Int, var argb : Int) {
  
  
  
  
  /**
   * @constructor
   * @param a
   * @param r
   * @param g
   * @param b
   */
  def this(a: Int, r: Int, g: Int, b: Int) {
    this(Color.clamp(a),
         Color.clamp(r),
         Color.clamp(g),
         Color.clamp(b),
         -1)
    this.setInt
  }
  
  /**
   * 
   * @constructor
   * @paaram argb
   */
  def this(argb: Int) {
    this(-1, -1, -1, -1, argb)
    this.setComponents()
  }
  
  
  /**
   * 
   */
  private def setInt() : Unit = {
    this.argb = this.a<<24 | this.r<<16 | this.g<<8 | this.b 
  }
  
  /**
   * 
   */
  private def setComponents() : Unit = {
    this.a = this.argb >>> 24 & 0xFF
    this.r = this.argb >>> 16 & 0xFF
    this.g = this.argb >>> 8  & 0xFF
    this.b = this.argb & 0xFF
  }

  
  
  

}
