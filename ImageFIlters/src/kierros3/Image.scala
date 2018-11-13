package kierros3

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

class Image(file: String) {
  private val image: BufferedImage = ImageIO.read(new File(file))

  // getters and setters for the width and height of the image
  private val _height: Int = image.getHeight()
  def height: Int = _height
  private val _width: Int = image.getWidth()
  def width: Int = _width

  // we attempt to conserve the original img, so that we can return to it when we return sliders to 0
  
  
  
  // is used to store the modified color data for each pixel
  private var modifiedData: Vector[Vector[Color]] = _

  /**
   * Returns the image data in usable form.
   *
   * @return The image as `BufferedImage`. If image has been modified the modified data is
   *         returned.
   */
  def getImage(): BufferedImage = {
    if (modifiedData != null) {
      var img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          img.setRGB(x, y, modifiedData(x)(y).argb)
        }
      }
      img
    } else {
      image
    }
  }

  /**
   * Sets the given two dimensional vector `pixels` to the variable for modified image data. This method
   * should be called after a filter is operated on image.
   *
   * @param pixels The new modified data for image.
   */
  def setImage(pixels: Vector[Vector[Color]]): Unit = modifiedData = pixels

  /**
   * Returns the value for alpha component for pixel at (`x`, `y`).
   *
   * @param x The x coordinate of the pixel whose color data is needed.
   * @param y The y coordinate of the pixel whose color data is needed.
   * @return  The alpha value for pixel at (`x`, `y`).
   */
  def getAlpha(x: Int, y: Int): Int = getColor(x, y).a

  /**
   * Returns the value for red component for pixel at (`x`, `y`).
   *
   * @param x The x coordinate of the pixel whose color data is needed.
   * @param y The y coordinate of the pixel whose color data is needed.
   * @return  The value of the red component for pixel at (`x`, `y`).
   */
  def getRed(x: Int, y: Int): Int = getColor(x, y).r

  /**
   * Returns the value for green component for pixel at (`x`, `y`).
   *
   * @param x The x coordinate of the pixel whose color data is needed.
   * @param y The y coordinate of the pixel whose color data is needed.
   * @return  The value of the green component for pixel at (`x`, `y`).
   */
  def getGreen(x: Int, y: Int): Int = getColor(x, y).g

  /**
   * Returns the value for blue component for pixel at (`x`, `y`).
   *
   * @param x The x coordinate of the pixel whose color data is needed.
   * @param y The y coordinate of the pixel whose color data is needed.
   * @return  The value of the blue component for pixel at (`x`, `y`).
   */
  def getBlue(x: Int, y: Int): Int = getColor(x, y).b

  /**
   * Returns the value for color component as `scala.Int`; for pixel at (`x`, `y`).
   *
   * @param x The x coordinate of the pixel whose color data is needed.
   * @param y The y coordinate of the pixel whose color data is needed.
   * @return  The color data as `Int`, each component takes 8 bits (value range is 0-255).
   */
  def getColor(x: Int, y: Int): Color = if (modifiedData != null) modifiedData(x)(y) else new Color(image.getRGB(x, y))

}
