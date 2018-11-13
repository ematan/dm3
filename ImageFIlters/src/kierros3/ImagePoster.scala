package kierros3

import scalaj.http._
import spray.json._
import javax.imageio.ImageIO
import java.io.ByteArrayOutputStream
import java.io.File
import java.awt.image.BufferedImage

class ImagePoster() {
  
  val client_id = "your client_id here"

  def postImage(img: Image): String = ???
}