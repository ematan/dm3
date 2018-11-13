
package kierros3

import scala.math._
import scalaj.http._

object Filter {
 
  
  def lightness(factor: Float, img: Image) : Unit = {                                                //lightens the picture by multiplyin every color with the given factor
    val adjustedColors: Vector[Vector[Color]] = Vector.tabulate(img.width, img.height)((x, y) =>  
      new Color(img.getAlpha(x, y), 
                (img.getRed(x, y)   * factor).toInt , 
                (img.getGreen(x, y) * factor).toInt ,
                (img.getBlue(x, y)  * factor).toInt ))
      img.setImage(adjustedColors)
  }

  def invert(img: Image) : Unit = {                                                                  //inverts each color and thus the whole pic
    val adjustedColors: Vector[Vector[Color]] = Vector.tabulate(img.width, img.height)((x, y) =>  
      new Color(img.getAlpha(x, y), 
                255 - img.getRed(x, y), 
                255 - img.getGreen(x, y),
                255 - img.getBlue(x, y)))
      img.setImage(adjustedColors)
  }

  def grayscale(img: Image) : Unit = {                                                               //turns the picture into grayscale
    def mean(x: Int, y: Int) = (img.getRed(x, y) + img.getGreen(x, y) + img.getBlue(x, y)) / 3          //creates a method to calculate the mean
    val adjustedColors: Vector[Vector[Color]] = Vector.tabulate(img.width, img.height)((x, y) =>  
      new Color(img.getAlpha(x, y),                                                                     //and applies it to every color in every pixel
                mean(x, y), 
                mean(x, y),
                mean(x, y)))
      img.setImage(adjustedColors)
  }

  def adjustRed(amount: Int, img: Image) : Unit = {                                                  //makes the redness of the picture adjustable
    val orig = new Image(View.fileName)                                                              //saves the original color
    val adjustedColors: Vector[Vector[Color]] = Vector.tabulate(img.width, img.height)((x, y) =>  
      new Color(img.getAlpha(x, y), 
                orig.getRed(x, y) + amount,                                                          //Adds the wanted amount into the original color.
                img.getGreen(x, y),                                                                    //As the original color is taken from source, 
                img.getBlue(x, y)))                                                                    //this method doesn't add separate slider moves together
      img.setImage(adjustedColors)                                                                     //and thus the picture isn't all muddled up if the slider 
  }                                                                                                    //is returned to its original position

  def adjustGreen(amount: Int, img: Image) : Unit = {                                                //same as adjustRed, but fro green this time
    val orig = new Image(View.fileName)
    val adjustedColors: Vector[Vector[Color]] = Vector.tabulate(img.width, img.height)((x, y) =>  
      new Color(img.getAlpha(x, y), 
                img.getRed(x, y), 
                orig.getGreen(x, y) + amount,
                img.getBlue(x, y)))
      img.setImage(adjustedColors)
  }

  def adjustBlue(amount: Int, img: Image) : Unit = {                                                 //same as adjustRed, but for blue this time
    val orig = new Image(View.fileName)
    val adjustedColors: Vector[Vector[Color]] = Vector.tabulate(img.width, img.height)((x, y) =>  
      new Color(img.getAlpha(x, y), 
                img.getRed(x, y), 
                img.getGreen(x, y),
                orig.getBlue(x, y) + amount))
      img.setImage(adjustedColors)
  }

  private def whenSumIs1(amount: Int): Float = {                                                     //private method to calculate the seed for blur
      if (amount % 2 == 0){
        (1.0 /(pow((amount/2 +1), 2) + pow(amount/2, 2))).toFloat
      }else{
        (1.0 /(pow(amount, 2))).toFloat
      }
  }

  def blur(amount: Int, image: Image) : Unit = {                                                     //creates a blur effect
    val filter = getFilter(amount, whenSumIs1(amount))                                               //creates a filter for the effect
    val blurredPixels: Array[Array[Color]] = Array.tabulate(image.width, image.height)((x, y) =>     //creates an 2 dimensional array with pixels from the source 
      new Color(image.getAlpha(x, y), 
                image.getRed(x, y), 
                image.getGreen(x, y),
                image.getBlue(x, y)))
    for(row <- amount/2 until image.width-amount/2 ; column <- amount/2 until image.height-amount/2){//loops all pixels that the filter can affect without it going out of bounds
      blurredPixels(row)(column) = multiplyWithFilter(row, column, image, filter)                      //replaces each pixel with the filter effect applied to it
    }
    image.setImage(blurredPixels.toVector.map(_.toVector))    
  }
  
  private def pixelInMid(amount:Int) = {                                                             //private method to calculate the coordinates for the pixel in the middle of the filter                    
    if(amount % 2 == 0){
      (amount+2)/2 -1
    }else{
      (amount+1)/2 -1
    }
  }
  
  def sharpen(amount: Int, image: Image) : Unit = {                                                  //creates a sharpen effect
    val filter = getFilter(amount, -1.toFloat)                                                       //creates filter with seed -1.0
    val midPixelValue: Float = (filter.flatten.count(_ != 0))                                        //calculates that middle pixel needs
    filter(pixelInMid(amount))(pixelInMid(amount)) = midPixelValue                                   //replaces the middle pixel with the correct value
    val sharpenedPixels: Array[Array[Color]] = Array.tabulate(image.width, image.height)((x, y) =>
      new Color(image.getAlpha(x, y), 
                image.getRed(x, y), 
                image.getGreen(x, y),
                image.getBlue(x, y)))
    for(row <- amount/2 until image.width-amount/2 ; column <- amount/2 until image.height-amount/2){
      sharpenedPixels(row)(column) = multiplyWithFilter(row, column, image, filter)
    }
    image.setImage(sharpenedPixels.toVector.map(_.toVector))
  }
  
  private def getFilter(amount: Int, seed: Float) : Array[Array[Float]] = {                          //creates a base for filter
    if (amount % 2 == 0){                                                                            //if the parameter for amount is an even number
      val full = Array.fill(amount+1, amount+1)(seed)                                                  //first creates an array of the wanted size
      for (y <- 0 to amount){                                                                          
        var zeroes = abs( (amount/2) - y)                                                              //then for each row of every column replaces the seed with 0
        for(x <- 0 to zeroes-1){                                                                       //accrording to the given guidelines
          full(x)(y) = 0
        }
        for(x <- (amount + 1 -zeroes) to amount){
          full(x)(y) = 0
        }
      }
      full
    }else{                                                                                           //if amount is uneven       
      Array.fill(amount, amount)(seed)
    }
  }
  
  private def multiplyWithFilter(x: Int, y: Int, image: Image, filter: Array[Array[Float]]) : Color = {
    val size = filter.size
    val x1 = size/2
    var valueR = 0                                                                            //variables for each color with the starting amount zeri
    var valueG = 0
    var valueB = 0
    for(row <- 0 to size-1 ; column <- 0 to size-1){                                          //for every pixel
      valueR += (image.getRed(  row + x-x1,  column + y-x1)  *   filter(column)(row)).toInt   //calculate the new value by multiplying with the given filter
      valueG += (image.getGreen(row + x-x1,  column + y-x1)  *   filter(column)(row)).toInt
      valueB += (image.getBlue( row + x-x1,  column + y-x1)  *   filter(column)(row)).toInt
    }
    new Color(image.getAlpha(x,y), valueR, valueG, valueB)                                    //and return a new color 
  }

  //cf22e30fabd2c00
  def postImage(img: Image): String = {
    val baseRequest: HttpRequest = Http("https://api.imgur.com/3/image")
    val postRequest: HttpRequest = baseRequest.postForm.param("param1", "a")
    
    ""
  }


}
