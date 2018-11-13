
package kierros3

import scala.swing.{
  Alignment,
  BoxPanel,
  Button,
  GridBagPanel,
  Label,
  MainFrame,
  Orientation,
  SimpleSwingApplication,
  Slider
}
import scala.swing.event.{ ButtonClicked, ValueChanged }
import scala.io.StdIn
import javax.swing.{ ImageIcon }
import java.awt.Dimension
import scala.swing.GridPanel
import scalaj.http._
import java.io.File
import javax.imageio.ImageIO
import java.io.ByteArrayOutputStream
import java.awt.Panel
import scala.swing.TextField

object View extends SimpleSwingApplication {

  def top: MainFrame = new MainFrame() {

    title = "Imageplay"

    contents = new GridBagPanel { // set up the layout of window
      val c = new Constraints
      c.gridx = 0
      c.gridy = 0
      c.gridwidth = 2 // we'll use two columns and want the image to be centered
      c.ipadx = 20 // image has 20 pixels padding on every side
      c.ipady = 20
      layout(imageLabel) = c

      c.ipadx = 10 // for the rest; only 10 pixel padding on left and right side 
      c.ipady = 0
      c.gridwidth = 1 // and they should take one cell in the grid

      c.gridx = 0 // first column contains labels
      for (i <- 0 until labelArray.length) {
        c.gridy = i + 1
        layout(labelArray(i)) = c
      }

      c.gridx = 1 // second column contains sliders
      for (i <- 0 until sliderArray.length) {
        c.gridy = i + 1
        layout(sliderArray(i)) = c
      }
      c.gridy = sliderArray.length + 2 // and the buttons
      for (i <- 0 until buttonArray.length by 1) {
        layout(new BoxPanel(Orientation.Vertical) {
          contents += buttonArray(i)
          if (buttonArray.length > i + 1) {
            contents += buttonArray(i + 1)
          }
        }) = c
        c.gridy += 1
      }
       c.gridy += 1
       linkField.editable = false
       linkField.minimumSize = new Dimension(300,100)
       layout(linkField) = c
    }

    for (s <- sliderArray) { // listen to actions in each slider
      listenTo(s)
    }
    listenTo(inverse, gray, original, brighter, darker, post) // listen to actions also in buttons
    reactions += { // each slider is tied to its respective filter
      case s: ValueChanged =>
        val source: Slider = s.source.asInstanceOf[Slider]
        //        if (source equals lightnessSlider) {
        //          Filter.lightness(source.value/10.0f, image)
        //        } else 
        if (source equals redSlider) {
          Filter.adjustRed(source.value, image)
        } else if (source equals greenSlider) {
          Filter.adjustGreen(source.value, image)
        } else if (source equals blueSlider) {
          Filter.adjustBlue(source.value, image)
        }
        //TODO you can add your own reactions to sliders here

        // because also moving the slider fires an event and blurring and sharpening are relatively slow, we only
        // perform filtering when slider is released
        if (!source.adjusting) {
          if (source equals blurSlider) {
            Filter.blur(source.value, image)
          } else if (source equals sharpenSlider) {
            Filter.sharpen(source.value, image)
          }
          //TODO you can add your own reactions to sliders also here
        }
        icon.setImage(image.getImage)
        imageLabel.icon = icon
        this.repaint
      case b: ButtonClicked =>
        val source: Button = b.source.asInstanceOf[Button]
        if (source equals inverse) {
          Filter.invert(image)
        } else if (source equals gray) {
          Filter.grayscale(image)
        } else if (source equals original) {
          sliderArray.foreach { x => x.value = 1 }
          image = new Image(fileName)
        } else if (source equals brighter) {
          Filter.lightness(1.1f, image)
        } else if (source equals darker) {
          Filter.lightness(0.9f, image)
        } else if (source equals post) {
          linkField.text = poster.postImage(image)
        }
        //TODO you can add your own reactions to buttons here
        icon.setImage(image.getImage)
        imageLabel.icon = icon
        this.repaint
    }
    this.peer.setResizable(false)
    this.peer.setLocationRelativeTo(null)
  }

  // =================================== IMAGE STUFF ===================================
  var image: Image = _
  val fileName: String = StdIn.readLine("Anna polku käsiteltävään kuvatiedostoon:\n")
  try {
    image = new Image(fileName)
  } catch {
    case e: Exception =>
      System.err.println("Tarkista antamasi tiedostopolku, ja että kyseessä on .png-tiedosto.")
  }
  val icon = new ImageIcon(image.getImage)
  val imageLabel = new Label("", icon, Alignment.Center)
  // ================================= IMAGE STUFF END =================================

  // ================================= POSTING STUFFF ==================================

  val CLIENT_ID = "730e13af2fd279d"
  
  val linkField = new TextField("Link to posted images goes here")

  val poster = new ImagePoster()

  // ================================= POSTING STUFF END ===============================

  // ===================================== BUTTONS =====================================
  val inverse = new Button("Inverse")
  val gray = new Button("Grayscale")
  val original = new Button("Original")
  val brighter = new Button("Brighten")
  val darker = new Button("Darken")
  val post = new Button("Post image")

  //TODO add your own button definitions here and remember to include them also in the below array 
  val buttonArray: Array[Button] = Array(inverse, gray, brighter, darker, original, post)

  // =================================== BUTTONS END ===================================

  // ===================================== SLIDERS =====================================
  //  val lightnessSlider = new Slider {
  //    orientation = Orientation.Horizontal
  //    min = 0
  //    max = 50
  //    value = 10
  //    majorTickSpacing = 2
  //    minorTickSpacing = 2
  //    paintTicks = true
  //    labels = Map(0 -> new Label("0"), 10 -> new Label("1,0"), 20 -> new Label("2,0"),
  //                 30 -> new Label("3,0"), 40 -> new Label("4,0"), 50 -> new Label("5,0"))
  //    paintLabels = true
  //  }

  val redSlider = new Slider() {
    orientation = Orientation.Horizontal
    min = -100
    max = 100
    value = 0
    majorTickSpacing = 25
    paintTicks = true
    paintLabels = true
  }

  val greenSlider = new Slider {
    orientation = Orientation.Horizontal
    min = -100
    max = 100
    value = 0
    majorTickSpacing = 25
    paintTicks = true
    paintLabels = true
  }

  val blueSlider = new Slider {
    orientation = Orientation.Horizontal
    min = -100
    max = 100
    value = 0
    majorTickSpacing = 25
    paintTicks = true
    paintLabels = true
  }

  val blurSlider = new Slider {
    orientation = Orientation.Horizontal
    min = 0
    max = 10
    value = 0
    majorTickSpacing = 1
    snapToTicks = true
    paintTicks = true
    paintLabels = true
  }

  val sharpenSlider = new Slider {
    orientation = Orientation.Horizontal
    min = 0
    max = 10
    value = 0
    majorTickSpacing = 1
    snapToTicks = true
    paintTicks = true
    paintLabels = true
  }

  //TODO add your own slider definions here and remember to add them with the according labels in the below arrays

  val sliderArray: Array[Slider] = Array(redSlider, greenSlider, blueSlider,
    blurSlider, sharpenSlider)
  val labelArray: Array[Label] = Array(new Label("Redness"), new Label("Greenness"),
    new Label("Blueness"), new Label("Blur"), new Label("Sharpen"))
  // =================================== SLIDERS END ===================================

}
