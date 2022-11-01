/* --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- *
 *                            EPL ASSIGNMENT 3 - VERSION 1.1                               *
 * --------------------------------------------------------------------------------------- *
 * --------------------------------------------------------------------------------------- */
 
package Assignment3.AnimatedGif
import com.sksamuel.scrimage._
import com.sksamuel.scrimage.nio.StreamingGifWriter
import com.sksamuel.scrimage.nio.StreamingGifWriter._
import java.awt.Color
import java.io.IOException
import java.awt.geom._
import java.awt.BasicStroke
import javax.imageio.ImageIO
import java.awt.image._
import java.io.File
import java.time.Duration
import Assignment3.FRP._
import Assignment3.FRP.Signal.{Time, envtime}

  ////////////////////////////////////////////////////////////////////
  // ************************************************************** //
  // *                   DO NOT CHANGE THIS CODE                  * //
  // ************************************************************** //
  ////////////////////////////////////////////////////////////////////


// ----------------------------------------------------------------
// Graphics Canvas

trait GraphicsCanvasTrait {
  // draw a picture given by `filename`
  def draw( filename: String, x: Integer, y: Integer
          , scaleFactor: Double, angle: Double): Unit
  // Saves the current image as an image to filename.
  def saveToFile(filename: String): Unit
}

class GraphicsCanvas(val width: Integer, val height: Integer) extends GraphicsCanvasTrait {
  val ANIMAL_NAMES = Set("bee", "cat", "elephant", "rabbit", "turtle")

  // Firstly, set up the canvas
  val bufferedImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
  val g2d = bufferedImg.createGraphics()
  // Default colour is black on white
  g2d.setColor(Color.WHITE);
  g2d.fillRect(0, 0, width, height);
  g2d.setColor(Color.BLACK)

  // Method implementations

  // Algorithm adapted from http://stackoverflow.com/questions/4787066/how-to-resize-and-rotate-an-image
  // (yes, TAs use StackOverflow too...)
  def rotateImage(image: BufferedImage, angle: Double): BufferedImage = {
      val angleSin = Math.abs(math.sin(angle))
      val angleCos = Math.abs(math.cos(angle))
      val w = image.getWidth()
      val h = image.getHeight()
      val newWidth = Math.floor(w * angleCos + h * angleSin).toInt
      val newHeight = Math.floor(h * angleCos + w * angleSin).toInt
      val resultImg = new BufferedImage(newWidth, newHeight, BufferedImage.TYPE_INT_ARGB)
      val g2d = resultImg.createGraphics()
      g2d.translate((newWidth - w)/2, (newHeight - h)/2)
      g2d.rotate(angle, w/2, h/2)
      g2d.drawRenderedImage(image, null)
      g2d.dispose()
      resultImg
  }

  def draw( name: String, x: Integer, y: Integer
          , scaleFactor: Double, angle: Double): Unit = 
    try {
      val animalImg = ImageIO.read(new File("images/" + name + ".png"))
      // Scale and rotate image
      val w = animalImg.getWidth()
      val h = animalImg.getHeight()
      val newW = w * scaleFactor
      val newH = h * scaleFactor
      if (newW.toInt <= 0 || newH.toInt <= 0) {()}
      else {
        val scaleImg = new BufferedImage(newW.toInt, newH.toInt, BufferedImage.TYPE_INT_ARGB);
        // Scale:
        val at = AffineTransform.getScaleInstance(scaleFactor, scaleFactor)
        val scaleGraphics = scaleImg.createGraphics()
        scaleGraphics.drawRenderedImage(animalImg, at)
        // Rotate:
        // val angleRads = math.toRadians(angle.toDouble)
        val angleRads = angle
        val rotateImg = rotateImage(scaleImg, angleRads)
        // Finally, translate to the correct position and draw to the canvas
        val absoluteX = x - (rotateImg.getWidth() / 2)
        val absoluteY = y - (rotateImg.getHeight() / 2)
        val translateTransform = new AffineTransform()
        translateTransform.translate(absoluteX, absoluteY)
        g2d.drawRenderedImage(rotateImg, translateTransform)
      }
    } catch {
      case (ioe: IOException) => print("Exception occurred while reading image: %s".format(ioe.toString))
    }

  def saveToFile(filename: String):Unit = {
    try {
      val outputfile = new File(filename)
      ImageIO.write(bufferedImg, "png", outputfile)
      ()
    } catch {
      case (ioe: IOException) => print("Exception occurred while saving image: %s".format(ioe.toString))
    }
  }

}

// ----------------------------------------------------------------
// Animated GIF

object AnimatedGif {

  // Type for pictures
  // abstract class Picture
  // A picture is given by: filename, x position, y position, scale factor, angle
  // default: position (0, 0) with scaleFactor = 0.5 and no rotation
  case class Picture( var name: String, var x: Integer = 0, var y: Integer = 0
                , var scaleFactor: Double = 1, var angle: Double = 0)


  // Some global configuration of animation
  val width     = 1000 // [-500, 500]
  val height    = 1000 // [-500, 500]
  val timeBegin = 0
  val timeStep  = 1
  val unit = 0.1
  val GIFinterval = Duration.ofMillis((unit*1000).toInt) // 0.1s

  // Types for frame (multiple pictures)
  type Frame = List[Picture]

  // Generate animated GIF of duration `timeEnd` from animation `frames` and save to `filename`
  def generateGif(frames: Signal[Frame], timeEnd: Time, filename: String): Unit = {
    val writer = new StreamingGifWriter(GIFinterval, true)
    val gif = writer.prepareStream(filename, BufferedImage.TYPE_INT_ARGB)
    addFrames(gif, frames, timeBegin, timeEnd, timeStep)
    gif.close()
  }

  // Add frames to a GIF stream
  def addFrames(gif: GifStream, frames: Signal[Frame], begin: Time, end: Time, step: Time)
  : Unit = {
    if (begin > end) { () }
    else {
      envtime() = begin
      val canvas = new GraphicsCanvas(width, height)
      draw(frames(), canvas)
      gif.writeFrame(ImmutableImage.fromAwt(canvas.bufferedImg))
      addFrames(gif, frames, begin + step, end, step)
    }
  }

  // Draw a frame
  def draw(imgs: Frame, canvas: GraphicsCanvas): Unit = imgs match {
    case Nil => ()
    case (x::xs) => {
      drawAnimal(x, canvas)
      draw(xs, canvas)
    }
  }

  // Draw a picture
  def drawAnimal(pic: Picture, canvas: GraphicsCanvas): Unit = {
    val shiftx = width / 2
    val shifty = height / 2
    pic match {
      case Picture(name, x, y, scaleFactor, angle) =>
        canvas.draw(name, x + shiftx, height-(y + shifty), scaleFactor, angle)
        // some magic coordinates transformation -- you don't need to care about this
    }
  }
}
