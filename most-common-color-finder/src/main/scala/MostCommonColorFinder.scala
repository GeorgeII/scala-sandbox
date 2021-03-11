import cats.effect.{ContextShift, IO}
import cats.implicits._

import java.awt.image.BufferedImage
import java.io.File
import java.util.concurrent.{Executors, ForkJoinPool}
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.concurrent.ExecutionContext

object MostCommonColorFinder {

  case class RGBPixel(red: Int, green: Int, blue: Int) {

    def color: Int = {
      red * 0x10000 + green * 0x100 + blue
    }
  }

  object RGBPixel {
    /**
     * Applies a formula to get red, green, and blue from a pixel of a BufferedImage picture.
     * @return (R, G, B)
     */
    def apply(color: Int): RGBPixel = {
      val red = (color & 0xff0000) / 0x10000
      val green = (color & 0xff00) / 0x100
      val blue = (color & 0xff)

      RGBPixel(red, green, blue)
    }
  }

  def processFilesPerThread(files: Vector[File]): IO[Vector[RGBPixel]] = {
    files.map(file => getMostFrequentPixelInImage(file)).sequence
  }

  def getMostFrequentPixelInImage(file: File): IO[RGBPixel] = IO {
    val image = ImageIO.read(file)

    val width = image.getWidth
    val height = image.getHeight

    val pixels: mutable.Map[RGBPixel, Int] = mutable.HashMap()

    for (x <- 0 until width) {
      for (y <- 0 until height) {
        val pixelMixedColors: Int = image.getRGB(x, y)

        val pixel: RGBPixel = RGBPixel(pixelMixedColors)

        if (pixels.contains(pixel))
          pixels(pixel) += 1
        else
          pixels(pixel) = 1
      }

      // this helpfully allows to track if the program runs in parallel (spoiler: it's not parallel...)
      println(x)
      println(Thread.currentThread().getName)
    }

    getMostFrequentInMap(pixels)
  }

  /**
   * scala.collection.Map argument lets us pass immutable Map as well as mutable one.
   */
  def getMostFrequentInMap(pixels: scala.collection.Map[RGBPixel, Int]): RGBPixel = {
    pixels
      .maxBy { case (colors, quantity) => quantity }
      ._1
  }

  /**
   * Function has side-effects as it writes a new file.
   */
  def paintMostFrequentPixels(pixels: Vector[RGBPixel], pathName: String): IO[Unit] = {
    val bufferedImageToPaint = makeBufferedImage(pixels)

    // this is where we have side-effects.
    writeImage(bufferedImageToPaint, pathName)
  }

  def makeBufferedImage(pixels: Vector[RGBPixel]): BufferedImage = {
    val pixelsOfStripePerSample = 3

    val height = 300
    val width = pixelsOfStripePerSample * pixels.length
    val outputImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    // each picture has a 3-pixel stripe
    for (x <- 0 until width by pixelsOfStripePerSample)
      for (y <- 0 until height) {
        val idx = x / pixelsOfStripePerSample
        val color = pixels(idx).color

        for (i <- 0 until pixelsOfStripePerSample)
          outputImage.setRGB(x + i, y, color)
      }

    outputImage
  }

  def writeImage(image: BufferedImage, pathName: String): IO[Unit] = IO {
    ImageIO.write(image, "jpg", new File(pathName))
  }

  def cpuEval[A](ioa: IO[A])(implicit cs: ContextShift[IO], ec: ExecutionContext): IO[A] = {
    cs.evalOn(ec)(ioa)
  }

  def main(args: Array[String]): Unit = {

    val numberOfAvailableThreads = Runtime.getRuntime.availableProcessors()
    // implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(numberOfAvailableThreads))
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(new ForkJoinPool(numberOfAvailableThreads))
    implicit val cs: ContextShift[IO] = IO.contextShift(ec)

    val outputPath = "data/output/MostFrequentColors.jpg"
    val inputPicturesDirectory = new File("data/")
    val pictures = inputPicturesDirectory.listFiles.filter(_.isFile).toVector
    val numberOfFilesPerThread = (1.0 * pictures.length / numberOfAvailableThreads).ceil.toInt

    val filesPerThread = pictures.grouped(numberOfFilesPerThread).toVector

    // Running on a list in parallel.
    val ioPixelPerThread = filesPerThread.parTraverse(processFilesPerThread)

    // concatenate results of each thread into one Vector. It's done inside of IO.
    val ioPixelForEveryPicture = ioPixelPerThread.map(_.flatten)

    // flatMap is needed to prevent a nested IO[IO[...]]
    val paintPixels = ioPixelForEveryPicture.flatMap(x => paintMostFrequentPixels(x, outputPath))

    paintPixels.unsafeRunSync()

    // To track the behavior because some ExecutionContexts do not terminate the program.
    println("Program reached the end of the main block.")
  }

}
