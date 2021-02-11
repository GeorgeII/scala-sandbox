import cats.effect.{ContextShift, IO}
import cats.implicits._

import java.awt.image.BufferedImage
import java.io.File
import java.util.concurrent.Executors
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.concurrent.ExecutionContext

object MostCommonColorFinder {

  type RGBPixel = (Int, Int, Int)

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

        val pixel: RGBPixel = extractRedGreenBlueFromPixel(pixelMixedColors)

        if (pixels.contains(pixel))
          pixels(pixel) += 1
        else
          pixels(pixel) = 1
      }

      // this helpfully allows to track if the program runs in parallel (spoiler: it's not parallel...)
      println(x)
    }

    val mostFrequentPixel = pixels
      .maxBy { case (colors, quantity) => quantity }
      ._1

    mostFrequentPixel
  }

  /**
   * Applies a formula to get red, green, and blue from a pixel of a BufferedImage picture.
   * @return (R, G, B)
   */
  def extractRedGreenBlueFromPixel(color: Int): RGBPixel = {
    val red = (color & 0xff0000) / 65536
    val green = (color & 0xff00) / 256
    val blue = (color & 0xff)

    (red, green, blue)
  }

  /**
   * Function has side-effects as it writes a new file.
   */
  def paintMostFrequentPixels(pixels: Vector[RGBPixel], pathName: String): IO[Unit] = IO {
    val pixelsOfStripePerSample = 3

    val height = 300
    val width = pixelsOfStripePerSample * pixels.length
    val outputImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    // each picture has a 3-pixel stripe
    for (x <- 0 until width by pixelsOfStripePerSample)
      for (y <- 0 until height) {
        val idx = x / pixelsOfStripePerSample
        val (red, green ,blue) = (pixels(idx)._1, pixels(idx)._2, pixels(idx)._3)
        val color = (red * 65536) + (green * 256) + blue

        for (i <- 0 until pixelsOfStripePerSample)
          outputImage.setRGB(x + i, y, color)
      }

    ImageIO.write(outputImage, "jpg", new File(pathName))
  }

  def cpuEval[A](ioa: IO[A])(implicit cs: ContextShift[IO], ec: ExecutionContext): IO[A] = {
    cs.evalOn(ec)(ioa)
  }

  def main(args: Array[String]): Unit = {

    val numberOfAvailableThreads = Runtime.getRuntime.availableProcessors()
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(numberOfAvailableThreads))
    implicit val cs: ContextShift[IO] = IO.contextShift(ec)

    val outputPath = "data/output/MostFrequentColors.jpg"
    val inputPicturesDirectory = new File("data/")
    val pictures = inputPicturesDirectory.listFiles.filter(_.isFile).toVector

    val filesPerThread = pictures.grouped(numberOfAvailableThreads).toVector

    // Vector of IO to IO of Vector
    val ioPixelPerThread = {
      for {
        files <- filesPerThread
      } yield processFilesPerThread(files) //cpuEval(processFilesPerThread(files)) // somehow, cpuEval does not work as expected...
    }.sequence

    // concatenate results of each thread into one Vector. It's done inside of IO.
    val ioPixelForEveryPicture = ioPixelPerThread.map(_.flatten)

    // flatMap is needed to prevent a nested IO[IO[...]]
    val paintPixels = ioPixelForEveryPicture.flatMap(x => paintMostFrequentPixels(x, outputPath))

    paintPixels.unsafeRunSync()
  }

}
