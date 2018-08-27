package kmeans

import java.io.File
import java.util.Date
import com.sksamuel.scrimage.{Color, Image}
import scala.util.Random

object ImageHandler {

  def withTime[R](func: => R): R = {
    val begin = new Date().getTime

    // call-by-name
    val result = func

    val end = new Date().getTime

    println(s"${end - begin} ms")
    result
  }

  def main(args: Array[String]): Unit = withTime{

    val inputFilePath = args(0)
    val clusterSize = args(1).toInt
    val diffLimit = args(2).toDouble

    println("args:")
    args.foreach(println)

    val image = Image.fromFile(new File(inputFilePath))

    val pixels = for {
      x <- 0 until image.width
      y <- 0 until image.height
    } yield {
      image.pixel(x, y)
    }

    def toPoint(pixel: Color): kMeans.Point = Vector(pixel.red, pixel.green, pixel.blue)

    def toColor(p: kMeans.Point): Color = Color(p(0).toInt, p(1).toInt, p(2).toInt)

    val initMeans = (0 to clusterSize).toList.map { _ =>
      val mean: kMeans.Point = Vector(
        Random.nextInt(256),
        Random.nextInt(256),
        Random.nextInt(256)
      )
      mean
    }

    val initCluster = pixels
      .map(p => toPoint(p.toColor))
      .map { p =>
        (initMeans(Random.nextInt(initMeans.size)), p)
      }.groupBy(_._1).mapValues(_.toList.map(_._2)).toList

    val cluster = kMeans.calculate(initCluster, diffLimit)

    println("creating colormap")
    val colorMap = cluster.flatMap {
      case (meanPoint, ps) =>
        ps.map(point =>
          (toColor(point), toColor(meanPoint))
        )
    }.toMap

    def findSimilarColor(c: Color): Color = colorMap(c)

    println("changing colors")

    for {
      x <- 0 until image.width
      y <- 0 until image.height
    } yield image.setPixel(x, y,
      findSimilarColor(image.pixel(x, y).toColor).toPixel
    )

    image.output(inputFilePath + ".png")
  }

}
