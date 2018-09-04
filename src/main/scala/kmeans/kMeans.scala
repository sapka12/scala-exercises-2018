package kmeans

object kMeans {

  type Point = Vector[Double]

  def mean(points: List[Point]): Point = points.transpose.map(line => line.sum / line.length).toVector

  def getDistance(mean: Point, x: Point): Double = Math.sqrt(x.zip(mean)
    .map{case (v1, v2) => (v1 - v2) * (v1 - v2)}
    .sum)

  def getMean(x: Point, means: List[Point]): Point = {
    means.map(mean =>
      (mean, getDistance(mean, x))
    ).minBy(_._2)._1
  }

  def calculate(clusters: List[(Point, List[Point])], limit: Double, iteration: Int = 0): List[(Point, List[Point])] = {
    val newMeans: List[Point] = clusters.map(cluster => cluster._2).map(mean)
    val newClusters = clusters.flatMap(x => x._2).groupBy(x => getMean(x, newMeans)).toList
    println(s"Iteration: $iteration")
    if (iteration > 5) newClusters else calculate(newClusters, limit, iteration+1)
  }
}
