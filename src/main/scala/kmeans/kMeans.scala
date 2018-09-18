package kmeans

object kMeans {

  type Point = Vector[Double]

  def mean(points: List[Point]): Point = points.transpose.map(line => line.sum / line.length).toVector

  def getDistance(mean: Point, x: Point): Double = Math.sqrt(x.zip(mean)
    .map { case (v1, v2) => (v1 - v2) * (v1 - v2) }
    .sum)

  def getMean(x: Point, means: List[Point]): Point = {
    means.map(mean =>
      (mean, getDistance(mean, x))
    ).minBy(_._2)._1
  }


  def stopCondition(clusters: List[(Point, List[Point])], limit: Double, iteration: Int): Boolean = {
    val error: Double = {
      val distances = clusters.flatMap {
        case (meanPoint, pointsInCluster) => pointsInCluster.map(getDistance(meanPoint, _))
      }
      //      val distances = clusters.flatMap(cluster => cluster._2.map(getDistance(cluster._1, _)))
      distances.sum / distances.size
    }
    println(s"Iteration: $iteration Error: $error Limit: $limit")
    iteration > 20 || error < limit
  }


  def calculate(clusters: List[(Point, List[Point])], limit: Double, iteration: Int = 0): List[(Point, List[Point])] = {
    val newMeans: List[Point] = clusters.map(_._2).map(mean)
    val allPoints = clusters.flatMap(_._2)
    val newClusters = allPoints.groupBy(getMean(_, newMeans)).toList

    if (stopCondition(newClusters, limit, iteration)) newClusters else calculate(newClusters, limit, iteration + 1)
  }
}
