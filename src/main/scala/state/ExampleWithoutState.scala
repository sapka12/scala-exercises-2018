package state

object ExampleWithoutState extends App {
  final case class Robot(
                          id: Long,
                          sentient: Boolean,
                          name: String,
                          model: String)

  val rng = new scala.util.Random(0L)

  def createRobot(): Robot = {
    val id = rng.nextLong()
    val sentient = rng.nextBoolean()
    val isCatherine = rng.nextBoolean()
    val name = if (isCatherine) "Catherine" else "Carlos"
    val isReplicant = rng.nextBoolean()
    val model = if (isReplicant) "replicant" else "borg"
    Robot(id, sentient, name, model)
  }

  println(createRobot())
}
