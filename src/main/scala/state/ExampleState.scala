package state

import cats.data.State

object ExampleState {

  final case class Seed(long: Long) {
    def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
  }

  val nextLong: State[Seed, Long] = State(seed => (seed.next, seed.long))
  val nextBool: State[Seed, Boolean] = nextLong.map(_ > 0)

  def main(args: Array[String]): Unit = {
    val initialSeed = Seed(1234)

    val f = for {
      x <- nextLong
      y <- nextBool
      y2 <- nextBool
    } yield (x, y, y2)

    val (finalState, (a, b, c)) = f.run(initialSeed).value

    println(a)
    println(b)
    println(c)



  }
}
