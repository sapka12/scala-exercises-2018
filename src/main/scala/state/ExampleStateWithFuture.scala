package state

import cats.data.StateT
import cats.instances.future._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object ExampleStateWithFuture extends App {

  final case class AsyncSeed(long: Long) {
    def next = Future(AsyncSeed(long * 6364136223846793005L + 1442695040888963407L))
  }

  val nextLong: StateT[Future, AsyncSeed, Long] = StateT { seed =>
    seed.next zip Future.successful(seed.long)
  }

  val nextBool: StateT[Future, AsyncSeed, Boolean] = nextLong.map(_ > 0)

  val initialSeed = AsyncSeed(1234)

  val f = for {
    x <- nextLong
    y <- nextBool
    y2 <- nextBool
  } yield (x, y, y2)

  val ff = f.run(initialSeed)

  ff.foreach { x =>
    val (finalState, (a, b, c)) = x
    println(a)
    println(b)
    println(c)
  }

  Thread.sleep(5000)
}
