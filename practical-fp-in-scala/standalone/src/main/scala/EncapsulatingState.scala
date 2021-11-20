import cats.Functor
import cats.effect.{ IO, Ref }

object EncapsulatingState {

  trait Counter[F[_]] {
    def incr: F[Unit]
    def get: F[Int]
  }

  object Counter {
    def make[F[_]: Functor: Ref.Make]: F[Counter[F]] =
      Ref.of[F, Int](0).map { ref =>
        new Counter[F] {
          def incr: F[Unit] = ref.update(_ + 1)

          def get: F[Int] = ref.get
        }
      }
  }

  def main(args: Array[String]): Unit = {

    def program(c: Counter[IO]): IO[Unit] =
      for {
        _ <- c.get.flatMap(IO.println)
        _ <- c.incr
        _ <- c.get.flatMap(IO.println)
        _ <- c.incr.replicateA(5).void
        _ <- c.get.flatMap(IO.println)
      } yield ()

    val counterIO = Counter.make[IO].flatMap(counter => program(counter))
    counterIO.unsafeRunSync()
  }

}
