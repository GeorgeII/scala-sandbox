package redBook.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

class Par[A] {

}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = ???

  def asyncF[A,B](f: A => B): A => Par[B] = {
    a: A => lazyUnit(f(a))
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((el, acc) => map2(el, acc)((x, y) => x :: y))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // my version is not parallel at all...
//  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
//    unit(as.filter(f))

  // from the answer
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  // a very naive approach. But we haven't defined flatMap yet. It's hard to write an impl without it.
  def countWords(l: List[String]): Int = {
    val parOfLengthsList = parMap(l)(str => str.split(" ").length)
    val parOfSumLength = map(parOfLengthsList)(par => sum(par.toIndexedSeq))
    run(run(parOfSumLength))
  }

  // again... flatMap is needed so much...
//  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
//    map2(unit((a, b)), c) { (tup, cc) =>
//      map2(tup._1, tup._2) { (aa, bb) =>
//        f(aa, bb, cc)
//      }
//    }
//
////      map(tup._1)(aa =>
////        map(tup._2)(bb =>
////          map(tup._3)(cc =>
////            f(aa, bb, cc)))))
//  }


  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
}

object FunctionalParallelism {

  def main(args: Array[String]): Unit = {
    import Par._

    val paragraphs = List("1 2", "here are 3", "these are 4 words", "5 words 5 5 5")
    val res = countWords(paragraphs)
    println(res)
  }
}
