// Example from Odersky 25.3
import collection.{IndexedSeqLike, mutable}
import collection.mutable.{ArrayBuffer, Builder}
import collection.generic.CanBuildFrom

abstract class Base
case object A extends Base
case object T extends Base
case object G extends Base
case object U extends Base

object Base {
  val fromInt: Int => Base = Array(A, T, G, U)
  val toInt: Base => Int = Map(A -> 0, T -> 1, G -> 2, U -> 3)
}

final class RNA1 private (val groups: Array[Int], val length: Int)
  extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA1] {

  import RNA1._

  // Mandatory re-implementation of 'newBuilder' in ''IndexedSeq'
  override protected[this] def newBuilder: Builder[Base, RNA1] = RNA1.newBuilder

    def apply(idx: Int) = {
      if (idx < 0 || length <= idx)
        throw new IndexOutOfBoundsException
      Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
    }

  // Optional re-implementation of foreach
  override def foreach[U](f: Base => U): Unit = {
    var i = 0
    var b = 0
    while (i < length) {
      b = if (i % N == 0) groups(i / N) else b >>> S // '>>>': right-shift without sign-extension, i.e. zero-extended
      f(Base.fromInt(b & M))
      i += 1
    }
  }
}

object RNA1 {

  // Number of bits necessary to represent group
  private val S = 2
  // Number of groups that fir in an Int
  private val N = 32 / S
  // Bitmask to isolate a group
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): RNA1 = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- 0 until buf.length)
      groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA1(groups, buf.length)
  }

  def apply(bases: Base*) = fromSeq(bases)

  def newBuilder: Builder[Base, RNA1] = new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[RNA1, Base, RNA1] =
    new CanBuildFrom[RNA1, Base, RNA1] {
      def apply(): Builder[Base, RNA1] = newBuilder
      def apply(from: RNA1): Builder[Base, RNA1] = newBuilder
    }

}


object RNARunner extends App {

  val xs = List(A, G, T, A)

  val rna1 = RNA1.fromSeq(xs)
  println(rna1)
  val rna2 = RNA1(A, U, G, G, T)
  println(rna2.length)
  println(rna2.last)
  println(rna2.take(3))



}




