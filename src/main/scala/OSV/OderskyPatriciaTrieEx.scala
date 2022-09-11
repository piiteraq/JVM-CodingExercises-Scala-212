package OSV

import scala.annotation.tailrec
import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Map
import scala.collection.mutable.{Builder, MapBuilder}

class PrefixMap[T]
  extends mutable.Map[String, T]
    with mutable.MapLike[String, T, PrefixMap[T]] {

  var suffixes: immutable.Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get (s(0)) flatMap (_.get(s substring 1))

  @tailrec
  private def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None => // Suffix starting with s(0) doesn't exist, so create it ..
          suffixes = suffixes + (leading -> empty) // 'empty' creates a new empty Prefix tree ..
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  override def update(s: String, elem: T) = withPrefix(s).value = Some(elem)

  override def remove(s: String): Option[T] =
    if (s.isEmpty) {
      val prev = value; value = None; prev
    }
    else suffixes get (s(0)) flatMap (_.remove(s substring 1))

  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (chr +: s, v))

  def +=(kv: (String, T)): this.type = { // this.type ~ OSV.PrefixMap[OSV.T]
    update(kv._1, kv._2); this
  }

  def -=(s: String): this.type = {
    remove(s); this
  }

  // Creates an empty tree, so that value can be set ..
  override def empty = new PrefixMap[T]
}


object PrefixMap extends {

  def empty[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val m: PrefixMap[T] = empty
    for (kv <- kvs) m += kv
    m
  }

  def newBuilder[T]: Builder[(String, T), PrefixMap[T]] =
    new MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def StringCanBuildFrom[T]:
    CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
      new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
        def apply(from: PrefixMap[_]) = newBuilder[T]
        def apply() = newBuilder[T]
      }
}


object PrefixMapRun extends App {

  val pm = PrefixMap[Int]()

  pm.update("", 1)
  pm.update("abe", 2)
  pm += (("abelian", 4))

  for ((k,v) <- pm) { println(s"k: $k, v: $v") }

  val pm1 = PrefixMap("hello" -> 5, "hi" -> 2)
  val pm2 = PrefixMap.empty[String]

  println(pm1 map { case (k,v) => (k + "!", "x" * v)})
  // OSV.PrefixMap[String] = Map(hello! -> xxxxx, hi! -> xx)
}