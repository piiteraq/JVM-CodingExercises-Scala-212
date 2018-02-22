import collection._
import scala.annotation.tailrec

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

  def +=(kv: (String, T)): this.type = { // this.type ~ PrefixMap[T]
    update(kv._1, kv._2); this
  }

  def -=(s: String): this.type = {
    remove(s); this
  }

  // Creates an empty tree, so that value can be set ..
  override def empty = new PrefixMap[T]
}

object PrefixMap {
  def apply[T]() = new PrefixMap[T].empty
}

object PrefixMapRun extends App {

  val pm = PrefixMap[Int]()

  pm.update("", 1)
  pm.update("abe", 2)
  pm += (("abelian", 4))

  for ((k,v) <- pm) { println(s"k: $k, v: $v") }
}