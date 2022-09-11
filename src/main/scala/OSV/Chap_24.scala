package OSV

object Chap_24 {

  def main(args: Array[String]) = {

    val xs = (1 to 10).toList
    val git = xs grouped 3
    val sit = xs sliding 3

    while (git.hasNext) {
      println(git.next())
    }

    while (sit.hasNext) {
      println(sit.next())
    }

  }

}
