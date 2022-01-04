package cuttingedge

package object progAnalysis {
  def pp(x: Any, nm: String = ""): Unit = {
    println(nm)
    pprint.pprintln(x, 100, 100000)
  }

}
