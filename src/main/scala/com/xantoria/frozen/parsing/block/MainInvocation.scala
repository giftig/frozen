package com.xantoria.frozen.parsing.block

class MainInvocation(className: String, methodName: String) extends Block {
  val children: Seq[Block] = Nil
  val rawResult: Seq[String] = Seq(
    "if __name__ == '__main__':",
    "  import sys",
    s"  $className().$methodName(sys.argv[1:])"
  )
}
