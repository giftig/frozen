package com.xantoria.frozen.parsing.block

class Package(val name: String) extends Block {
  val children: Seq[Block] = Nil
  val rawResult: Seq[String] = Nil
}

object Package {
  final val Pattern = """^here I stand: ([A-Za-z_][A-Za-z0-9_\.]*)$""".r

  def apply: PartialFunction[UnparsedBlock, Package] = {
    case UnparsedBlock(Pattern(name), tail, _) => new Package(name)
  }
}
