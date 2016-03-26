package com.xantoria.frozen.parsing

import com.xantoria.frozen.struct.Package

class Parser extends ClassParsing {
  def parse(s: String): Package = parseModule(s)
}
