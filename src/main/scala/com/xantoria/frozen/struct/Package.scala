package com.xantoria.frozen.struct

import java.io.{File, FileOutputStream, PrintWriter}

class Package(val name: String, val content: Seq[String]) {
  def dest: File = new File(name.replace('.', '/') + ".py")

  def mkdirs(f: File): Unit = {
    if (!f.isDirectory) {
      Option(f.getParent) foreach { s: String => mkdirs(new File(s)) }
      f.mkdir
      new File(f, "__init__.py").createNewFile()
    }
  }

  def write(): Unit = {
    val f = dest
    Option(f.getParent) foreach { s: String => mkdirs(new File(s)) }

    val out = new PrintWriter(new FileOutputStream(f))
    content foreach { out.println(_) }
    out.close()
  }
}
