import java.io.PushbackInputStream

import scala.annotation.tailrec

/**
  * Created by john on 15/12/15.
  */

trait Value

case class Fixnum(v: Integer) extends Value

object VM {
  val delims = "();\"".toSet
  val eof = -1

  def isDelimiter(c: Char) = c == eof || Character.isWhitespace(c) || delims.contains(c)

  def eatWhitespace(stream: PushbackInputStream): Unit = {
    var c = 0
    while ({c = stream.read(); c != -1}) {
      if (Character.isWhitespace(c)) return eatWhitespace(stream)
      else if (c == ';') {
        while ({c = stream.read(); c != -1 && c != '\n'}) {/* just ignore line */}
        return eatWhitespace(stream)
      } else {
        stream.unread(c)
        return
      }
    }
  }

  def peek(stream: PushbackInputStream) = {
    val c = stream.read()
    stream.unread(c)
    c
  }

  def read(stream: PushbackInputStream) = {
    eatWhitespace(stream)

    var c = stream.read()
    var sign = 1
    var num = 0
    if (Character.isDigit(c) || (c == '-' && Character.isDigit(peek(stream)))) {
      if (c == '-') {
        sign = -1
      } else {
        stream.unread(c)
      }
      while ({c = stream.read(); c != -1 && Character.isDigit(c)}) {
        num = num * 10 + c - '0'
      }
      num *= sign
      if (isDelimiter(c.toChar)) {
        stream.unread(c)
        new Fixnum(num)
      } else {
        throw new RuntimeException("number not followed by delimiter")
      }
    } else {
      throw new RuntimeException(s"bad input. unexpected '$c'")
    }
  }

  def eval(v: Value) = v

  def write(v: Value) = v match {
    case Fixnum(v) => print(v)
    case _ => throw new RuntimeException("Cannot write unknown type")
  }

  def repl(): Unit = {
    println("Welcome to Epicus-Doomicus-Metallicus v0.1. Use ctlr-c to exit.")
    while (true) {
      print("> ")
      write(eval(read(new PushbackInputStream(System.in))))
      println()
    }
  }
}

object edm extends App {
  VM.repl()
}
