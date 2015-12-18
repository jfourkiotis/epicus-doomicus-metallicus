import java.io.PushbackInputStream
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

/**
  * Created by john on 15/12/15.
  */

trait Value

case class Fixnum(v: Integer) extends Value
case object True extends Value
case object False extends Value
case class CharacterLit(v: Char) extends Value
case class StringLit(v: String) extends Value

class LiteralFactory[T, U](func: T => U) {
  val literals = new HashMap[T, U]()

  def mkLiteral(v: T) = literals.get(v) match {
    case Some(x) => x
    case None => {
      val newlit = func(v)
      literals.update(v, newlit)
      newlit
    }
  }
}

object CharacterLit extends LiteralFactory[Char, CharacterLit]( new CharacterLit(_) )
object StringLit extends LiteralFactory[String, StringLit] ( new StringLit(_) )

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

  def eatExpectedString(stream: PushbackInputStream, str: String): Unit = {
    for (c <- str) {
      if (c != stream.read()) {
        throw new RuntimeException(s"unexpected character '$c'")
      }
    }
  }

  def peekExpectedDelimiter(stream: PushbackInputStream): Unit = {
    if (!isDelimiter(peek(stream).toChar)) {
      throw new RuntimeException("character not followed by delimiter")
    }
  }

  def readCharacter(stream: PushbackInputStream): CharacterLit = {
    val c = stream.read()
    if (c == -1) {
      throw new RuntimeException("incomplete character literal")
    } else if (c == 's') {
      if (peek(stream) == 'p') {
        eatExpectedString(stream, "pace")
        peekExpectedDelimiter(stream)
        return CharacterLit.mkLiteral(' ')
      }
    } else if (c == 'n') {
      if (peek(stream) == 'e') {
        eatExpectedString(stream, "ewline")
        peekExpectedDelimiter(stream)
        return CharacterLit.mkLiteral('\n')
      }
    }
    peekExpectedDelimiter(stream)
    return CharacterLit.mkLiteral(c.toChar)
  }

  def read(stream: PushbackInputStream) = {
    eatWhitespace(stream)

    var c = stream.read()
    var sign = 1
    var num = 0
    if (c == '#') {
      c = stream.read()
      if (c == 't') {
        True
      } else if (c == 'f') {
        False
      } else if (c == '\\') {
        readCharacter(stream)
      } else {
        throw new RuntimeException(s"unknown boolean or character literal '${c.toChar}'")
      }
    } else if (Character.isDigit(c) || (c == '-' && Character.isDigit(peek(stream)))) {
      if (c == '-') {
        sign = -1
      } else {
        stream.unread(c)
      }
      while ( {
        c = stream.read(); c != -1 && Character.isDigit(c)
      }) {
        num = num * 10 + c - '0'
      }
      num *= sign
      if (isDelimiter(c.toChar)) {
        stream.unread(c)
        new Fixnum(num)
      } else {
        throw new RuntimeException("number not followed by delimiter")
      }
    } else if (c == '"') {
      var literal = ArrayBuffer[Char]()
      while ({c = stream.read(); c != '"'}) {
        if (c == '\\') {
          c = stream.read()
          if (c == 'n') {
            c = '\n'
          }
        }
        if (c == -1) {
          throw new RuntimeException("non-terminated string literal")
        }
        literal.append(c.toChar)
      }
      StringLit.mkLiteral(literal.mkString)
    } else {
      throw new RuntimeException(s"bad input. unexpected '$c'")
    }
  }

  def eval(v: Value) = v

  def writeCharacter(c: Char): Unit = {
    print("#\\")
    if (c == '\n') {
      print("newline")
    } else if (c == ' ') {
      print("space")
    } else {
      print(c)
    }
  }

  def writeString(s: String): Unit = {
    print("\"")
    for (c <- s) {
      if (c == '\n') {
        print("\\n")
      } else if (c == '\\') {
        print("\\\\")
      } else if (c == "\"") {
        print("\\\"")
      } else {
        print(c)
      }
    }
    print("\"")
  }

  def write(v: Value) = v match {
    case Fixnum(v) => print(v)
    case True => print("#t")
    case False => print("#f")
    case CharacterLit(c) => writeCharacter(c)
    case StringLit(s) => writeString(s)
    case _ => throw new RuntimeException("Cannot write unknown type")
  }

  def repl(): Unit = {
    println("Welcome to Epicus-Doomicus-Metallicus v0.4. Use ctlr-c to exit.")
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
