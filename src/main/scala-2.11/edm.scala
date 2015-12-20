import java.io.PushbackInputStream
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by john on 15/12/15.
  */

trait Value

case class Fixnum(v: Integer) extends Value
case object True extends Value
case object False extends Value
case object Empty extends Value
case class CharacterLit(v: Char) extends Value
case class StringLit(v: String) extends Value
case class Symbol(v: String) extends Value
case class Pair(var first: Value, var second: Value) extends Value

class LiteralFactory[T, U](func: T => U) {
  val literals = new mutable.HashMap[T, U]()

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
object Symbol extends LiteralFactory[String, Symbol] ( new Symbol(_) )

object VM {
  val delims = "();\"".toSet
  val initials = "*/><=?!".toSet
  val eof = -1

  val quote  = Symbol.mkLiteral("quote")
  val define = Symbol.mkLiteral("define")
  val set    = Symbol.mkLiteral("set!")
  val ok     = Symbol.mkLiteral("ok")

  def isDelimiter(c: Char) = c == eof || Character.isWhitespace(c) || delims.contains(c)

  def isInitial(c: Char) = Character.isAlphabetic(c) || initials.contains(c)

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

  def readPair(stream: PushbackInputStream): Value = {
    eatWhitespace(stream)

    var c = stream.read()
    if (c == ')') { /* read the empty list */
      return Empty
    }
    stream.unread(c)
    val first = read(stream)
    eatWhitespace(stream)
    c = stream.read()
    if (c == '.') { /* read the improper list */
      c = peek(stream)
      if (!isDelimiter(c.toChar)) {
        throw new RuntimeException("dot (.) not followed be delimiter")
      }
      val second = read(stream)
      eatWhitespace(stream)
      c = stream.read()
      if (c != ')') {
        throw new RuntimeException("where was the trailing paren?")
      }
      return Pair(first, second)
    } else { /* read list */
      stream.unread(c)
      val second = readPair(stream)
      return Pair(first, second)
    }
  }

  def read(stream: PushbackInputStream): Value = {
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
        c = stream.read()
        c != -1 && Character.isDigit(c)
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
    } else if (isInitial(c.toChar) || ((c == '+' || c == '-') && isDelimiter(peek(stream).toChar))) {
      val buffer = ArrayBuffer[Char]()
      while (isInitial(c.toChar) || Character.isDigit(c) || c == '+' || c =='-') {
        buffer += c.toChar
        c = stream.read()
      }
      if (isDelimiter(c.toChar)) {
        stream.unread(c)
        Symbol.mkLiteral(buffer.mkString)
      } else {
        throw new RuntimeException(s"symbol not followed by delimiter. found '${c.toChar}'")
      }
    } else if (c == '"') {
      val literal = ArrayBuffer[Char]()
      while ( {
        c = stream.read(); c != '"'
      }) {
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
    } else if (c == '(') {
      readPair(stream) /* read the empty list or pair */
    } else if (c == '\'') {
      Pair(quote, Pair(read(stream), Empty))
    } else {
      throw new RuntimeException(s"bad input. unexpected '${c.toChar}'")
    }
  }

  def isSelfEvaluating(v: Value) = v match {
    case Fixnum(_) | True | False | CharacterLit(_) | StringLit(_) => true
    case _ => false
  }

  def isSymbol(v: Value) = v match {
    case Symbol(_) => true
    case _ => false
  }

  def car(v: Value) = v match {
    case Pair(first, _) => first
    case _ => throw new ClassCastException()
  }
  def cdr(v: Value) = v match {
    case Pair(_, second) => second
    case _ => throw new ClassCastException
  }
  def cadr(v: Value) = car(cdr(v))
  def cddr(v: Value) = cdr(cdr(v))
  def caddr(v: Value) = car(cddr(v))

  def isTagged(expression: Value, tag: Value) = expression match {
    case Pair(first, _) => first == tag
    case _ => false
  }

  def isQuoted(expression: Value) = isTagged(expression, quote)

  /**
    *
    * {{{
    *   > (quote (1 . 2))
    *   (1 . 2)
    * }}}
    * @param expression is a quoted form
    * @return the quoted value
    */
  def quotationText(expression: Value) = cadr(expression)


  val empty_env = Empty
  val global_env = setupEnvironment()

  /**
    * A frame is a Pair of two lists- a list of variables and a list of values
    * @param variables the variable names of the frame
    * @param values the values of the variables
    * @return an environment frame
    */
  def mkFrame(variables: Value, values: Value) = Pair(variables, values)
  def frameVariables(frame: Value) = car(frame)
  def frameValues(frame: Value) = cdr(frame)
  def addBindingToFrame(variable: Value, value: Value, frame: Value): Unit = {
    val p = frame.asInstanceOf[Pair]
    p.first = Pair(variable, p.first)
    p.second= Pair(value, p.second)
  }

  /**
    * Creates a new environment by extending a given environment
    * An environment is a Pair(frame, base_env)
    * @param vars the variables of the new environment
    * @param vals the variable values of the new environment
    * @param base_env the parent environment
    * @return a new environment
    */
  def extendEnvironment(vars: Value, vals: Value, base_env: Value) = Pair(mkFrame(vars, vals), base_env)
  def setupEnvironment() = extendEnvironment(Empty, Empty, empty_env)
  def firstFrame(env: Value) = car(env)
  def enclosingEnvironment(env: Value) = cdr(env)

  def lookupVariableValue(variable: Value, env: Value): Value = {
    var current_env = env
    while (current_env != Empty) {
      val frame = firstFrame(current_env)
      var variables = frameVariables(frame)
      var values = frameValues(frame)
      while (variables != Empty) {
        if (variable == car(variables)) return car(values)
        variables = cdr(variables)
        values = cdr(values)
      }
      current_env = enclosingEnvironment(current_env)
    }
    throw new RuntimeException("unbound variable")
  }

  def defineVariable(variable: Value, value: Value, env: Value): Unit = {
    val frame = firstFrame(env)
    var variables = frameVariables(frame)
    var values = frameValues(frame)

    while (variables != Empty) {
      if (variable == car(variables)) {
        val p = values.asInstanceOf[Pair]
        p.first = value
        return
      }
      variables = cdr(variables)
      values = cdr(values)
    }
    addBindingToFrame(variable, value, frame)
  }

  def setVariableValue(variable: Value, value: Value, env: Value): Unit = {
    var current_env = env
    while (env != Empty) {
      val frame = firstFrame(current_env)
      var variables = frameVariables(frame)
      var values = frameValues(frame)
      while (variables != Empty) {
        if (variable == car(variables)) {
          val p = values.asInstanceOf[Pair]
          p.first = value
          return
        }
        variables = cdr(variables)
        values = cdr(values)
      }
      current_env = enclosingEnvironment(env)
    }
    throw new RuntimeException("unbound variable")
  }

  def isAssignment(form: Value) = isTagged(form, set)
  def assignmentVariable(form: Value) = cadr(form)
  def assignmentValue(form: Value) = caddr(form)
  /**
    * The **define** form has the following structure:
    *
    *  (define . +
    *            |
    *       (var .  +
    *               |
    *          (val . nil)
    * example:
    * {{{
    *   > (define x 5)
    *   ok
    * }}}
    * @param form the definition form
    * @return the cadr of the definition form
    */
  def isDefinition(form: Value) = isTagged(form, define)
  def definitionVariable(form: Value) = cadr(form)
  def definitionValue(form: Value) = caddr(form)

  def evalAssignment(form: Value, env: Value): Value = {
    setVariableValue(assignmentVariable(form), eval(assignmentValue(form), env), env)
    ok
  }

  def evalDefinition(form: Value, env: Value): Value = {
    defineVariable(definitionVariable(form), eval(definitionValue(form), env), env)
    ok
  }

  def isVariable(v: Value) = isSymbol(v)

  def eval(v: Value, env: Value) = {
    if (isSelfEvaluating(v)) {
      v
    } else if (isVariable(v)) {
      lookupVariableValue(v, env)
    } else if (isQuoted(v)) {
      quotationText(v)
    } else if (isAssignment(v)) {
      evalAssignment(v, env)
    } else if (isDefinition(v)) {
      evalDefinition(v, env)
    } else {
      throw new RuntimeException("cannot eval unknown expression type")
    }
  }

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

  def writePair(first: Value, second: Value): Unit = {
    write(first)
    second match {
      case Pair(f, s) => {
        print(" ")
        writePair(f, s)
      }
      case Empty => ; /* do nothing */
      case _ => {
        print(" . ")
        write(second)
      }
    }
  }

  def write(v: Value) = v match {
    case Fixnum(v) => print(v)
    case True => print("#t")
    case False => print("#f")
    case Empty => print("()")
    case CharacterLit(c) => writeCharacter(c)
    case StringLit(s) => writeString(s)
    case Symbol(s) => print(s)
    case Pair(first, second) =>
      print("(")
      writePair(first, second)
      print(")")
    case _ => throw new RuntimeException("Cannot write unknown type")
  }

  def repl(): Unit = {
    println("Welcome to Epicus-Doomicus-Metallicus v0.9. Use ctrl-c to exit.")
    while (true) {
      print("> ")
      write(eval(read(new PushbackInputStream(System.in)), global_env))
      println()
    }
  }
}

object edm extends App {
  VM.repl()
}
