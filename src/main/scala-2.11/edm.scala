import java.io.PushbackInputStream
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by john on 15/12/15.
  */

trait Value

case class Fixnum(v: Integer) extends Value {
  override def toString = v.toString
}
case object True extends Value
case object False extends Value
case object Empty extends Value
case class CharacterLit(v: Char) extends Value
case class StringLit(v: String) extends Value
case class Symbol(v: String) extends Value {
  override def toString = v
}
case class Pair(var first: Value, var second: Value) extends Value {
  override def toString = "(" + first.toString + " , " + second.toString + ")"
}
case class PrimitiveProc(val fun: Value => Value) extends Value
case class CompoundProc(params: Value, body: Value, env: Value) extends Value

class LiteralFactory[T, U](func: T => U) {
  val literals = new mutable.HashMap[T, U]()

  def mkLiteral(v: T) = literals.get(v) match {
    case Some(x) => x
    case None =>
      val newlit = func(v)
      literals.update(v, newlit)
      newlit
  }
}

object CharacterLit extends LiteralFactory[Char, CharacterLit]( new CharacterLit(_) )
object StringLit extends LiteralFactory[String, StringLit] ( new StringLit(_) )
object Symbol extends LiteralFactory[String, Symbol] ( new Symbol(_) )

object VM {
  val delims = "();\"".toSet
  val initials = "*/><=?!".toSet
  val eof = -1

  val QUOTE  = Symbol.mkLiteral("quote")
  val DEFINE = Symbol.mkLiteral("define")
  val SET    = Symbol.mkLiteral("set!")
  val OK     = Symbol.mkLiteral("ok")
  val IF     = Symbol.mkLiteral("if")
  val LAMBDA = Symbol.mkLiteral("lambda")
  val BEGIN  = Symbol.mkLiteral("begin")
  val COND   = Symbol.mkLiteral("cond")
  val ELSE   = Symbol.mkLiteral("else")
  val LET    = Symbol.mkLiteral("let")
  val AND    = Symbol.mkLiteral("and")
  val OR     = Symbol.mkLiteral("or")

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
      Pair(QUOTE, Pair(read(stream), Empty))
    } else {
      throw new RuntimeException(s"bad input. unexpected '${c.toChar}'")
    }
  }

  def isSelfEvaluating(v: Value) = v match {
    case Fixnum(_) | True | False | CharacterLit(_) | StringLit(_) => true
    case _ => false
  }

  def isFixnum(v: Value) = v match {
    case Fixnum(_) => true
    case _ => false
  }

  def isSymbol(v: Value) = v match {
    case Symbol(_) => true
    case _ => false
  }

  def isPair(v: Value) = v match {
    case Pair(_,_) => true
    case _ => false
  }

  def isString(v: Value) = v match {
    case StringLit(_) => true
    case _ => false
  }

  def isCharacter(v: Value) = v match {
    case CharacterLit(_) => true
    case _ => false
  }

  def isBoolean(v: Value) = v == True || v == False

  def isPrimitiveProc(v: Value) = v match {
    case PrimitiveProc(_) => true
    case _ => false
  }

  def isCompoundProc(v: Value) = v match {
    case CompoundProc(_, _, _) => true
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
  def caar(v: Value) = car(car(v))
  def cddr(v: Value) = cdr(cdr(v))
  def cdar(v: Value) = cdr(car(v))
  def caadr(v: Value) = car(cadr(v))
  def caddr(v: Value) = car(cddr(v))
  def cdadr(v: Value) = cdr(cadr(v))

  def isTagged(expression: Value, tag: Value) = expression match {
    case Pair(first, _) => first == tag
    case _ => false
  }

  def isQuoted(expression: Value) = isTagged(expression, QUOTE)

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

  def isAssignment(form: Value) = isTagged(form, SET)
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
    *
    * The definition can also be a compound procedure:
    *
    * {{{
    *   > (define (fib n) ( ... ))
    *   ok
    * }}}
    *
    * The structure of this form is the following:
    *
    *  (define . +
    *            |
    *       ( +  .  + )
    *         |     |
    *     (fib n)  ( ... )
    *
    *
    * @param form the definition form
    * @return the cadr of the definition form
    */
  def isDefinition(form: Value) = isTagged(form, DEFINE)
  def definitionVariable(form: Value) = if (isSymbol(cadr(form))) cadr(form) else caadr(form)
  def definitionValue(v: Value) = {
    if (isSymbol(cadr(v))) caddr(v) else mkLambda(cdadr(v), cddr(v))
  }

  def evalAssignment(form: Value, env: Value): Value = {
    setVariableValue(assignmentVariable(form), eval(assignmentValue(form), env), env)
    OK
  }

  def evalDefinition(form: Value, env: Value): Value = {
    defineVariable(definitionVariable(form), eval(definitionValue(form), env), env)
    OK
  }

  def mkLambda(params: Value, body: Value) = Pair(LAMBDA, Pair(params, body))
  def lambdaParameters(lambda: Value) = cadr(lambda)
  def lambdaBody(lambda: Value) = cddr(lambda)
  def isLambda(form: Value) = isTagged(form, LAMBDA)

  def isLastExpression(seq: Value) = cdr(seq) == Empty
  def firstExpression(seq: Value) = car(seq)
  def restExpressions(seq: Value) = cdr(seq)

  def mkBegin(v: Value) = Pair(BEGIN, v)
  def isBegin(v: Value) = isTagged(v, BEGIN)
  def beginActions(v: Value) = cdr(v)

  def isCond(v: Value) = isTagged(v, COND)
  def isCondElseClause(v: Value) = v == ELSE
  def condClauses(cond: Value) = cdr(cond)
  def condPredicate(clause: Value) = car(clause)
  def condActions(clause: Value) = cdr(clause)

  def sequenceToExp(seq: Value) = {
    if (seq == Empty) {
      seq
    } else if (isLastExpression(seq)) {
      firstExpression(seq)
    } else {
      mkBegin(seq)
    }
  }

  def expandClauses(clauses: Value): Value = {
    if (clauses == Empty) False
    else {
      val first = car(clauses)
      val rest  = cdr(clauses)
      if (isCondElseClause(first)) {
        if (rest == Empty) sequenceToExp(condActions(first)) else throw new RuntimeException("else clause isn't last")
      } else {
        makeIf(condPredicate(first), sequenceToExp(condActions(first)), expandClauses(rest))
      }
    }
  }

  def condToIf(cond: Value) = expandClauses(condClauses(cond))

  def isVariable(v: Value) = isSymbol(v)

  /**
    * An `if` form has the following structure:
    *
    * (if . +
    *       |
    * (pred . +
    *         |
    *   (cons . +
    *           |
    *      (alt . nil)
    *
    * @param form the form given
    * @return true if the form is an `if` form.
    */
  def isIf(form: Value) = isTagged(form, IF)
  def makeIf(predicate: Value, conseq: Value, alter: Value) = Pair(IF, Pair(predicate, Pair(conseq, Pair(alter, Empty))))
  def ifPredicate(form: Value) = cadr(form)
  def ifConsequent(form: Value) = caddr(form)
  def ifAlternate(form: Value) = car(cdr(cddr(form)))

  def isAnd(form: Value) = isTagged(form, AND)
  def andTests(form: Value) = cdr(form)
  def isOr(form: Value) = isTagged(form, OR)
  def orTests(form: Value) = cdr(form)

  def procAdd(arguments: Value) = {
    var result = 0
    var current_args = arguments
    while (current_args != Empty) {
      result += (car(current_args) match { case Fixnum(l) => l })
      current_args = cdr(current_args)
    }
    new Fixnum(result)
  }

  def procSub(arguments: Value) = {
    var current_args = arguments
    var result = car(current_args) match { case Fixnum(n) => n }
    while ({current_args = cdr(current_args); current_args != Empty}) {
      result -= (car(current_args) match { case Fixnum(n) => n })
    }
    new Fixnum(result)
  }

  def procMul(arguments: Value) = {
    var result = 1
    var current_args = arguments
    while (current_args != Empty) {
      result *= (car(current_args) match { case Fixnum(n) => n })
      current_args = cdr(current_args)
    }
    new Fixnum(result)
  }

  def fixnumToInt(v: Value) = v match {
    case Fixnum(n) => n
    case _ => throw new RuntimeException("value not integer")
  }

  def procQuotient(arguments: Value) = {
    val a = fixnumToInt(car(arguments))
    val b = fixnumToInt(cadr(arguments))
    new Fixnum(a / b)
  }

  def procRemainder(arguments: Value) = {
    val a = fixnumToInt(car(arguments))
    val b = fixnumToInt(cadr(arguments))
    new Fixnum(a % b)
  }

  def procAreNumbersEqual(arguments: Value): Value = {
    var current_args = arguments
    val n = fixnumToInt(car(current_args))
    while ({current_args = cdr(current_args); current_args != Empty}) {
      if (n != fixnumToInt(car(current_args))) {
        return False
      }
    }
    return True
  }

  def procIsLessThan(arguments: Value): Value = {
    var current_args = arguments
    var previous = fixnumToInt(car(current_args))
    var next = 0
    while ({current_args = cdr(current_args); current_args != Empty}) {
      next = fixnumToInt(car(current_args))
      if (previous < next) {
        previous = next
      } else {
        return False
      }
    }
    return True
  }

  def procIsGreaterThan(arguments: Value): Value = {
    var current_args = arguments
    var previous = fixnumToInt(car(current_args))
    var next = 0
    while ({current_args = cdr(current_args); current_args != Empty}) {
      next = fixnumToInt(car(current_args))
      if (previous > next) {
        previous = next
      } else {
        return False
      }
    }
    return True
  }

  def procCons(arguments: Value) = Pair(car(arguments), cadr(arguments))
  def procCar(arguments: Value) = caar(arguments)
  def procCdr(arguments: Value) = cdar(arguments)
  def procSetCar(arguments: Value) = {
    val p = car(arguments).asInstanceOf[Pair]
    p.first = cadr(arguments)
    OK
  }
  def procSetCdr(arguments: Value) = {
    val p = car(arguments).asInstanceOf[Pair]
    p.second = cadr(arguments)
    OK
  }

  def procList(arguments: Value) = arguments

  def procIsEq(arguments: Value) = {
    val obj1 = car(arguments)
    val obj2 = cadr(arguments)

    (obj1, obj2) match {
      case (Fixnum(n1), Fixnum(n2)) => if (n1 == n2) True else False
      case (CharacterLit(c1), CharacterLit(c2)) => if (c1 == c2) True else False
      case (StringLit(s1), StringLit(s2)) => if (s1 == s2) True else False
      case _ => if (obj1 == obj2) True else False
    }
  }

  def procIsNull(arguments: Value) = if (car(arguments) == Empty) True else False
  def procIsSymbol(arguments: Value) = if (isSymbol(car(arguments))) True else False
  def procIsInteger(arguments: Value) = if (isFixnum(car(arguments))) True else False
  def procIsBoolean(arguments: Value) = if (isBoolean(car(arguments))) True else False
  def procIsProcedure(arguments: Value) = if (isPrimitiveProc(car(arguments)) || isCompoundProc(car(arguments))) True else False
  def procIsPair(arguments: Value) = if (isPair(car(arguments))) True else False
  def procIsString(arguments: Value) = if (isString(car(arguments))) True else False
  def procIsCharacter(arguments: Value) = if (isCharacter(car(arguments))) True else False

  def procCharToInteger(arguments: Value) = car(arguments) match {
    case CharacterLit(c) => Fixnum(c.toInt)
    case _ => throw new RuntimeException("invalid argument type (expected character)")
  }

  def procIntegerToChar(arguments: Value) = car(arguments) match {
    case Fixnum(n) => CharacterLit.mkLiteral(n.toChar)
    case _ => throw new RuntimeException("invalid argument type (expected integer)")
  }

  def procNumberToString(arguments: Value) = car(arguments) match {
    case Fixnum(n) => StringLit.mkLiteral(n.toString)
    case _ => throw new RuntimeException("invalid argument type (expected integer)")
  }

  def procStringToNumber(arguments: Value) = car(arguments) match {
    case StringLit(s) => Fixnum(s.toInt)
    case _ => throw new RuntimeException("invalid argument type (expected string)")
  }

  def procSymbolToString(arguments: Value) = car(arguments) match {
    case Symbol(s) => StringLit.mkLiteral(s)
    case _ => throw new RuntimeException("invalid argument type (expected symbol)")
  }

  def procStringToSymbol(arguments: Value) = car(arguments) match {
    case StringLit(s) => Symbol.mkLiteral(s)
    case _ => throw new RuntimeException("invalid argument type (expected string)")
  }

  def isProcApplication(form: Value) = isPair(form)
  def procApplicationOperator(form: Value) = car(form)
  def procApplicationOperands(form: Value) = cdr(form)
  def firstOperand(operands: Value) = car(operands)
  def restOperands(operands: Value) = cdr(operands)
  def noOperands(operands: Value) = operands == Empty
  def makeApplication(operator: Value, operands: Value) = Pair(operator, operands)

  def isLet(form: Value) = isTagged(form, LET)
  def letBindings(form: Value) = cadr(form)
  def letBody(form: Value) = cddr(form)

  def bindingParam(binding: Value) = car(binding)
  def bindingArg(binding: Value) = cadr(binding)

  def bindingParameters(bindings: Value): Value = if (bindings == Empty) {
    Empty
  } else {
    Pair(bindingParam(car(bindings)), bindingParameters(cdr(bindings)))
  }

  def bindingArguments(bindings: Value): Value = if (bindings == Empty) {
    Empty
  } else {
    Pair(bindingArg(car(bindings)), bindingArguments(cdr(bindings)))
  }

  def letParameters(v: Value) = bindingParameters(letBindings(v))
  def letArguments(v: Value) = bindingArguments(letBindings(v))

  def letToApplication(v: Value) = makeApplication(mkLambda(letParameters(v),letBody(v)), letArguments(v))

  def listOfValues(operands: Value, env: Value): Value = {
    if (noOperands(operands)) {
      Empty
    } else {
      Pair(eval(firstOperand(operands), env), listOfValues(restOperands(operands), env))
    }
  }

  def createProcedure(name: String, fun: Value => Value): Unit = {
    defineVariable(Symbol.mkLiteral(name), PrimitiveProc(fun), global_env)
  }

  createProcedure("null?", procIsNull)
  createProcedure("boolean?", procIsBoolean)
  createProcedure("symbol?", procIsSymbol)
  createProcedure("integer?", procIsInteger)
  createProcedure("char?", procIsCharacter)
  createProcedure("string?", procIsString)
  createProcedure("pair?", procIsPair)
  createProcedure("procedure?", procIsProcedure)

  createProcedure("char->integer", procCharToInteger)
  createProcedure("integer->char", procIntegerToChar)
  createProcedure("number->string", procNumberToString)
  createProcedure("string->number", procStringToNumber)
  createProcedure("symbol->string", procSymbolToString)
  createProcedure("string->symbol", procStringToSymbol)


  createProcedure("+", procAdd)
  createProcedure("-", procSub)
  createProcedure("*", procMul)
  createProcedure("quotient", procQuotient)
  createProcedure("remainder", procRemainder)
  createProcedure("=", procAreNumbersEqual)
  createProcedure("<", procIsLessThan)
  createProcedure(">", procIsGreaterThan)

  createProcedure("cons", procCons)
  createProcedure("car", procCar)
  createProcedure("cdr", procCdr)
  createProcedure("set-car!", procSetCar)
  createProcedure("set-cdr!", procSetCdr)
  createProcedure("list", procList)
  createProcedure("eq?", procIsEq)


  def eval(v: Value, env: Value): Value = {
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
    } else if (isIf(v)) {
      val branch = if (eval(ifPredicate(v), env) == True) ifConsequent(v) else ifAlternate(v)
      eval(branch, env)
    } else if (isLambda(v)) {
      CompoundProc(lambdaParameters(v), lambdaBody(v), env)
    } else if (isBegin(v)) {
      var actions = beginActions(v)
      while (!isLastExpression(actions)) {
        eval(firstExpression(actions), env)
        actions = restExpressions(actions)
      }
      actions = firstExpression(actions)
      eval(actions, env)
    } else if (isCond(v)) {
      val new_v = condToIf(v)
      eval(new_v, env)
    } else if (isLet(v)) {
      val new_v = letToApplication(v)
      eval(new_v, env)
    } else if (isAnd(v)) {
      var new_v = andTests(v)
      if (new_v == Empty) True
      else {
        while (!isLastExpression(new_v)) {
          val r = eval(firstExpression(new_v), env)
          if (r == False) return r
          new_v = restExpressions(new_v)
        }
        new_v = firstExpression(new_v)
        eval(new_v, env)
      }
    } else if (isOr(v)) {
      var new_v = orTests(v)
      if (new_v == Empty) False
      else {
        while (!isLastExpression(new_v)) {
          val r = eval(firstExpression(new_v), env)
          if (r != False) return r
          new_v = restExpressions(new_v)
        }
        new_v = firstExpression(new_v)
        eval(new_v, env)
      }
    } else if (isProcApplication(v)) {
      val proc = eval(procApplicationOperator(v), env)
      val arguments = listOfValues(procApplicationOperands(v), env)
      proc match {
        case PrimitiveProc(fun) => fun(arguments)
        case CompoundProc(proc_params, proc_body, proc_env) => {
          val new_env = extendEnvironment(proc_params, arguments, proc_env)
          var new_exp = proc_body
          while (!isLastExpression(new_exp)) {
            eval(firstExpression(new_exp), new_env)
            new_exp = restExpressions(new_exp)
          }
          new_exp = firstExpression(new_exp)
          eval(new_exp, new_env)
        }
        case _ => throw new RuntimeException("Invalid procedure object")
      }
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
      } else if (c == '\"') {
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
      case Pair(f, s) =>
        print(" ")
        writePair(f, s)
      case Empty => ; /* do nothing */
      case _ =>
        print(" . ")
        write(second)
    }
  }

  def write(v: Value) = v match {
    case Fixnum(n) => print(n)
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
    case PrimitiveProc(f) => print("#<procedude>")
    case CompoundProc(_,_,_) => print("#<lambda-procedure>")
    case _ => throw new RuntimeException("Cannot write unknown type")
  }

  def repl(): Unit = {
    println("Welcome to EDM v0.17. Use ctrl-c to exit.")
    while (true) {
      print("edm> ")
      write(eval(read(new PushbackInputStream(System.in)), global_env))
      println()
    }
  }
}

object edm extends App {
  VM.repl()
}
