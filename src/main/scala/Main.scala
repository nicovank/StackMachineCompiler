import java.io.PrintWriter

import scala.io.Source

case class Program(statements: Array[Statement])

abstract class Statement
case class If(condition: Expression, first: Statement, second: Option[Statement]) extends Statement
case class While(condition: Expression, statement: Statement) extends Statement
case class Block(statements: Array[Statement]) extends Statement
case class Assign(register: Int, expression: Expression) extends Statement
case class Output(expression: Expression) extends Statement

abstract class Expression
case class LessThan(first: Expression, second: Expression) extends Expression
case class GreaterThan(first: Expression, second: Expression) extends Expression
case class Equal(first: Expression, second: Expression) extends Expression
case class NotEqual(first: Expression, second: Expression) extends Expression
case class Add(first: Expression, second: Expression) extends Expression
case class Subtract(first: Expression, second: Expression) extends Expression
case class Multiply(first: Expression, second: Expression) extends Expression
case class Divide(first: Expression, second: Expression) extends Expression
case class Mod(first: Expression, second: Expression) extends Expression
case class BitwiseAnd(first: Expression, second: Expression) extends Expression
case class BitwiseOr(first: Expression, second: Expression) extends Expression
case class Parenthesis(expression: Expression) extends Expression
case class Register(reference: Int) extends Expression
case class Literal(value: Int) extends Expression
case class Input() extends Expression

object Token extends Enumeration {
  type Token = Value
  val IF, ELSE, WHILE, OUT, IN, REGISTER, +, -, *, /, %, ASSIGN, ==, !=, <, >, &, |, LITERAL, LP, RP, LB, RB, EOF = Value
}

class Lexer(input: String) {
  private var _index: Int = 0
  private var _token: Token.Token = _
  private var _value: Int = _

  def value: Int = _value

  next()

  private def isHexadecimalCharacter(c: Char): Boolean = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F')

  def next(): Unit = {
    while (_index < input.length && input.charAt(_index).isWhitespace) _index += 1
    if (_index >= input.length) _token = Token.EOF
    if (_token == Token.EOF) return

    _token = input.charAt(_index) match {
      case '+' => Token.+
      case '-' => Token.-
      case '*' => Token.*
      case '/' => Token./
      case '%' => Token.%
      case '&' => Token.&
      case '|' => Token.|
      case '<' => Token.<
      case '>' => Token.>
      case '(' => Token.LP
      case ')' => Token.RP
      case '{' => Token.LB
      case '}' => Token.RB
      case '=' =>
        if (_index < input.length - 1 && input.charAt(_index + 1) == '=') {
          _index += 1
          Token.==
        } else Token.ASSIGN
      case '!' =>
        if (_index < input.length - 1 && input.charAt(_index + 1) == '=') {
          _index += 1
          Token.!=
        } else throw new RuntimeException("Unexpected character: '! '.")
      case 'I' =>
        if (_index < input.length - 1 && input.charAt(_index + 1) == 'F') {
          _index += 1
          Token.IF
        } else if (_index < input.length - 1 && input.charAt(_index + 1) == 'N') {
          _index += 1
          Token.IN
        } else throw new RuntimeException("Unexpected character: 'I'.")
      case 'W' =>
        if (_index < input.length - 4 && input.substring(_index, _index + 5) == "WHILE") {
          _index += 4
          Token.WHILE
        } else throw new RuntimeException("Unexpected character: 'W'.")
      case 'E' =>
        if (_index < input.length - 3 && input.substring(_index, _index + 4) == "ELSE") {
          _index += 3
          Token.ELSE
        } else throw new RuntimeException("Unexpected character: 'E'.")
      case 'O' =>
        if (_index < input.length - 2 && input.substring(_index, _index + 3) == "OUT") {
          _index += 2
          Token.OUT
        } else throw new RuntimeException("Unexpected character: 'O'.")
      case '$' =>
        if (_index < input.length - 2 && isHexadecimalCharacter(input.charAt(_index + 1)) && isHexadecimalCharacter(input.charAt(_index + 2))) {
          _value = Integer.parseInt(input.substring(_index + 1, _index + 3), 16)
          _index += 2
          Token.REGISTER
        } else throw new RuntimeException("Unexpected character: '$'.")
      case _ =>
        if (!input.charAt(_index).isDigit) throw new RuntimeException("Unexpected character: '" + input.charAt(_index) + "'.")
        val builder: StringBuilder = new StringBuilder()
        builder.append(input.charAt(_index))
        while (_index < input.length - 1 && input.charAt(_index + 1).isDigit) {
          _index += 1
          builder.append(input.charAt(_index))
        }
        _value = builder.result().toInt
        Token.LITERAL
    }

    _index += 1
  }

  def matches(token: Token.Token): Boolean = this._token == token
  def consume(token: Token.Token): Boolean = if (matches(token)) {
    next()
    true
  } else false
}

class Parser(input: String) {
  private val _lexer: Lexer = new Lexer(input)

  def parseProgram(): Program = {
    var statements: Array[Statement] = Array()
    while (!_lexer.matches(Token.EOF)) statements = statements appended parseStatement()
    Program(statements)
  }

  def parseStatement(): Statement = {
    if (_lexer.consume(Token.IF)) {
      if (!_lexer.consume(Token.LP)) throw new RuntimeException("Expected '('.")
      val condition: Expression = parseExpression()
      if (!_lexer.consume(Token.RP)) throw new RuntimeException("Expected ')'.")
      val first: Statement = parseStatement()
      if (_lexer.consume(Token.ELSE)) If(condition, first, Some(parseStatement())) else If(condition, first, None)
    } else if (_lexer.consume(Token.WHILE)) {
      if (!_lexer.consume(Token.LP)) throw new RuntimeException("Expected '('.")
      val condition: Expression = parseExpression()
      if (!_lexer.consume(Token.RP)) throw new RuntimeException("Expected ')'.")
      While(condition, parseStatement())
    } else if (_lexer.consume(Token.LB)) {
      var statements: Array[Statement] = Array()
      while (!_lexer.consume(Token.RB)) statements = statements appended parseStatement()
      Block(statements)
    } else if (_lexer.matches(Token.REGISTER)) {
      val register: Int = _lexer.value
      _lexer.next()
      if (!_lexer.consume(Token.ASSIGN)) throw new RuntimeException("Expected '='.")
      Assign(register, parseExpression())
    } else if (_lexer.consume(Token.OUT)) {
      if (!_lexer.consume(Token.LP)) throw new RuntimeException("Expected '('.")
      val expression: Expression = parseExpression()
      if (!_lexer.consume(Token.RP)) throw new RuntimeException("Expected ')'.")
      Output(expression)
    } else throw new RuntimeException("Unexpected token.")
  }

  def parseExpression(): Expression = {
    val factor: Expression = parseFactor()
    if (_lexer.consume(Token.==)) Equal(factor, parseExpression())
    else if (_lexer.consume(Token.!=)) NotEqual(factor, parseExpression())
    else if (_lexer.consume(Token.<)) LessThan(factor, parseExpression())
    else if (_lexer.consume(Token.>)) GreaterThan(factor, parseExpression())
    else factor
  }

  def parseFactor(): Expression = {
    val term: Expression = parseTerm()
    if (_lexer.consume(Token.+)) Add(term, parseFactor())
    else if (_lexer.consume(Token.-)) Subtract(term, parseFactor())
    else term
  }

  def parseTerm(): Expression = {
    val atom: Expression = parseAtom()
    if (_lexer.consume(Token.*)) Multiply(atom, parseTerm())
    else if (_lexer.consume(Token./)) Divide(atom, parseTerm())
    else if (_lexer.consume(Token.%)) Mod(atom, parseTerm())
    else atom
  }

  def parseAtom(): Expression = {
    val quark: Expression = parseQuark()
    if (_lexer.consume(Token.&)) BitwiseAnd(quark, parseAtom())
    else if (_lexer.consume(Token.|)) BitwiseOr(quark, parseAtom())
    else quark
  }

  def parseQuark(): Expression = {
    if (_lexer.consume(Token.LP)) {
      val expression: Expression = parseExpression()
      if (!_lexer.consume(Token.RP)) throw new RuntimeException("Expected ')'.")
      expression
    } else if (_lexer.matches(Token.REGISTER)) {
      val register: Register = Register(_lexer.value)
      _lexer.next()
      register
    } else if (_lexer.matches(Token.LITERAL)) {
      val value: Literal = Literal(_lexer.value)
      _lexer.next()
      value
    } else if (_lexer.consume(Token.IN)) Input()
    else throw new RuntimeException("Expected '(', '$', 'IN', or an integer literal.")
  }
}

class Box[A](private var _element: A) {
  def element: A = _element
  def element_=(element: A): Unit = this._element = element
}

class OutputWriter(writer: PrintWriter) {
  private var _program: Array[Box[String]] = Array()

  def index: Int = _program.length + 1

  def println(line: String): Unit = {
    println(new Box(line))
  }

  def println(line: Box[String]): Unit = {
    _program = _program appended line
  }

  def flush(): Unit = {
    writer.println(_program.length)
    writer.println()
    _program.foreach(line => writer.println(line.element))
    writer.flush()
  }
}

object CodeGenerator {
  def generateProgram(program: Program, output: PrintWriter): Unit = {
    val writer: OutputWriter = new OutputWriter(output)
    generateProgram(program, writer)
    writer.flush()
  }

  private def generateProgram(program: Program, output: OutputWriter): Unit = {
    program.statements.foreach(statement => generateStatement(statement, output))
  }

  private def generateStatement(statement: Statement, output: OutputWriter): Unit = statement match {
    case If(condition, first, second) =>
      generateExpression(condition, output)
      output.println("LIT 0")
      val conditional: Box[String] = new Box("IFEQ")
      output.println(conditional)
      generateStatement(first, output)
      val jump: Box[String] = new Box("JUMP")
      output.println(jump)
      conditional.element = "IFEQ " + output.index
      if (second.isDefined) generateStatement(second.get, output)
      jump.element = "JUMP " + output.index
    case While(condition, statement) =>
      val index: Int = output.index
      generateExpression(condition, output)
      output.println("LIT 0")
      val conditional: Box[String] = new Box("IFEQ")
      output.println(conditional)
      generateStatement(statement, output)
      output.println("JUMP " + index)
      conditional.element = "IFEQ " + output.index
    case Block(statements) =>
      statements.foreach(statement => generateStatement(statement, output))
    case Assign(register, expression) =>
      generateExpression(expression, output)
      output.println("STOR " + register)
    case Output(expression) =>
      generateExpression(expression, output)
      output.println("OUT")
  }

  private def generateExpression(expression: Expression, output: OutputWriter): Unit = expression match {
    case LessThan(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("IFLT " + (output.index + 3))
      output.println("LIT 0")
      output.println("JUMP " + (output.index + 2))
      output.println("LIT 1")
    case GreaterThan(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("DUP 2")
      output.println("IFLT " + (output.index + 4))
      output.println("IFEQ " + (output.index + 3))
      output.println("LIT 1")
      output.println("JUMP " + (output.index + 2))
      output.println("LIT 0")
    case Equal(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("IFEQ " + (output.index + 3))
      output.println("LIT 0")
      output.println("JUMP " + (output.index + 2))
      output.println("LIT 1")
    case NotEqual(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("IFEQ " + (output.index + 3))
      output.println("LIT 1")
      output.println("JUMP " + (output.index + 2))
      output.println("LIT 0")
    case Add(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("ADD")
    case Multiply(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("MUL")
    case Subtract(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("SUB")
    case Divide(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("DIV")
    case Mod(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("MOD")
    case BitwiseAnd(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("AND")
    case BitwiseOr(first, second) =>
      generateExpression(second, output)
      generateExpression(first, output)
      output.println("OR")
    case Parenthesis(expression) => generateExpression(expression, output)
    case Register(reference) => output.println("LOAD " + reference)
    case Literal(value) => output.println("LIT " + value)
    case Input() => output.println("IN")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val source: Source = Source.fromFile("test.smp")
    val parser: Parser = new Parser(source.mkString)
    source.close()

    val program: Program = parser.parseProgram()

    CodeGenerator.generateProgram(program, new PrintWriter(System.out))
  }
}