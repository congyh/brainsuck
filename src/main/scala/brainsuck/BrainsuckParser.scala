package brainsuck

import scala.util.parsing.combinator.RegexParsers

class BrainsuckParser extends RegexParsers { // Note: RegexParsers是Scala标准库中的工具类
  def apply(input: String) = // Note: 这里相当于将apply方法直接adapt到了RegexParsers的parseAll方法上
    parseAll(instructions, input) match { // Note: parseAll是RegexParsers中的一个方法
      case Success(compiled, _) => compiled // Note: 如果解析成功了, 返回代码
      case failureOrError       => sys.error(failureOrError.toString)
    }

  def instructions: Parser[Instruction] =
    instruction.* ^^ { // Note: 这里的*, ^^, 包括后面的^^^, |, ~>, <~都是Scala标准库中的Parser定义的运算符重载, 真是太草了
      case seq => seq.foldRight(Halt: Instruction)(_ apply _)
    }

  def instruction: Parser[Instruction => Instruction] =
    (
      "+" ^^^ { Add(1, _) }
        | "-" ^^^ { Add(-1, _) }
        | "<" ^^^ { Move(-1, _) }
        | ">" ^^^ { Move(1, _) }
        | "." ^^^ { Out(_) }
        | "," ^^^ { In(_) }
        | "[" ~> instructions <~ "]" ^^ {
          case body => Loop(body, _)
        }
    )
}

object BrainsuckParser {
  def apply(input: String) = (new BrainsuckParser)(input)
}
