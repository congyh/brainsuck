package brainsuck

import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import scopt.OptionParser

import brainsuck.RulesExecutor.Batch
import brainsuck.RulesExecutor.FixedPoint
import brainsuck.RulesExecutor.Once

class Memory(private val buffer: ArrayBuffer[Int] = ArrayBuffer.fill[Int](1024)(0)) { // Note: Memory是一个一维数组, 用于执行brainfuck语言的指令, 详见: https://en.wikipedia.org/wiki/Brainfuck
  def same(that: Memory) =
    (this eq that) || {
      val commonLength = buffer.length.min(that.buffer.length)
      buffer.take(commonLength) == that.buffer.take(commonLength) &&
      buffer.drop(commonLength).forall(_ == 0) &&
      that.buffer.drop(commonLength).forall(_ == 0)
    }

  private def ensureSpace(atLeast: Int): Unit = // Note: ensureSpace的作用是, 默认当前buffer的长度如果不够用的话, 进行动态拓展 (注意buffer的定义是变长一维数组)
    if (atLeast >= buffer.size) {
      val bytesNeeded = atLeast - buffer.size + 1
      buffer ++= ArrayBuffer.fill[Int](bytesNeeded)(0) // Note: 数组元素初始化为0, 这是brainfuck语言的要求.
    }

  def apply(pointer: Int): Int = {
    ensureSpace(pointer) // Note: 动态扩展buffer数组长度
    buffer(pointer) // Note: 返回buffer数组的指定位置元素值
  }

  def update(pointer: Int, value: Int): Unit = {
    ensureSpace(pointer)
    buffer(pointer) = value
  }

  override def toString = buffer.mkString("|")
}

class Machine(var pointer: Int, val memory: Memory) {
  def same(that: Machine) =
    (this eq that) || {
      pointer == that.pointer && memory.same(that.memory)
    }

  def value = memory(pointer)

  def value_=(n: Int): Unit = memory(pointer) = n

  override def toString = s"$pointer @ $memory"
}

trait Optimizer {
  def batches: Seq[Batch[Instruction]] // Note: TODO: Optimizer就是接收一组batches, 然后转发给RulesExecutor进行执行的?

  def apply(code: Instruction) = RulesExecutor(code, batches)
}

object Interpreter { // Note: 程序入口
  case class Config(optimizationLevel: Int = 2, input: File = null) // Note: optimizationLevel决定了后续用多少优化器, 注意这里input竟然用了null, 在Scala代码中非常少见.

  private def benchmark[T](desc: String)(f: => T) = { // Note: 这里的f是code block作为参数的写法, 这里用到了柯里化, 下面89行和101行都是实例, 可以发现scalatest也是类似的写法
    val start = System.nanoTime()
    val result = f // Note: 这里实际上已经完成了f这个code block的执行
    println(s"$desc: ${(System.nanoTime() - start).toDouble / 1000000}")
    result
  }

  def main(args: Array[String]): Unit = {
    val optionParser = new OptionParser[Config]("brainsuck") { // Note: OptionParser是一个开源依赖, 用于构建命令行工具
      head("brainsuck", "0.1.0")

      opt[Int]('O', "optimize")
        .optional()
        .text("Optimization level.")
        .action { (level, config) => config.copy(optimizationLevel = level) } // Note: 将命令行参数拷贝到返回对象中

      arg[File]("<input file>")
        .maxOccurs(1)
        .required()
        .text("Input file.")
        .action { (input, config) => config.copy(input = input) }
    }

    optionParser.parse(args, Config()).foreach {
      case Config(optimizationLevel, input) =>
        val code = benchmark("Parsing") {
          BrainsuckParser(Source.fromFile(input, "UTF-8").mkString)
        }

        val optimizer = new Optimizer { // Note: 这里直接定义了一个匿名内部类实现
          override def batches =
            Seq(
              Batch("Contraction", MergeAdds :: MergeMoves :: Nil, FixedPoint.Unlimited), // Note: Contraction的意思是收缩, 在这里是合并连续相同步骤的意思.
              Batch("LoopSimplification", Clears :: Scans :: MultisAndCopies :: Nil, Once)
            ).take(optimizationLevel) // Note: take的作用是, 从Seq中取出前n个元素
        }

        val optimized = benchmark("Optimization") {
          if (optimizationLevel > 0) optimizer(code) else code // Note: 这里个人感觉代码不太易读, 虽然通过apply控制了封装, 但是optimizer(code)的可读性个人觉得是不如optimizer.optimize(code)的
        }

        benchmark("Execution") {
          Instruction.untilHalt(optimized, new Machine(0, new Memory())) // Note: 初始化Machine, pointer位置指向一维数组位置0
        }
    }
  }
}