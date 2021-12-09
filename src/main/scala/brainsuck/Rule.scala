package brainsuck

import scala.annotation.tailrec

trait Rule[BaseType <: TreeNode[BaseType]] {
  def apply(tree: BaseType): BaseType // Note: 定义Rule为TreeNode->TreeNode的一种变换
}

object RulesExecutor {
  sealed trait Strategy {
    def maxIterations: Int
  }

  case object Once extends Strategy {
    val maxIterations = 1
  }

  final case class FixedPoint(maxIterations: Int) extends Strategy {
    require(maxIterations > 0 || maxIterations == -1)
  }

  case object FixedPoint {
    val Unlimited = FixedPoint(-1)
  }

  case class Batch[BaseType <: TreeNode[BaseType]]( // Note: 这里并不是Batch定义成TreeNode类型, 而是为rule定义的, 这就是一个type parameter.
    name: String,
    rules: Seq[Rule[BaseType]],
    strategy: Strategy
  )

  private def executeBatch[T <: TreeNode[T]](batch: Batch[T], tree: T) = {
    def executeRules(rules: Seq[Rule[T]], tree: T) =
      rules.foldLeft(tree) { case (toTransform, rule) => rule(toTransform) }

    @tailrec def untilFixedPoint(rules: Seq[Rule[T]], tree: T, maxIterations: Int): T = {
      val transformed = executeRules(rules, tree)
      if (transformed.same(tree) || maxIterations == 1) transformed
      else untilFixedPoint(rules, transformed, maxIterations - 1)
    }

    batch.strategy match {
      case Once =>
        executeRules(batch.rules, tree)

      case FixedPoint(maxIterations) =>
        untilFixedPoint(batch.rules, tree, maxIterations)
    }
  }

  def apply[T <: TreeNode[T]](tree: T, batches: Seq[Batch[T]]): T = // Note: 唯一的一个调用: code: Instruction, batches: Seq[Batch[Instruction]].
    batches.foldLeft(tree) { case (toTransform, batch) => executeBatch(batch, toTransform) }
}
