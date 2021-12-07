package brainsuck

sealed trait Instruction extends TreeNode[Instruction] { // Note: 定义Instruction为TreeNode类型
  def next: Instruction // Note: 指令是一个序列, 所以这里相当于定义了一个链表结构
  def run(machine: Machine): Unit // Note: run的作用是根据具体的指令, 对Machine的pointer和Memory进行操作. 例如Add, 是在memory的指定位置+=n.
}

sealed trait LeafInstruction extends Instruction with LeafNode[Instruction] {
  self: Instruction =>
  def next: Instruction = this // Note: TODO: next为什么不应该是null?
}

sealed trait UnaryInstruction extends Instruction with UnaryNode[Instruction] {
  self: Instruction =>
  def next: Instruction = child
}

case object Halt extends LeafInstruction {
  override def run(machine: Machine) = () // Note: TODO: 看下Halt是用来干啥的?
}

case class Add(n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head) // Note: TODO: 理解下这个函数
  override def run(machine: Machine) = machine.value += n // Note: Add指令 (相当于合并了brainfuck语言的'+', '-'指令, 并进行了强化), 就是将pointer所在的位置的memory value变更n
}

case class Move(n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.pointer += n // Note: Move指令 (相当于合并了brainfuck语言的'>', '<'指令), 就是将pointer指向的memory位置移动n
}

case class Scan(n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = {
    while (machine.value != 0) {
      machine.pointer += n
    }
  }
}

case class Out(child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = print(machine.value.toChar) // Note: Out指令 (相当于brainfuck语言的'.'指令), 输出memory此处的value值, optimizer中也不会用到这个指令. 这里toChar是int to char, 用于屏幕输出.
}

case class In(child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.value = System.in.read() // Note: In指令 (相当于brainfuck语言的','指令), 接收一字符的数据输入, 在hanoi.b程序中没有用到这个指令, optimizer中也不会用到.
}

case class Loop(body: Instruction, next: Instruction) extends Instruction { // Note: 注意Loop和前面的指令不同, 除了next instruction, 还有一个body instruction.
  override def children = Seq(body, next)
  override protected def makeCopy(args: Seq[Instruction]) = copy(body = args.head, next = args.last)
  override def run(machine: Machine) = {
    while (machine.value != 0) {
      Instruction.untilHalt(body, machine)
    }
  }
}

case class Clear(child: Instruction) extends UnaryInstruction { // Note: Clear还有下面的Copy和Multi都是二阶优化 (LoopSimplification) 中对循环优化所产生的拓展指令.
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.value = 0 // Note: 这个看起来很好理解, 就是当连续的增减能够抵消的话, 直接将pointer位置赋值为0即可.
}

case class Copy(offset: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.memory(machine.pointer + offset) += machine.value
}

case class Multi(offset: Int, n: Int, child: Instruction) extends UnaryInstruction {
  override protected def makeCopy(args: Seq[Instruction]) = copy(child = args.head)
  override def run(machine: Machine) = machine.memory(machine.pointer + offset) += machine.value * n
}

object Instruction {
  def untilHalt(code: Instruction, machine: Machine): Unit = { // Note: Instruction是递归数据结构.
    var next = code
    while (next ne Halt) { // Note: TODO: ne方法没有定义呀
      next.run(machine)
      next = next.next
    }
  }
}
