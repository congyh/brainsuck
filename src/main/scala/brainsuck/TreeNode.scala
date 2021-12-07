package brainsuck

trait TreeNode[BaseType <: TreeNode[BaseType]] { // Note: 这里就是递归类型的定义
  self: BaseType =>

  def children: Seq[BaseType] // Note: Scala中默认是public方法, 下面的protected打头的是为了定义模板方法的, 子类实现父类调用.

  def same(that: BaseType) = (this eq that) || this == that // Note: TODO: eq这个方法没有定义呀

  protected def sameChildren(otherChildren: Seq[BaseType]) =
    children.size == otherChildren.size && children.lazyZip(otherChildren).forall(_ same _) // Note: 这个lazyZip也是绝了, Scala为了提升性能真是无所不作.

  protected def withChildren(otherChildren: Seq[BaseType]): BaseType =
    if (this sameChildren otherChildren) this else makeCopy(otherChildren)

  protected def makeCopy(args: Seq[BaseType]): BaseType

  def transform(rule: PartialFunction[BaseType, BaseType]): BaseType = transformDown(rule) // Note: PartialFunction TreeNode->TreeNode, 默认是transformDown

  def transformDown(rule: PartialFunction[BaseType, BaseType]): BaseType = {
    val selfTransformed = rule.applyOrElse(this, identity[BaseType])
    if (this same selfTransformed) this transformChildrenDown rule // Note: 这里和下面一行的意思是, 如果本层没被转换, 那么直接放弃此次结果就可以了
    else selfTransformed transformChildrenDown rule
  }

  private def transformChildrenDown(rule: PartialFunction[BaseType, BaseType]): BaseType =
    this.withChildren(children.map(_ transformDown rule)) // Note: 在这里完成了递归操作

  def transformUp(rule: PartialFunction[BaseType, BaseType]): BaseType = { // Note: transformDown和transformUp的区别就是, 一个是前序变换 一个是后序变换.
    val childrenTransformed = transformChildrenUp(rule)
    if (this same childrenTransformed) rule.applyOrElse(this, identity[BaseType])
    else rule.applyOrElse(childrenTransformed, identity[BaseType])
  }

  private def transformChildrenUp(rule: PartialFunction[BaseType, BaseType]): BaseType =
    this.withChildren(children.map(_ transformUp rule))
}

trait LeafNode[BaseType <: TreeNode[BaseType]] extends TreeNode[BaseType] {
  self: BaseType =>

  override def children = Seq.empty[BaseType] // Note: LeafNode对children和makeCopy有了清晰的定义, 那就是没有children, Copy返回的是本身

  override def makeCopy(args: Seq[BaseType]) = this
}

trait UnaryNode[BaseType <: TreeNode[BaseType]] extends TreeNode[BaseType] {
  self: BaseType =>

  def child: BaseType // Note: UnaryNode只有一个child

  override def children = Seq(child) // Note: 由于UnaryNode只有一个child, 所以children也很容易推导出来该如何定义了.
}
