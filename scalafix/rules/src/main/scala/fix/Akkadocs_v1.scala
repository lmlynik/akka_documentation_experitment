package fix

import scalafix._
import scala.meta._

case class Akkadocs_v1(index: SemanticdbIndex) extends SemanticRule(index, "Akkadocs_v1") {
  override def fix(ctx: RuleCtx): Patch = {
    ctx.debugIndex()
    // Can't `ctx.replaceTree(ctx.tree, "hello!") because https://github.com/scalacenter/scalafix/issues/339
    val yaml = "hello: world"
    ctx.tokens.drop(2).map(ctx.removeToken).asPatch +
    ctx.addRight(ctx.tree, yaml)
  }
}
