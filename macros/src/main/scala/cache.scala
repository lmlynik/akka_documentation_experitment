package cache


import scala.meta._

class openapi extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {

    def printTerm(t: Term.Arg, idx: Int): Unit = println(Seq.fill(idx)("-").mkString + s"$t :::: ${t.getClass}")

    def printType(t: Type.Arg, idx: Int): Unit = println(Seq.fill(idx)("-").mkString + s"Type $t :::: ${t.getClass}")

    def navigateTypes(t: Type.Arg, idx: Int, parentSyntax: => Option[Class[_]]): Unit = t match {
      case n: Type.Name =>
        printType(n, idx)
      case ts: Type.Select =>
        navigateTypes(ts.name, idx, Some(Class.forName(ts.syntax)))
    }

    def navigateTerms(t: Term.Arg, idx: Int): Unit = t match {
      case a: Term.Apply =>
        printTerm(a.fun, idx)
        if (a.fun.toString().startsWith("entity(")) {
          println("entity params")
          navigateTerms(a.fun, idx)
          a.args.foreach(printTerm(_, idx))
        }
        a.args.foreach(navigateTerms(_, idx + 2))
      case at: Term.ApplyType =>
        printTerm(at.fun, idx)
        at.targs.foreach(f => navigateTypes(f, idx, None))

      case a: Term.ApplyInfix =>
        a.args.foreach(navigateTerms(_, idx + 2))
        navigateTerms(a.lhs, idx + 2)
      case f: Term.Function =>
        navigateTerms(f.body, idx + 2)
      case pf: Term.PartialFunction =>
        pf.cases.foreach(f => navigateTerms(f.body, idx + 2))
      case s: Term.Select =>
        printTerm(s, idx)

      case n: Term.Name =>
        printTerm(n, idx)
    }

    defn match {
      case valn: Defn.Val =>
        navigateTerms(valn.rhs, 0)

        this match {
          case q"new $_()" =>
            //            val body: Term = CacheMacroImpl.expand(tpr, backendParam, defn)
            defn
          case x =>
            abort(s"Unrecognized pattern $x")
        }

      case _ =>
        abort("This annotation only works on `def`")
    }
  }
}

//
//object CacheMacroImpl {
//
//  def expand(fnTypeParams: Seq[Type], cacheExpr: Term.Arg, annotatedDef: Defn.Def): Term = {
//    val cache: Term.Name = Term.Name(cacheExpr.syntax)
//    annotatedDef match {
//      case q"..$_ def $methodName[..$tps](..$nonCurriedParams): $rtType = $expr" =>
//
//        if (nonCurriedParams.size == 1) {
//          val paramAsArg = Term.Name(nonCurriedParams.head.name.value)
//          q"""
//            val result: ${rtType} = $cache.get($paramAsArg) match {
//              case Some(v) => v
//              case None =>
//                val value = ${expr}
//                $cache.put($paramAsArg, value)
//                value
//            }
//            result
//           """
//        } else {
//          val paramAsArg = nonCurriedParams.map(p => Term.Name(p.name.value))
//          q"""
//            val result: ${rtType} = $cache.get((..$paramAsArg)) match {
//              case Some(v) => v
//              case None =>
//                val value = ${expr}
//                $cache.put((..$paramAsArg), value)
//                value
//            }
//            result
//           """
//        }
//      case other => abort(s"Expected non-curried method, got $other")
//    }
//  }
//}
