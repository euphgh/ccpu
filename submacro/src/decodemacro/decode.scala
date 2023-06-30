package decodemacro
import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
  * MacroDecode use example:
  * 1. write [[import decodemacro._]]
  * 2. def a Bundle which is used to Decode output like blow
  * @example:
  *   @ MacroDecode
  *   class DecodeOut extends Bundle {
  *     val foo = ChiselFuType()
  *     val bar = BranchType()
  *   }
  * 3. new this Bundle as Wire, Reg or Vec, like [[val foo = Wire(new DecodeOut)]]
  * 4. call "decode" method in a module, then decode logic will be generated in this module,
  *    like [[foo.decode(input, relation, default)]]
  * 5. use foo.foo, foo.bar to get output
  *
  * Detail: macros will expand class to blow
  * class DecodeOut extends Bundle {
  *   val foo = ChiselFuType()
  *   val bar = BranchType()
  *
  *   // start macro generate
  *   def trans(const: List[chisel3.ChiselEnum#Type]): chisel3.util.BitPat = {
  *     val optChiselFuType = const.find(_.isInstanceOf[ChiselFuType.Type])
  *     val bpChiselFuType =
  *       if (optChiselFuType.isDefined) chisel3.util.BitPat(optChiselFuType.get.asUInt)
  *       else chisel3.util.BitPat("b" + "?" * ChiselFuType.getWidth)
  *     bpChiselFuType
  *   }
  *   def decode(
  *     input:    UInt,
  *     relation: Seq[Tuple2[chisel3.util.BitPat, List[chisel3.ChiselEnum#Type]]],
  *     default:  List[chisel3.ChiselEnum#Type],
  *     opt:      chisel3.util.experimental.decode.Minimizer = chisel3.util.experimental.decode.EspressoMinimizer
  *   ) = {
  *     relation.foreach(tuple => {
  *       require(tuple._1.getWidth == input.getWidth)
  *     })
  *     require(input.getWidth == relation(0)._1.getWidth)
  *     // this assign can not pass compile, only as example
  *     chisel3.util.Cat(foo.asUInt, bar.asUInt) := chisel3.util.experimental.decode.decoder(
  *       opt,
  *       input,
  *       chisel3.util.experimental.decode.TruthTable(
  *         relation.map(x => {
  *           (x._1, trans(x._2))
  *         }),
  *         trans(default)
  *       )
  *     )
  *   }
  * }
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class MacroDecode extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroDecode.impl
}

object MacroDecode {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def extractClassNameAndBody(classDecl: ClassDef) = {
      val constructCode = new StringBuffer()
      val bitPatGenCode = new StringBuffer()
      val bitPatRetCode = new StringBuffer()
      val assignCode    = new StringBuffer()
      try {
        val q"class $className(..$fields) extends ..$bases { ..$body }" = classDecl
        body.foreach(ele => {
          ele match {
            case q"val $nameTerm = $rhsTerm()" => {
              val name = nameTerm.toTermName.toString
              val rhs  = rhsTerm.toString
              constructCode.append(
                s"""
                val start${name} = counter
                val width${name} = ${rhs}.getWidth
                counter += width${name}
                """
              )
              bitPatGenCode.append(s"""
              val opt${name} = const.find(_.isInstanceOf[${rhs}.Type])
              val bip${name} = if (opt${name}.isDefined) chisel3.util.BitPat(opt${name}.get.litValue.U(width${name}.W)) 
                                          else chisel3.util.BitPat("b" + "?" * width${name})
              """)
              bitPatRetCode.append(s" ## bip${name}")
              assignCode.append(
                s"${name} := ${rhs}.safe(tmp(start${name}+width${name}-1, start${name}))._1\n"
              )
            }
            case q"val $nameTerm = $rhsTerm.$default" => {
              val name = nameTerm.toTermName.toString
              val rhs  = rhsTerm.toString
              constructCode.append(
                s"""
                val start${name} = counter
                val width${name} = ${rhs}.getWidth
                counter += width${name}
                """
              )
              bitPatGenCode.append(s"""
              val opt${name} = const.find(_.isInstanceOf[${rhs}.Type])
              val bip${name} = if (opt${name}.isDefined) chisel3.util.BitPat(opt${name}.get.litValue.U(width${name}.W)) 
                                          else chisel3.util.BitPat(${rhsTerm}.${default}.litValue.U(width${name}.W))
              """)
              bitPatRetCode.append(s" ## bip${name}")
              assignCode.append(
                s"${name} := ${rhs}.safe(tmp(start${name}+width${name}-1, start${name}))._1\n"
              )
            }
            case _ => c.abort(c.enclosingPosition, "can not write in decode out")
          }
        })
        val newClass = s"""
        class $className(${fields.map(_.toString()).mkString}) extends ${bases
          .map(_.toString())
          .mkString} { 
          var counter = 0
          ${constructCode.toString()}
          ${body.map(_.toString()).mkString("\n")}
          def trans(const: List[chisel3.ChiselEnum#Type]): chisel3.util.BitPat = {
            ${bitPatGenCode.toString()}
            ${bitPatRetCode.substring(3)}
          }
          def decode(
            input:    UInt,
            relation: Seq[Tuple2[chisel3.util.BitPat, List[chisel3.ChiselEnum#Type]]],
            default:  List[chisel3.ChiselEnum#Type],
            opt:      chisel3.util.experimental.decode.Minimizer = chisel3.util.experimental.decode.EspressoMinimizer
          ) = {
            relation.foreach(tuple => {
              require(tuple._1.getWidth == input.getWidth)
            })
            require(input.getWidth == relation(0)._1.getWidth)
            val tmp = chisel3.util.experimental.decode.decoder(
              opt,
              input,
              chisel3.util.experimental.decode.TruthTable(
                relation.map(x => {
                  (x._1, trans(x._2))
                }),
                trans(default)
              )
            )
            ${assignCode.toString()}
          }
        }"""
        c.parse(newClass)
      } catch {
        case _: MatchError => c.abort(c.enclosingPosition, "not match class")
      }
    }

    annottees.map(_.tree) match {
      case (param: ClassDef) :: Nil => {
        val res = extractClassNameAndBody(param)
        // println(res)
        c.Expr(res)
      }
      case _ => {
        println("not match")
        c.abort(c.enclosingPosition, "hahaha")
      }
    }
  }
}
