package copmacro

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.collection.mutable.ArrayBuffer

@compileTimeOnly("enable macro paradise to expand macro annotations")
class MacroCOP extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroCOP.impl
}

object MacroCOP {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def generateBundle(
      regName: TypeName,
      sel:     Tree,
      rd:      Tree,
      fields:  List[Tuple5[TermName, Tree, Tree, Tree, Tree]]
    ) = {
      val defList = fields
        .map(attr => {
          val (fieldName, msb, lsb, initValue, writable) = attr
          q"val $fieldName = chisel3.UInt(($msb - $lsb+1).W)"
        })

      val initStr = fields
        .map(attr => {
          val (fieldName, msb, lsb, initValue, writable) = attr
          s"$fieldName := $initValue.U"
        })
        .mkString("\n")
      val initCode = c.parse(s"def init = { $initStr }")
      val readStr = fields
        .map(attr => {
          val (fieldName, msb, lsb, initValue, writable) = attr
          s"| chisel3.util.Cat(0.U((31-$msb).W), ${fieldName}, 0.U($lsb.W))"
        })
        .mkString(" ")
      val readCode = c.parse(s"""
      def read = {
        ${readStr.substring(1)}
      }""")
      val writeStr = fields
        .map(attr => {
          val (fieldName, msb, lsb, initValue, writable) = attr
          s"if ($writable) $fieldName := data($msb, $lsb)"
        })
        .mkString("\n")
      val writeCode = c.parse(s"""
      def write(data: chisel3.UInt) = {
        require(data.getWidth ==32)
        $writeStr
      }""")
      val res       = q"""
      class $regName extends chisel3.Bundle {
        ..$defList
        $initCode
        $readCode
        $writeCode
      }
      """
      res
    }

    def extractClassNameAndBody(classDecl: ModuleDef) = {
      try {
        val q"object $name { ..$body }" = classDecl
        val regDefStr                   = new StringBuffer()
        val createFunctions             = new StringBuffer()
        val addrMatchStr                = new StringBuffer()
        val readStr                     = new StringBuffer()
        val writeStr                    = new StringBuffer()
        val regsDef = body.collect {
          case (cls: ClassDef) => {
            val q"class $regName(sel: Int = $sel, rd: Int = $rd) { ..$body}" = cls
            // init
            val createfunc = body.collectFirst({ case q"def create = {..$body}" => body })
            val nameStr    = regName.toTermName.toString + "Reg"
            regDefStr.append(s"""val $nameStr = chisel3.RegInit(new ${regName.toTermName}, {
                val init = Wire(new $regName)
                init.init
                init
              })
              """)
            createFunctions.append(s"${createfunc.fold("")(_.mkString("\n"))}\n")
            addrMatchStr.append(
              s"val ${regName.toTermName}Wen = ($sel.U === writeSel) && ($rd.U === writeRd) && isMtc0\n"
            )
            // mfc0
            readStr.append(s", ((sel===$sel.U && rd===$rd.U) -> ${regName.toTermName}Reg.read) \n")
            // mtc0
            val writefunc = body.collectFirst({ case q"def write(data: UInt) = {..$body}" => body })
            writeStr.append(s"""
            when (${regName.toTermName}Wen) {
              ${regName.toTermName}Reg.write(writeData)
              ${writefunc.fold("")(_.mkString("\n"))}
            }""")
            val attrList = body.collect(ele => {
              ele match {
                case q"val $fieldName = ($msb, $lsb, $initValue, $writable)" => {
                  (fieldName, msb, lsb, initValue, writable)
                }
                case q"val $fieldName = ($msb, $lsb, $initValue)" => {
                  (fieldName, msb, lsb, initValue, q"false")
                }
              }
            })
            generateBundle(regName, sel, rd, attrList)
          }
        }
        val copRegs = c.parse(s"""
        class BasicCOP extends chisel3.Module {
          val isMtc0 = WireInit(false.B)
          val writeSel = WireInit(0.U(3.W))
          val writeRd  = WireInit(0.U(5.W))
          val writeData  = WireInit(0.U(32.W))
          $regDefStr
          $addrMatchStr
          $writeStr
          $createFunctions
          def mtc0(sel:chisel3.UInt, rd:chisel3.UInt, data:chisel3.UInt) = {
            require(sel.getWidth==3)
            require(rd.getWidth==5)
            require(data.getWidth==32)
            isMtc0 := true.B
            writeSel := sel
            writeRd  := rd
            writeData := data
          }
          def mfc0(sel:chisel3.UInt, rd:chisel3.UInt):UInt = {
            require(sel.getWidth==3)
            require(rd.getWidth==5)
            chisel3.util.MuxLookup(chisel3.util.Cat(sel,rd), 0.U)(Seq(
            ${readStr.substring(1)}
            ))
          }
        }
        """)
        q"""
        ..$regsDef
        $copRegs
        """
      } catch {
        case _: MatchError => c.abort(c.enclosingPosition, "not match class")
      }
    }

    annottees.map(_.tree) match {
      case (param: ModuleDef) :: Nil => {
        val res = extractClassNameAndBody(param)
        println(res)
        c.Expr(res)
      }
      case _ => {
        c.abort(c.enclosingPosition, "not a object")
      }
    }
  }
}
