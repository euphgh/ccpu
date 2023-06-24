package object cop {
  import copmacro._
  import chisel3._
  val WR = true; val R = false
  @MacroCOP
  object MyCop { // any name(MyCop) is ok,  only need import cop._ before use
    class entryHi(sel: Int = 0, rd: Int = 10) {
      val vpn2 = (31, 13, 0x0, WR)
    }
    class context(sel: Int = 4, rd: Int = 0) { // write name, sel and rd, use int not UInt
      val ptebase = (31, 23, 0x0, WR)
      val badvpn2 = (22, 4, 0x0, WR)
      val z3_0    = (3, 0, 0x0)
      def write(data: UInt) = {}
      def create = {}
    }
    class pagemask(sel: Int = 5, rd: Int = 0) {
      val z31_29 = (31, 29, 0x0)
      val mask   = (28, 13, 0x0)
      val maskx  = (12, 11, 0x0)
      val z10_0  = (10, 0, 0x0)
    }
  }
}

/* above MyCop object will be expanded into below */
// object cop {
//   class context extends chisel3.Bundle {
//     val ptebase = UInt((31 - 23).W)
//     val badvpn2 = UInt((22 - 4).W)
//     val z3_0 = UInt((3 - 0).W)
//     def init = {
//       ptebase := 0.U
//       badvpn2 := 0.U
//       z3_0 := 0.U
//     }
//     def read =  (Cat(0.U(31 - 31).W), ptebase, 0.U(23.W))|
//                 (Cat(0.U(31 - 22).W), badvpn2, 0.U(4.W))|
//                 (Cat(0.U(31 - 3).W), z3_0, 0.U(0.W))
//     def write(data: chisel3.UInt) = {
//       require(data.getWidth == (32))
//       if (WR)
//         ptebase.$colon$eq(data(31, 23))
//       if (WR)
//         badvpn2.$colon$eq(data(22, 4))
//       if (false)
//         z3_0.$colon$eq(data(3, 0))
//     }
//   }
//   class pagemask extends chisel3.Bundle {
//     val z31_29 = UInt(31 - 29).W
//     val mask = UInt(28 - 13).W
//     val maskx = UInt(12 - 11).W
//     val z10_0 = UInt(10 - 0).W
//     def init = {
//       z31_29.$colon$eq(0.U)
//       mask.$colon$eq(0.U)
//       maskx.$colon$eq(0.U)
//       z10_0.$colon$eq(0.U)
//     }
//     def read =  (Cat(0.U(31 - 31).W), z31_29, 0.U(29.W))|
//                 (Cat(0.U(31 - 28).W), mask, 0.U(13.W))|
//                 (Cat(0.U(31 - 12).W), maskx, 0.U(11.W))|
//                 (Cat(0.U(31 - 10).W), z10_0, 0.U(0.W))
//     def write(data: chisel3.UInt) = {
//       require(data.getWidth == (32))
//       if (false)
//         z31_29.$colon$eq(data(31, 29))
//       if (false)
//         mask.$colon$eq(data(28, 13))
//       if (false)
//         maskx.$colon$eq(data(12, 11))
//       if (false)
//         z10_0.$colon$eq(data(10, 0))
//     }
//   }
//   class COPRegs extends chisel3.Module {
//     val contextReg = chisel3.RegInit(new context(), {
//       val init = Wire(new context())
//       init.init
//       init
//     })
//     val pagemaskReg = chisel3.RegInit(new pagemask(), {
//       val init = Wire(new pagemask())
//       init.init
//       init
//     })
//     def mtc0(sel: chisel3.UInt, rd: chisel3.UInt, data: chisel3.UInt) = {
//       require(sel.getWidth == (3))
//       require(rd.getWidth == (5))
//       require(data.getWidth == (32))
//       val contextWen = Cat(4.U, 0.U) === (Cat(sel, rd))
//       when(contextWen)({
//         contextReg.write(data)
//         pagemaskReg.mask.$colon$eq(5.U)
//       })
//       val pagemaskWen = Cat(5.U, 0.U) === (Cat(sel, rd))
//       when(pagemaskWen)(pagemaskReg.write(data))
//     }
//     def mfc0(sel: UInt, rd: UInt): UInt = MuxLookup(Cat(sel, rd), 0.U)(Seq(
//       Cat(4.U(3.W), 0.U(5.U)). -> (contextReg.read),
//       Cat(5.U(3.W), 0.U(5.U)). -> (pagemaskReg.read))
//       )
//   }
// }
