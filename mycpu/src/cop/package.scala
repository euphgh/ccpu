package object cop {
  import copmacro._
  import chisel3._
  import chisel3.util._

  val TLBNR       = 8
  val TLBIDXWIDTH = log2Up(TLBNR)
  val WR          = true; val R = false
  @MacroCOP
  object MyCop { // any name(MyCop) is ok,  only need import cop._ before use
    class index(sel: Int = 0, rd: Int = 0) {
      val p     = (31, 31, 0x0)
      val z30_n = (30, TLBIDXWIDTH, 0x0)
      val index = ((TLBIDXWIDTH - 1), 0, 0x0, WR)
    }
    class random(sel: Int = 0, rd: Int = 1) {
      val z31_n  = (31, TLBIDXWIDTH, 0x0)
      val random = ((TLBIDXWIDTH - 1), 0, (TLBNR - 1))
    }
    class entrylo0(sel: Int = 0, rd: Int = 2) {
      val fill = (31, 26, 0)
      val pfn  = (25, 6, 0, WR)
      val c    = (5, 3, 0, WR)
      val d    = (2, 2, 0, WR)
      val v    = (1, 1, 0, WR)
      val g    = (0, 0, 0, WR)
    }
    class entrylo1(sel: Int = 0, rd: Int = 3) {
      val fill = (31, 26, 0)
      val pfn  = (25, 6, 0, WR)
      val c    = (5, 3, 0, WR)
      val d    = (2, 2, 0, WR)
      val v    = (1, 1, 0, WR)
      val g    = (0, 0, 0, WR)
    }
    class context(sel: Int = 0, rd: Int = 4) { // write name, sel and rd, use int not UInt
      val ptebase = (31, 23, 0x0, WR)
      val badvpn2 = (22, 4, 0x0)
      val z3_0    = (3, 0, 0x0, WR)

    }
    class pagemask(sel: Int = 0, rd: Int = 5) {
      val z31_29 = (31, 29, 0x0)
      val mask   = (28, 13, 0x0)
      val maskx  = (12, 11, 0x0)
      val z10_0  = (10, 0, 0x0)
    }
    class wire(sel: Int = 0, rd: Int = 6) {
      val z31_n = (31, TLBIDXWIDTH, 0)
      val wire  = ((TLBIDXWIDTH - 1), 0, (TLBNR - 1), WR)
      def write(data: UInt) = {
        randomReg.random := ((TLBNR - 1).U)
      }
    }
    class badvaddr(sel: Int = 0, rd: Int = 8) {
      val all = (31, 0, 0)
    }
    class count(sel: Int = 0, rd: Int = 9) {
      val all = (31, 0, 0, WR)
      def create = {
        val tick = RegInit(0.U)
        tick := ~tick
        when(!countWen && tick) { countReg.all := countReg.all + 1.U }
      }
    }
    class entryhi(sel: Int = 0, rd: Int = 10) {
      val vpn2  = (31, 13, 0, WR)
      val vpn2x = (12, 11, 0)
      val z10_8 = (10, 8, 0)
      val asid  = (7, 0, 0, WR)
    }
    class compare(sel: Int = 0, rd: Int = 11) {
      val all = (31, 0, 0, WR)
      def write(data: UInt) = {
        causeReg.ti := 0.U
      }
    }
    class status(sel: Int = 0, rd: Int = 12) {
      val cu321  = (31, 29, 0)
      val cu0    = (28, 28, 0, WR)
      val z27_23 = (27, 23, 0)
      val bev    = (22, 22, 1, WR)
      val z21_16 = (21, 16, 0)
      val im     = (15, 8, 0, WR)
      val z7_5   = (7, 5, 0)
      val ksu    = (4, 3, 0, WR)
      val erl    = (2, 2, 1, WR)
      val exl    = (1, 1, 0, WR)
      val ie     = (0, 0, 0, WR)
    }
    class cause(sel: Int = 0, rd: Int = 13) {
      val bd      = (31, 31, 0)
      val ti      = (30, 30, 0)
      val z29_24  = (29, 24, 0)
      val iv      = (23, 23, 0, WR)
      val z22_16  = (22, 16, 0)
      val ip_h    = (15, 10, 0)
      val ip_s    = (9, 8, 0, WR)
      val z7_7    = (7, 7, 0)
      val exccode = (6, 2, 0)
      val z1_0    = (1, 0, 0)
      def create = {
        when(countReg.all === compareReg.all && !compareWen) {
          causeReg.ti := 1.U
        }
      }
    }
    class epc(sel: Int = 0, rd: Int = 14) {
      val all = (31, 0, 0, WR)
    }
    class prid(sel: Int = 0, rd: Int = 15) {
      val cpo  = (31, 24, 0)
      val cpid = (23, 16, 1)
      val prid = (15, 8, 0x80)
      val revs = (7, 0, 0x3)
    }
    class ebase(sel: Int = 1, rd: Int = 15) {
      val one31   = (31, 31, 1)
      val z30     = (30, 30, 0)
      val eptbase = (29, 12, 0, WR)
      val z11_10  = (11, 10, 0)
      val cpunum  = (9, 0, 0)
    }
    class config0(sel: Int = 0, rd: Int = 16) {
      val m    = (31, 31, 1)
      val k23  = (30, 28, 0)
      val ku   = (27, 25, 0)
      val impl = (24, 16, 0)
      val be   = (15, 15, 0)
      val at   = (14, 13, 0)
      val ar   = (12, 10, 0)
      val mt   = (9, 7, 1)
      val z6_4 = (6, 4, 0)
      val vi   = (3, 3, 0)
      val k0   = (2, 0, 0x3, WR)
    }
    class config1(sel: Int = 1, rd: Int = 16) {
      val m  = (31, 31, 0)
      val ms = (30, 25, 0xf)
      val is = (24, 22, 0)
      val il = (21, 19, 0x5)
      val ia = (18, 16, 0x1)
      val ds = (15, 13, 0)
      val dl = (12, 10, 0x5)
      val da = (9, 7, 0x1)
      val c2 = (6, 6, 0)
      val md = (5, 5, 0)
      val pc = (4, 4, 0)
      val wr = (3, 3, 0)
      val ca = (2, 2, 0)
      val ep = (1, 1, 0)
      val fp = (0, 0, 0)
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
