import config._
package object cop extends MycpuParam {
  import copmacro._
  import chisel3._
  import chisel3.util._

  val TLBNR       = tlbEntriesNum
  val TLBIDXWIDTH = tlbIndexWidth
  val WR          = true
  val R           = false
  val cacheSetMap = Map(
    64   -> 0,
    128  -> 1,
    256  -> 2,
    512  -> 3,
    1024 -> 4,
    2048 -> 5,
    4096 -> 6
  )
  val cacheBytesMap = Map(
    4   -> 1,
    8   -> 2,
    16  -> 3,
    32  -> 4,
    64  -> 5,
    128 -> 6
  )
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
        val tick = RegInit(false.B)
        tick := !tick
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
      val iph7    = (15, 15, 0)
      val iph6    = (14, 14, 0)
      val iph5    = (13, 13, 0)
      val iph4    = (12, 12, 0)
      val iph3    = (11, 11, 0)
      val iph2    = (10, 10, 0)
      val ips1    = (9, 9, 0, WR)
      val ips0    = (8, 8, 0, WR)
      val z7_7    = (7, 7, 0)
      val exccode = (6, 2, 0)
      val z1_0    = (1, 0, 0)
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
      val m  = (31, 31, 0) //值为0，表示不存在config2寄存器
      val ms = (30, 25, tlbEntriesNum - 1) //TLB大小,7对应8

      val is = (24, 22, cacheSetMap(math.pow(2, IcacheIndexWidth).toInt)) //组数目
      val il = (21, 19, cacheBytesMap(IcachLineBytes)) //行大小
      val ia = (18, 16, (IcachRoads - 1)) //相联度

      val ds = (15, 13, cacheSetMap(math.pow(2, DcacheIndexWidth).toInt))
      val dl = (12, 10, cacheBytesMap(DcachLineBytes))
      val da = (9, 7, (DcachRoads - 1))

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
