import chisel3._
import chisel3.util._
import config._
import frontend._
import backend._
import utils._
import bundle._

class Axi2to1Arbiter extends MycpuModule {
  val io = IO(new Bundle {
    val imem   = Flipped(new DramReadIO)
    val dmem   = Flipped(new DramIO)
    val master = new AxiIO
  })
  val (imem, dmem, master)   = (io.imem, io.dmem, io.master)
  val (ar0, r0)              = (imem.ar.bits, imem.r.bits)
  val (ar1, r1, aw1, w1, b1) = (dmem.ar.bits, dmem.r.bits, dmem.aw.bits, dmem.w.bits, dmem.b.bits)
  val (ar, r, aw, w, b)      = (master.ar.bits, master.r.bits, master.aw.bits, master.w.bits, master.b.bits)

  //unused
  val arwUnused = Wire(new ArwUnused)
  asg(arwUnused.cache, 0.U(4.W))
  asg(arwUnused.lock, 0.U(2.W))
  asg(arwUnused.prot, 0.U(3.W))

  //ar channel
  asg(ar.used, Mux(dmem.ar.valid, ar1, ar0)) //pri
  asg(ar.unused, arwUnused)
  asg(master.ar.valid, imem.ar.valid || dmem.ar.valid)
  asg(dmem.ar.ready, dmem.ar.valid && master.ar.ready) //pri
  asg(imem.ar.ready, !dmem.ar.valid && imem.ar.valid && master.ar.ready) //pri

  //r channel
  asg(r0, r.used)
  asg(r1, r.used)
  val isDmem = isDataId(r.used.id)
  asg(imem.r.valid, !isDmem && master.r.valid)
  asg(dmem.r.valid, isDmem && master.r.valid)
  asg(master.r.ready, Mux(isDmem, dmem.r.ready, imem.r.ready))

  //aw channel
  asg(aw.used, aw1)
  asg(aw.unused, arwUnused)
  asg(master.aw.valid, dmem.aw.valid)
  asg(dmem.aw.ready, master.aw.ready)

  //w
  asg(w.used, w1)
  asg(master.w.valid, dmem.w.valid)
  asg(dmem.w.ready, master.w.ready)

  //b
  asg(b1, b.used)
  asg(dmem.b.valid, master.b.valid)
  asg(master.b.ready, dmem.b.ready)
}
