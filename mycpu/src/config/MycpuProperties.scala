package config
import chisel3._
import bundle.TLBEntry

trait MycpuProperties extends MycpuParam {
  def getJavaProperty = {
    import java.io.FileInputStream
    import java.util.Properties
    import scala.jdk.CollectionConverters._
    val prop         = new Properties()
    val propFileName = "application.properties"
    val inputStream  = new FileInputStream(propFileName)
    prop.load(inputStream)
    prop.asScala.toMap
  }
  val jpy = getJavaProperty

  /* simulate option */
  val sim       = jpy.get("SIM").isDefined
  val verilator = sim
  val vivado    = !verilator
  val debug     = jpy.get("DEBUG").fold(if (sim) true else false)(_.toBoolean)
  val linux     = if (sim) jpy.get("SIM").get == "LINUX" else false
}
object MycpuInit extends MycpuProperties {
  val PCReset = jpy.get("PC").fold(if (linux) "h80100000".U else "hbfc00000".U)(_.U)
  val PRFReset = (0 until 64).map(i => {
    if (i > 32) 0.U(32.W)
    else {
      jpy.get(s"GPR$i").fold(0.U(32.W))(_.U(32.W))
    }
  })
  val HIReset    = jpy.get("HI").fold(0.U)(_.U)
  val LOReset    = jpy.get("LO").fold(0.U)(_.U)
  val LLBITReset = jpy.get("LLBITS").fold(false.B)(_.toBoolean.B)
  def CP0Reset = {
    val allCP0key = List(
      "INDEX",
      "RANDOM",
      "ENTRYLO0",
      "ENTRYLO1",
      "CONTEXT",
      "CONFIG1",
      "PAGEMASK",
      "WIRE",
      "BADVADDR",
      "COUNT",
      "ENTRYHI",
      "COMPARE",
      "STATUS",
      "CAUSE",
      "EPC",
      "PRID",
      "EBASE",
      "CONFIG0",
      "CONFIG1"
    )
    jpy
      .flatMap({
        case (key, value) if allCP0key.contains(key) => Some(key -> value.U)
        case _                                       => None
      })
      .toMap
  }
  def TLBReset = {
    import bundle.TLBEntry
    (0 until tlbEntriesNum).map(i => {
      val tmp = Wire(new TLBEntry)
      tmp.g    := jpy.get(s"TLB${i}G").fold(false.B)(_.toBoolean.B)
      tmp.v0   := jpy.get(s"TLB${i}V0").fold(false.B)(_.toBoolean.B)
      tmp.v1   := jpy.get(s"TLB${i}V1").fold(false.B)(_.toBoolean.B)
      tmp.d0   := jpy.get(s"TLB${i}D0").fold(false.B)(_.toBoolean.B)
      tmp.d1   := jpy.get(s"TLB${i}D1").fold(false.B)(_.toBoolean.B)
      tmp.c0   := jpy.get(s"TLB${i}C0").fold(0.U)(BigInt(_, 16).U)
      tmp.c1   := jpy.get(s"TLB${i}C1").fold(0.U)(BigInt(_, 16).U)
      tmp.pfn0 := jpy.get(s"TLB${i}PFN0").fold(0.U)(BigInt(_, 16).U)
      tmp.pfn1 := jpy.get(s"TLB${i}PFN1").fold(0.U)(BigInt(_, 16).U)
      tmp.vpn2 := jpy.get(s"TLB${i}VPN2").fold(0.U)(BigInt(_, 16).U)
      tmp.asid := jpy.get(s"TLB${i}ASID").fold(0.U)(BigInt(_, 16).U)
      tmp
    })
  }
}
