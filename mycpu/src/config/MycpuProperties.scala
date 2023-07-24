package config
import chisel3._

trait MycpuProperties extends MycpuParam {
  import java.io.FileInputStream
  import java.util.Properties
  import scala.jdk.CollectionConverters._
  def readProps(fileName: String) = {
    val prop        = new Properties()
    val inputStream = new FileInputStream(fileName)
    prop.load(inputStream)
    prop.asScala.toMap
  }
  def getJavaProperty = {
    val prop         = new Properties()
    val propFileName = "application.properties"
    val inputStream  = new FileInputStream(propFileName)
    prop.load(inputStream)
    prop.asScala.toMap
  }
  val jprops = readProps("application.properties")
  def getInitProps = {
    val ssOpt = jprops.get("SNAPSHOT")
    val hasSs = ssOpt.isDefined
    if (hasSs) readProps("hitd/snapshot/" + ssOpt.get + "/nemu.properties") else Map[String, String]()
  }
  val initProps = getInitProps

  /* simulate option */
  val sim       = jprops.get("SIM").isDefined
  val verilator = sim
  val vivado    = !verilator
  val debug     = jprops.get("DEBUG").fold(if (sim) true else false)(_.toBoolean)
  val linux     = if (sim) jprops.get("SIM").get == "LINUX" else false

  val hasSnapShot = jprops.get("SNAPSHOT").isDefined || linux
}
object MycpuInit extends MycpuProperties {
  val PCReset = initProps.get("PC").fold(if (linux) "h80100000".U else "hbfc00000".U)(_.U)
  val PRFReset = (0 until 64).map(i => {
    if (i > 32) 0.U(32.W)
    else {
      initProps.get(s"GPR$i").fold(0.U(32.W))(_.U(32.W))
    }
  })
  val HIReset    = initProps.get("HI").fold(0.U)(_.U)
  val LOReset    = initProps.get("LO").fold(0.U)(_.U)
  val LLBITReset = initProps.get("LLBITS").fold(false.B)(_.toBoolean.B)
  def CP0Reset = {
    val allCP0key = List(
      "index",
      "random",
      "entrylo0",
      "entrylo1",
      "context",
      "config1",
      "pagemask",
      "wire",
      "badvaddr",
      "count",
      "entryhi",
      "compare",
      "status",
      "cause",
      "epc",
      "prid",
      "ebase",
      "config0",
      "config1"
    )
    initProps
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
      tmp.g    := initProps.get(s"TLB${i}G").fold(false.B)(_.toBoolean.B)
      tmp.v0   := initProps.get(s"TLB${i}V0").fold(false.B)(_.toBoolean.B)
      tmp.v1   := initProps.get(s"TLB${i}V1").fold(false.B)(_.toBoolean.B)
      tmp.d0   := initProps.get(s"TLB${i}D0").fold(false.B)(_.toBoolean.B)
      tmp.d1   := initProps.get(s"TLB${i}D1").fold(false.B)(_.toBoolean.B)
      tmp.c0   := initProps.get(s"TLB${i}C0").fold(0.U)(_.U)
      tmp.c1   := initProps.get(s"TLB${i}C1").fold(0.U)(_.U)
      tmp.pfn0 := initProps.get(s"TLB${i}PFN0").fold(0.U)(_.U)
      tmp.pfn1 := initProps.get(s"TLB${i}PFN1").fold(0.U)(_.U)
      tmp.vpn2 := initProps.get(s"TLB${i}VPN2").fold(0.U)(_.U)
      tmp.asid := initProps.get(s"TLB${i}ASID").fold(0.U)(_.U)
      tmp
    })
  }
}
