import chisel3._
import chisel3.util.log2Ceil

// Problem:
//
// Implement a register file of {m} registers of {n}-bit words.
// The register with address 0 must always return 0 (and writing 
// to it should not yield any effect whatsoever). All registers
// must be initialised to 0 upon reset.
//
class RegFile(val m: Int, val n: Int) extends Module {
    val addrWidth = log2Ceil(m)
    val io = IO(new Bundle {
        val wen     = Input(Bool())
        val wrAddr  = Input(UInt(addrWidth.W))
        val rdAddr1 = Input(UInt(addrWidth.W))
        val rdAddr2 = Input(UInt(addrWidth.W))
        val wrData  = Input(UInt(n.W))
        val rdData1 = Output(UInt(n.W))
        val rdData2 = Output(UInt(n.W))
    })

    // Instantiate the register file and initialise all to 0
    val regs = RegInit(VecInit(Seq.fill(m)(0.U(n.W))))

    // Output the appropriate registers
    io.rdData1 := regs(io.rdAddr1)
    io.rdData2 := regs(io.rdAddr2)

    // Write new value to register except for Register 0
    when (io.wen && (io.wrAddr =/= 0.U)) {
        regs(io.wrAddr) := io.wrData
    }
}
