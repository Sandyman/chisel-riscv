import chisel3._
import chisel3.util.log2Ceil

// Problem:
//
// Implement a register file of {m} registers of {n}-bit words.
// The register with address 0 must always return 0 (and writing 
// to it should not yield any effect whatsoever). All registers
// must be initialised to 0 upon reset.
//

class RegFileIO(m: Int, xlen: Int) extends Bundle {
    def addrWidth = log2Ceil(m)
    val wen     = Input(Bool())
    val wrAddr  = Input(UInt(addrWidth.W))
    val rdAddr1 = Input(UInt(addrWidth.W))
    val rdAddr2 = Input(UInt(addrWidth.W))
    val wrData  = Input(UInt(xlen.W))
    val rdData1 = Output(UInt(xlen.W))
    val rdData2 = Output(UInt(xlen.W))
}

trait RegFile extends Module {
    def m: Int
    def xlen: Int
    val io: RegFileIO
}

class SimpleRegFile(val m: Int, val xlen: Int) extends RegFile {
    val io = IO(new RegFileIO(m, xlen))

    // Instantiate the register file and initialise all to 0
    val regs = RegInit(VecInit(Seq.fill(m)(0.U(xlen.W))))

    // Output the appropriate registers
    io.rdData1 := regs(io.rdAddr1)
    io.rdData2 := regs(io.rdAddr2)

    // Write new value to register except for Register 0
    when (io.wen && (io.wrAddr =/= 0.U)) {
        regs(io.wrAddr) := io.wrData
    }
}
