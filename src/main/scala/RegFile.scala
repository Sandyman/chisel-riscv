import chisel3._
import chisel3.util.log2Ceil

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

    // Instantiate the register file
    val regs = Mem(m, UInt(xlen.W))

    // Output the appropriate registers
    io.rdData1 := Mux(io.rdAddr1.orR, regs(io.rdAddr1), 0.U)
    io.rdData2 := Mux(io.rdAddr2.orR, regs(io.rdAddr2), 0.U)

    // Write new value to register except for Register 0
    when (io.wen & io.wrAddr.orR) {
        regs(io.wrAddr) := io.wrData
    }
}
