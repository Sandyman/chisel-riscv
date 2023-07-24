package eek

import chisel3._
import chisel3.util._
import chisel3.experimental._

class IMemIO(xlen: Int) extends Bundle {
    val pc = Input(UInt(xlen.W))
    val inst = Output(UInt(xlen.W))
}

trait IMem extends Module {
    def s: Int
    def xlen: Int
    val io: IMemIO
}

class SimpleInstructionMemory(val s: Int, val xlen: Int) extends IMem {
    val io = IO(new IMemIO(xlen))

    // Create the instruction memory, fill with NOP instructions
    val instMem = VecInit(Seq.fill(s)(0x33.U(xlen.W)))              // Initialise with NOP

    // Add a few "useful" instructions so we can test

    //                3         2            1          0
    //               109876543210 98765 432 10987 6543210
    instMem(0) := "0b000000000001_00000_000_11100_0010011".U    // ADDI x28, x0, 1
    instMem(1) := "0b000000001111_11100_000_00101_0010011".U    // ADDI x5, x28, 15

    //                3          2            1          0
    //               1098765 43210 98765 432 10987 6543210
    instMem(2) := "0b0000000_00101_11100_001_00110_0110011".U   // SLL x6, x28, x5

    // x6 should now be 0x0001_0000 (== 65536 == 1 << 16)
    instMem(3) := "0b00000000000000010000_00101_0110111".U      // LUI x5, (1 << 4) << 12
}
