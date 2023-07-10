import chisel3._
import chisel3.util._
import chisel3.experimental._

object Funct3 {
    val addi    = 0.U(3.W)
    val slti    = 2.U(3.W)
    val sltiu   = 3.U(3.W)
    val xori    = 4.U(3.W)
    val ori     = 6.U(3.W)
    val andi    = 7.U(3.W)
}

import Funct3._

class ALU(val m: Int, val n: Int) extends Module {
    val addrWidth = log2Ceil(m)
    val io = IO(new Bundle {
        val enabled = Input(Bool())
        val imm12   = Input(UInt(12.W))
        val funct3  = Input(UInt(3.W))
        val rs1Data = Input(UInt(n.W))
        val rdData  = Output(UInt(n.W))
    })

    val extended_sign = Fill(n - 12, io.imm12(11))
    val imm32s = Cat(extended_sign, io.imm12)

    io.rdData := 0.U
    when (io.enabled) {
        io.rdData := MuxLookup(
            io.funct3,
            io.rs1Data,
            Seq(
                addi -> (io.rs1Data + imm32s)
            )
        )
    }
}
