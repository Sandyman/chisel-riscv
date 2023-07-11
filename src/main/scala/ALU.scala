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

class AluIO(xlen: Int) extends Bundle {
        val enabled = Input(Bool())
        val imm12   = Input(UInt(12.W))
        val funct3  = Input(UInt(3.W))
        val rs1Data = Input(UInt(xlen.W))
        val rs2Data = Input(UInt(xlen.W))
        val rdData  = Output(UInt(xlen.W))
}

trait Alu extends Module {
    def m: Int
    def xlen: Int
    val io: AluIO
}

class AluSimple(val m: Int, val xlen: Int) extends Alu {
    val io = IO(new AluIO(xlen))

    val funct7 = io.imm12(11, 5)
    val shamt = io.imm12(4, 0)

    // Sign-extend the immediate value
    val extended_sign = Fill(xlen - 12, io.imm12(11))
    val imm_xlen_s = Cat(extended_sign, io.imm12)

    // Just pad the immediate value
    val ext_xlen_u = 0.U((xlen - 12).W)
    val imm_xlen_u = Cat(ext_xlen_u, io.imm12)

    io.rdData := 0.U
    when (io.enabled) {
        io.rdData := MuxLookup(
            io.funct3,
            io.rs1Data,
            Seq(
                addi -> (io.rs1Data + imm_xlen_s),
                slti -> (Mux(io.rs1Data < imm_xlen_s, 1.U, 0.U)),
                sltiu -> (Mux(io.rs1Data < imm_xlen_u, 1.U, 0.U)),
                xori -> (io.rs1Data ^ imm_xlen_s),
                ori -> (io.rs1Data | imm_xlen_s),
                andi -> (io.rs1Data & imm_xlen_s),
            )
        )
    }
}
