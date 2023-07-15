import chisel3._
import chisel3.util._
import chisel3.experimental._

object Operation {
    val ADDI   =  0.U(4.W)
    val ADD    =  1.U(4.W)
    val SLLI   =  2.U(4.W)
    val SLL    =  3.U(4.W)
    val SLTI   =  4.U(4.W)
    val SLT    =  5.U(4.W)
    val SLTIU  =  6.U(4.W)
    val SLTU   =  7.U(4.W)
    val XORI   =  8.U(4.W)
    val XOR    =  9.U(4.W)
    val SRI    = 10.U(4.W)
    val SR     = 11.U(4.W)
    val ORI    = 12.U(4.W)
    val OR     = 13.U(4.W)
    val ANDI   = 14.U(4.W)
    val AND    = 15.U(4.W)
}

import Operation._

class AluIO(xlen: Int) extends Bundle {
    val inst = Input(UInt(xlen.W))
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

    val imm12 = io.inst(31, 20)
    val funct7 = io.inst(31, 25)
    val shamt = io.inst(24, 20)
    val funct3 = io.inst(14, 12)
    val opcode = io.inst(6, 0)

    val operation = Cat(funct3, opcode(5))
    val oper_ext = io.inst(30)

    // Sign-extend the immediate value
    val extended_sign = Fill(xlen - 12, imm12(11))
    val imm_xlen_s = Cat(extended_sign, imm12)

    // Just pad the immediate value
    val ext_xlen_u = 0.U((xlen - 12).W)
    val imm_xlen_u = Cat(ext_xlen_u, imm12)

    val add: UInt => UInt = a => io.rs1Data + a
    val sll: UInt => UInt = a => io.rs1Data << a(4, 0)
    val slt: UInt => UInt = a => Mux(io.rs1Data.asSInt < a.asSInt, 1.U, 0.U)
    val sltu: UInt => UInt = a => Mux(io.rs1Data < a, 1.U, 0.U)
    val xor: UInt => UInt = a => io.rs1Data ^ a
    val srl: UInt => UInt = a => io.rs1Data >> a(4, 0)
    val sra: UInt => UInt = a => (io.rs1Data.asSInt >> a(4, 0)).asUInt
    val or: UInt => UInt = a => io.rs1Data | a
    val and: UInt => UInt = a => io.rs1Data & a

    io.rdData := MuxLookup(
        operation,
        io.rs1Data,
        Seq(
            ADDI -> add(imm_xlen_s),
            ADD -> Mux(oper_ext === 0.B, add(io.rs2Data), add(-io.rs2Data)),
            SLLI -> sll(imm_xlen_s),
            SLL -> sll(io.rs2Data),
            SLTI -> slt(imm_xlen_s),
            SLT -> slt(io.rs2Data),
            SLTIU -> sltu(imm_xlen_u),
            SLTU -> sltu(io.rs2Data),
            XORI -> xor(imm_xlen_s),
            XOR -> xor(io.rs2Data),
            SRI -> Mux(oper_ext === 0.B, srl(shamt), sra(shamt)),
            SR -> Mux(oper_ext === 0.B, srl(io.rs2Data), sra(io.rs2Data)),
            ORI -> or(imm_xlen_s),
            OR -> or(io.rs2Data),
            ANDI -> and(imm_xlen_s),
            AND -> and(io.rs2Data),
        )
    )
}
