import chisel3._
import chisel3.util._
import chisel3.experimental._

object Funct3 {
    val ADD    = 0.U(3.W)
    val SLL    = 1.U(3.W)
    val SLT    = 2.U(3.W)
    val SLTU   = 3.U(3.W)
    val XOR    = 4.U(3.W)
    val SR     = 5.U(3.W)
    val OR     = 6.U(3.W)
    val AND    = 7.U(3.W)
}

object Shift2 {
    val SRLI  = 0.U(2.W)
    val SRAI  = 1.U(2.W)
    val SRL   = 2.U(2.W)
    val SRA   = 3.U(2.W)
}

import Funct3._
import Shift2._

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

    val opcode2 = Cat(io.inst(5), io.inst(30))

    // Sign-extend the immediate value
    val extended_sign = Fill(xlen - 12, imm12(11))
    val imm_xlen_s = Cat(extended_sign, imm12)

    // Just pad the immediate value
    val ext_xlen_u = 0.U((xlen - 12).W)
    val imm_xlen_u = Cat(ext_xlen_u, imm12)

    val add: UInt => UInt = a => io.rs1Data + a
    val sll: UInt => UInt = a => io.rs1Data << a
    val slt: UInt => UInt = a => Mux(io.rs1Data.asSInt < a.asSInt, 1.U, 0.U)
    val sltu: UInt => UInt = a => Mux(io.rs1Data < a, 1.U, 0.U)
    val xor: UInt => UInt = a => io.rs1Data ^ a
    val srl: UInt => UInt = a => io.rs1Data >> a
    val sra: UInt => UInt = a => (io.rs1Data.asSInt >> a).asUInt
    val or: UInt => UInt = a => io.rs1Data | a
    val and: UInt => UInt = a => io.rs1Data & a

    io.rdData := MuxLookup(
        funct3,
        io.rs1Data,
        Seq(
            ADD -> (MuxCase(
                add(imm_xlen_s), 
                Seq(
                    (opcode2 === 2.U(2.W)) -> add(io.rs2Data),
                    (opcode2 === 3.U(2.W)) -> add(-io.rs2Data),
            ))),
            SLL -> (MuxCase(
                sll(shamt),
                Seq(
                    (opcode2 === 2.U(2.W)) -> sll(io.rs2Data(5, 0)),
                )
            )),
            SLT -> (MuxCase(
                slt(imm_xlen_s),
                Seq(
                    (opcode2 === 2.U(2.W)) -> slt(io.rs2Data),
                )
            )),
            SLTU -> (MuxCase(
                sltu(imm_xlen_s),
                Seq(
                    (opcode2 === 2.U(2.W)) -> sltu(io.rs2Data),
                )
            )),
            XOR -> (MuxCase(
                xor(imm_xlen_s),
                Seq(
                    (opcode2 === 2.U(2.W)) -> xor(io.rs2Data),
                )
            )),
            SR -> (MuxLookup(
                opcode2,
                io.rs1Data,
                Seq(
                    SRLI -> srl(shamt),
                    SRAI -> sra(shamt),
                    SRL -> srl(io.rs2Data(4,0)),
                    SRA -> sra(io.rs2Data(4,0)),
                )
            )),
            OR -> (MuxCase(
                or(imm_xlen_s),
                Seq(
                    (opcode2 === 2.U(2.W)) -> or(io.rs2Data),
                )
            )),
            AND -> (MuxCase(
                and(imm_xlen_s),
                Seq(
                    (opcode2 === 2.U(2.W)) -> and(io.rs2Data),
                )
            )),
        )
    )
}
