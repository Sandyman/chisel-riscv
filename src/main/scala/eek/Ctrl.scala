package eek

import chisel3._
import chisel3.stage._
import chisel3.util._

object Signal {
    val N    = 0.U(1.W)
    val Y    = 1.U(1.W)
}

object AluAMux {
    val A_MUX_RS1  = 0.U(1.W)
    val A_MUX_PC   = 1.U(1.W)
    val A_MUX_N    = 0.U(1.W)
}

object AluBMux {
    val B_MUX_RS2  = 0.U(1.W)
    val B_MUX_IMM  = 1.U(1.W)
    val B_MUX_N    = 0.U(1.W)
}

object RegMux {
    val REG_MUX_ALU_Y = 0.U(2.W)
    val REG_MUX_PC_4  = 1.U(2.W)
    val REG_MUX_D_OUT = 2.U(2.W)
    val REG_MUX_N     = 0.U(2.W)
}

object PCMux {
    val PC_MUX_PC_4         = 0.U(2.W)
    val PC_MUX_PC_OFFSET_RS = 1.U(2.W)
    val PC_MUX_PC_OFFSET    = 2.U(2.W)
    val PC_MUX_X            = 0.U(2.W)
}

object RegRWEnable {
    val REG_WEN_0   = false.B
    val REG_WEN_1   = true.B
}

object MemRWEnable {
    val MEM_WEN_0   = false.B
    val MEM_WEN_1   = true.B
    val MEM_REN_0   = false.B
    val MEM_REN_1   = true.B
}

object BrTaken {
    val BR_NOT_TAKEN = false.B
    val BR_TAKEN     = true.B
}

import Signal._
import AluAMux._
import AluBMux._
import RegMux._
import PCMux._
import RegRWEnable._
import MemRWEnable._
import BrTaken._
import Operation._
import BranchType._
import ImmType._
import Instructions._

class CtrlIO(xlen: Int) extends Bundle {
    val inst = Input(UInt(xlen.W))
    val alu_a_mux = Output(UInt(1.W))
    val alu_b_mux = Output(UInt(1.W))
    val reg_mux = Output(UInt(2.W))
    val pc_mux = Output(UInt(2.W))
    val reg_w_en = Output(UInt(1.W))
    val mem_w_en = Output(UInt(1.W))
    val mem_r_en = Output(UInt(1.W))
    val br_type = Output(UInt(3.W))
    val br_taken = Input(UInt(1.W))
    val alu_oper = Output(UInt(4.W))
}

trait Ctrl extends Module {
    def xlen: Int
    val io: CtrlIO
}

class SimpleCtrl(val xlen: Int) extends Ctrl {
    val io = IO(new CtrlIO(xlen))
    io := DontCare

    val ctrl_signals =
        ListLookup(io.inst,
                          List(N, A_MUX_N,   B_MUX_N,   OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_N,     PC_MUX_PC_4,         BR_N),
            Array(
                // I-type instructions
                ADDI   -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SLTI   -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SLT,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SLTIU  -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SLTU, REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                XORI   -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_XOR,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                ORI    -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_OR,   REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                ANDI   -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_AND,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SRLI   -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SRL,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SLLI   -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SLL,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SRAI   -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SRA,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),

                // R-type instructions
                ADD    -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SUB    -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SUB,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SLL    -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SLL,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SLT    -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SLT,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SLTU   -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SLTU, REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                XOR    -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_XOR,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SRL    -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SRL,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                SRA    -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SRA,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                OR     -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_OR,   REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                AND    -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_AND,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),

                // B-type instructions
                BEQ    -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_EQ),
                BNE    -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NE),
                BLTU   -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_LTU),
                BLT    -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_LT),
                BGEU   -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_GEU),
                BGE    -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_GE),

                // U-type instructions
                LUI    -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_COPY, REG_WEN_1, MEM_WEN_0, MEM_REN_0, UImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),
                AUIPC  -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, UImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_N),

                // I/J-type instructions
                JAL    -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, JImm, REG_MUX_PC_4,  PC_MUX_PC_OFFSET,    BR_N),
                JALR   -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_PC_4,  PC_MUX_PC_OFFSET_RS, BR_N),

                // Load instructions
                LB     -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4,  PC_MUX_PC_4,         BR_N),
                LH     -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4,  PC_MUX_PC_4,         BR_N),
                LW     -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4,  PC_MUX_PC_4,         BR_N),
                LBU    -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4,  PC_MUX_PC_4,         BR_N),
                LHU    -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4,  PC_MUX_PC_4,         BR_N),

                // Store instructions
                SB     -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_0, MEM_WEN_1, MEM_REN_0, SImm, REG_MUX_PC_4,  PC_MUX_PC_4,         BR_N),
                SH     -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_0, MEM_WEN_1, MEM_REN_0, SImm, REG_MUX_PC_4,  PC_MUX_PC_4,         BR_N),
                SW     -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_0, MEM_WEN_1, MEM_REN_0, SImm, REG_MUX_PC_4,  PC_MUX_PC_4,         BR_N),

                // System instructions
                FENCE  -> List(Y, A_MUX_N,   B_MUX_N,   OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_N,     PC_MUX_PC_4,         BR_N),
                ECALL  -> List(Y, A_MUX_N,   B_MUX_N,   OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_N,     PC_MUX_PC_4,         BR_N),
                EBREAK -> List(Y, A_MUX_N,   B_MUX_N,   OP_N,    REG_WEN_0, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_N,     PC_MUX_PC_4,         BR_N),
            )
        )
}

object CtrlDriver extends App {
    val xlen = 32
    (new ChiselStage).emitVerilog(new SimpleCtrl(xlen), args)
}
