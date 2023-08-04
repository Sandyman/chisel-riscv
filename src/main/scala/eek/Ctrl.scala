package eek

import chisel3._
import chisel3.stage._
import chisel3.util._

object Signal {
    val N    = 0.U(1.W)
    val Y    = 1.U(1.W)
}

import Signal._

object AluAMux {
    val A_MUX_RS1  = 0.U(2.W)
    val A_MUX_PC   = 1.U(2.W)
    val A_MUX_Z    = 2.U(2.W)
}

import AluAMux._

object AluBMux {
    val B_MUX_RS2  = 0.U(1.W)
    val B_MUX_IMM  = 1.U(1.W)
}

import AluBMux._

object RegMux {
    val REG_MUX_ALU_Y = 0.U(2.W)
    val REG_MUX_PC_4  = 1.U(2.W)
    val REG_MUX_D_OUT = 2.U(2.W)
}

import RegMux._

object PCMux {
    val PC_MUX_PC_4         = 0.U(2.W)
    val PC_MUX_PC_OFFSET_RS = 1.U(2.W)
    val PC_MUX_PC_OFFSET    = 2.U(2.W)
}

import PCMux._

object RegRWEnable {
    val REG_WEN_0   = 0.U(1.W)
    val REG_WEN_1   = 1.U(1.W)
}

import RegRWEnable._

object MemRWEnable {
    val MEM_WEN_0   = 0.U(1.W)
    val MEM_WEN_1   = 1.U(1.W)
    val MEM_REN_0   = 0.U(1.W)
    val MEM_REN_1   = 1.U(1.W)
}

import MemRWEnable._

object BrTaken {
    val BR_NOT_TAKEN = 0.U(1.W)
    val BR_TAKEN     = 1.U(1.W)
}

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

    val ctrl_signals =
        ListLookup(io.inst,
                         List(N, A_MUX_RS1, B_MUX_RS2, OP_ADD,  REG_WEN_0, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
            Array(
                // I-type instructions
                ADDI  -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SLTI  -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SLT,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SLTIU -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SLTU, REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                XORI  -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_XOR,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                ORI   -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_OR,   REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                ANDI  -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_AND,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SRLI  -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SRL,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SLLI  -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SLL,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SRAI  -> List(Y, A_MUX_RS1, B_MUX_IMM, OP_SRA,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),

                // R-type instructions
                ADD   -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SUB   -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SUB,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SLL   -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SLL,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SLT   -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SLT,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SLTU  -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SLTU, REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                XOR   -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_XOR,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SRL   -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SRL,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                SRA   -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_SRA,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                OR    -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_OR,   REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                AND   -> List(Y, A_MUX_RS1, B_MUX_RS2, OP_AND,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),

                // B-type instructions
                BEQ   -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_EQ),
                BNE   -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NE),
                BLTU  -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_LTU),
                BLT   -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_LT),
                BGEU  -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_GEU),
                BGE   -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_0, MEM_WEN_0, MEM_REN_0, BImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_GE),

                // U-type instructions
                LUI   -> List(Y, A_MUX_Z,  B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, UImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),
                AUIPC -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, UImm, REG_MUX_ALU_Y, PC_MUX_PC_4,         BR_NONE),

                // I/J-type instructions
                JAL   -> List(Y, A_MUX_Z,  B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, JImm, REG_MUX_PC_4,  PC_MUX_PC_OFFSET,    BR_NONE),
                JALR  -> List(Y, A_MUX_PC, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_0, IImm, REG_MUX_PC_4,  PC_MUX_PC_OFFSET_RS, BR_NONE),

                // Load instructions
                LB    -> List(Y, A_MUX_RS1, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4, PC_MUX_PC_4,         BR_NONE),
                LH    -> List(Y, A_MUX_RS1, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4, PC_MUX_PC_4,         BR_NONE),
                LW    -> List(Y, A_MUX_RS1, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4, PC_MUX_PC_4,         BR_NONE),
                LBU   -> List(Y, A_MUX_RS1, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4, PC_MUX_PC_4,         BR_NONE),
                LHU   -> List(Y, A_MUX_RS1, B_MUX_IMM,  OP_ADD,  REG_WEN_1, MEM_WEN_0, MEM_REN_1, IImm, REG_MUX_PC_4, PC_MUX_PC_4,         BR_NONE),

                // Store instructions
                SB    -> List(Y, A_MUX_RS1, B_MUX_IMM,  OP_ADD,  REG_WEN_0, MEM_WEN_1, MEM_REN_0, SImm, REG_MUX_PC_4, PC_MUX_PC_4,         BR_NONE),
                SH    -> List(Y, A_MUX_RS1, B_MUX_IMM,  OP_ADD,  REG_WEN_0, MEM_WEN_1, MEM_REN_0, SImm, REG_MUX_PC_4, PC_MUX_PC_4,         BR_NONE),
                SW    -> List(Y, A_MUX_RS1, B_MUX_IMM,  OP_ADD,  REG_WEN_0, MEM_WEN_1, MEM_REN_0, SImm, REG_MUX_PC_4, PC_MUX_PC_4,         BR_NONE),
            )
        )
}

object CtrlDriver extends App {
    val xlen = 32
    (new ChiselStage).emitVerilog(new SimpleCtrl(xlen), args)
}
