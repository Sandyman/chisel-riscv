package eek

import chisel3._
import chisel3.stage._
import chisel3.util._
import chisel3.experimental._

object BranchType {
    val BR_N      = 0.U(3.W)
    val BR_EQ     = 2.U(3.W)
    val BR_NE     = 3.U(3.W)
    val BR_LT     = 4.U(3.W)
    val BR_GE     = 5.U(3.W)
    val BR_LTU    = 6.U(3.W)
    val BR_GEU    = 7.U(3.W)
}

import BranchType._

class BrancherIO(xlen: Int) extends Bundle {
    val rs1       = Input(UInt(xlen.W))
    val rs2       = Input(UInt(xlen.W))
    val sel       = Input(UInt(3.W))
    val br_assert = Output(UInt(1.W))
}

trait BrancherGen extends Module {
    def xlen: Int
    val io: BrancherIO
}

class BrancherGenSimple(val xlen: Int) extends BrancherGen {
    val io = IO(new BrancherIO(xlen))

    val eq = io.rs1 === io.rs2
    val ne = ~eq
    val ltu = io.rs1 < io.rs2
    val geu = ~ltu
    val lt = io.rs1.asSInt < io.rs2.asSInt
    val ge = ~lt

    io.br_assert := MuxLookup(
        io.sel,
        false.B,
        Seq(
            BR_EQ -> eq,
            BR_NE -> ne,
            BR_LTU -> ltu,
            BR_LT -> lt,
            BR_GEU -> geu,
            BR_GE -> ge,
        )
    )
}

object BrancherDriver extends App {
    val xlen = 32
    (new ChiselStage).emitVerilog(new BrancherGenSimple(xlen), args)
}
