package eek

import chisel3._
import chisel3.util._
import chisel3.experimental._

object BranchType {
    val EQ     = 0.U(3.W)
    val NE     = 1.U(3.W)
    val LT     = 4.U(3.W)
    val GE     = 5.U(3.W)
    val LTU    = 6.U(3.W)
    val GEU    = 7.U(3.W)
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
        0.B,
        Seq(
            EQ -> eq,
            NE -> ne,
            LTU -> ltu,
            LT -> lt,
            GEU -> geu,
            GE -> ge,
        )
    )
}
