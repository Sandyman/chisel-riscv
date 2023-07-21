import chisel3._
import chisel3.util._
import chisel3.experimental._

object ImmType {
    val IImm     = 0.U(3.W)
    val SImm     = 1.U(3.W)
    val BImm     = 2.U(3.W)
    val UImm     = 3.U(3.W)
    val JImm     = 4.U(3.W)
}

import ImmType._

class ImmGenIO(xlen: Int) extends Bundle {
    val inst = Input(UInt(xlen.W))
    val sel = Input(UInt(3.W))
    val imm = Output(UInt(xlen.W))
}

trait ImmGen extends Module {
    def xlen: Int
    val io: ImmGenIO
}

class ImmGenSimple (val xlen: Int) extends ImmGen {
    val io = IO(new ImmGenIO(xlen))

    val imm_i = Cat(Fill(21, io.inst(31)), io.inst(30, 20))
    val imm_s = Cat(Fill(21, io.inst(31)), io.inst(30, 25), io.inst(11, 7))
    val imm_b = Cat(Fill(20, io.inst(31)), io.inst(7), io.inst(30, 25), io.inst(11, 8), 0.B)
    val imm_u = Cat(io.inst(31, 12), Fill(12, 0.B))
    val imm_j = Cat(Fill(12, io.inst(31)), io.inst(19, 12), io.inst(20), io.inst(30, 21), 0.B)

    io.imm := MuxLookup(
        io.sel,
        0.U,
        Seq(
            IImm -> imm_i,
            SImm -> imm_s,
            BImm -> imm_b,
            UImm -> imm_u,
            JImm -> imm_j,
        )
    )
}
