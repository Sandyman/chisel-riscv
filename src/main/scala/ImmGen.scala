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

    val imm_i: UInt => UInt = a => Cat(Fill(21, a(31)), a(30, 20))
    val imm_s: UInt => UInt = a => Cat(Fill(21, a(31)), a(30, 25), a(11, 7))
    val imm_b: UInt => UInt = a => Cat(Fill(20, a(31)), a(7), a(30, 25), a(11, 8), 0.B)
    val imm_u: UInt => UInt = a => Cat(a(31, 12), Fill(12, 0.B))
    val imm_j: UInt => UInt = a => Cat(Fill(12, a(31)), a(19, 12), a(20), a(30, 21), 0.B) 

    io.imm := MuxLookup(
        0.U,
        io.sel,
        Seq(
            IImm -> imm_i(io.inst),
            SImm -> imm_s(io.inst),
            BImm -> imm_b(io.inst),
            UImm -> imm_u(io.inst),
            JImm -> imm_j(io.inst),
        )
    )
}
