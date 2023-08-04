package eek

import chisel3._
import chisel3.stage._
import chisel3.util.log2Ceil

class CtrlIO(xlen: Int) extends Bundle {
    val inst = Input(UInt(xlen.W))
}

trait Ctrl extends Module {
    def xlen: Int
    val io: CtrlIO
}

class SimpleCtrl(val xlen: Int) extends Ctrl {
    val io = IO(new CtrlIO(xlen))
}

object CtrlDriver extends App {
    val xlen = 32
    (new ChiselStage).emitVerilog(new SimpleCtrl(xlen), args)
}
