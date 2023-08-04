package eek

import chisel3._
import chisel3.stage._
import chisel3.util._
import chisel3.experimental._

object Operation {
    val OP_X      =  0.U(4.W)
    val OP_ADD    =  1.U(4.W)
    val OP_SUB    =  2.U(4.W)
    val OP_SLL    =  3.U(4.W)
    val OP_SLT    =  4.U(4.W)
    val OP_SLTU   =  5.U(4.W)
    val OP_XOR    =  6.U(4.W)
    val OP_SRL    =  7.U(4.W)
    val OP_SRA    =  8.U(4.W)
    val OP_OR     =  9.U(4.W)
    val OP_AND    = 10.U(4.W)
    val OP_COPY   = 11.U(4.W)
}

import Operation._

class AluIO(xlen: Int) extends Bundle {
    val oper = Input(UInt(xlen.W))
    val aData = Input(UInt(xlen.W))
    val bData = Input(UInt(xlen.W))
    val yData  = Output(UInt(xlen.W))
}

trait AluGen extends Module {
    def xlen: Int
    val io: AluIO
}

class AluSimple(val xlen: Int) extends AluGen {
    val io = IO(new AluIO(xlen))

    val add: UInt => UInt = x => io.aData + x
    val sll: UInt => UInt = x => {
        // Make sure we use as little bits as needed. This is necessary
        // as {dshl} widens the target variable by as many bits as needed
        // to perform the left shift. - FIRRTL Spec., Version 0.2.0, 7.13
        val shft = Wire(UInt(xlen.W))
        shft := io.aData << x(4, 0)
        shft
    }
    val slt: UInt => UInt = x => Mux(io.aData.asSInt < x.asSInt, 1.U, 0.U)
    val sltu: UInt => UInt = x => Mux(io.aData < x, 1.U, 0.U)
    val xor: UInt => UInt = x => io.aData ^ x
    val srl: UInt => UInt = x => io.aData >> x(4, 0)
    val sra: UInt => UInt = x => (io.aData.asSInt >> x(4, 0)).asUInt
    val or: UInt => UInt = x => io.aData | x
    val and: UInt => UInt = x => io.aData & x

    io.yData := MuxLookup(
        io.oper,
        0.U(xlen.W),
        Seq(
            OP_ADD  -> add(io.bData),
            OP_SUB  -> add(-io.bData),
            OP_SLL  -> sll(io.bData),
            OP_SLT  -> slt(io.bData),
            OP_SLTU -> sltu(io.bData),
            OP_XOR  -> xor(io.bData),
            OP_SRL  -> srl(io.bData),
            OP_SRA  -> sra(io.bData),
            OP_OR   -> or(io.bData),
            OP_AND  -> and(io.bData),
            OP_COPY -> io.bData,
        )
    )
}

object AluDriver extends App {
    val xlen = 32
    (new ChiselStage).emitVerilog(new AluSimple(xlen), args)
}
