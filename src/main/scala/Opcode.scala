import chisel3._
import chisel3.util._
import chisel3.experimental._

object Opcode {
    val LUI      = "b0110111".U,
    val AUIPC    = "b0010111".U,
    val JAL      = "b1101111".U,
    val JALR     = "b1100111".U,
    val BRANCH   = "b1100011".U,
    val LOAD     = "b0000011".U,
    val STORE    = "b0100011".U,
    val OPIMM    = "b0010011".U,
    val OP       = "b0110011".U,
    val MISCMEM  = "b0001111".U,
    val SYSTEM   = "b1110011".U,
}
