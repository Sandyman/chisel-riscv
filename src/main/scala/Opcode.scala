import chisel3._
import chisel3.util._
import chisel3.experimental._

object Opcode {
    val LUI      = "b0110111".U(7.W)
    val AUIPC    = "b0010111".U(7.W)
    val JAL      = "b1101111".U(7.W)
    val JALR     = "b1100111".U(7.W)
    val BRANCH   = "b1100011".U(7.W)
    val LOAD     = "b0000011".U(7.W)
    val STORE    = "b0100011".U(7.W)
    val OPIMM    = "b0010011".U(7.W)
    val OP       = "b0110011".U(7.W)
    val MISCMEM  = "b0001111".U(7.W)
    val SYSTEM   = "b1110011".U(7.W)
}
