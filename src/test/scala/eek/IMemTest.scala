package eek

import chisel3._
import chisel3.testers._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class IMemTester(imem: => IMem) extends BasicTester with TestUtils {
    val dut = Module(imem)
    val xlen = dut.xlen

    val NUL = 0x00.U(xlen.W)
    val NOP = 0x33.U(xlen.W)

    dut.io.pc := 0.U(xlen.W)

    stop()
    assert(dut.io.inst === NUL)
    printf("PC: %x, inst=%x\n", dut.io.pc, dut.io.inst);
}
class IMemTests extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32
    val size = 128
    it should "pass" in {
        test(new IMemTester(new SimpleInstructionMemory(size, xlen))).runUntilStop()
    }
}
