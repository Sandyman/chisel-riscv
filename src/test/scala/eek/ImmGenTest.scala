package eek

import chisel3._
import chisel3.testers._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import ImmType._

class IImmGenTester(immgen: => ImmGen, count: => Int, sel: => UInt) extends BasicTester {
    val dut = Module(immgen)
    val xlen = dut.xlen

    val rnd = new scala.util.Random

    val (cntr, done) = Counter(true.B, count)

    // Create a bunch of random values starting with 0
    val initialSequence = Seq(0)
    val randomNumbers = Seq.fill(count)(rnd.nextInt())
    val finalSequence = initialSequence ++ randomNumbers

    val i = VecInit(finalSequence.map(x => (x.asSInt).asUInt))

    def imm_i(x: UInt) = { Cat(Fill(21, x(31)), x(30, 20)) }
    def imm_u(x: UInt) = { Cat(x(31, 12), Fill(12, 0.B)) }
    def imm_s(x: UInt) = { Cat(Fill(21, x(31)), x(30, 25), x(11, 7)) }

    val out = MuxLookup(
        sel,
        imm_i(i(cntr)),
        Seq(
            IImm -> imm_i(i(cntr)),
            UImm -> imm_u(i(cntr)),
            SImm -> imm_s(i(cntr)),
        )
    )

    dut.io.sel := sel
    dut.io.inst := i(cntr)

    when(done) { stop() }
    assert(dut.io.imm === out)
    printf("Counter: %d, i: 0x%x, Out: %x ?= %x\n", cntr, i(cntr), dut.io.imm, out)
}

class ImmGenTests extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32
    val count = 50
    "I-type immediates" should "pass" in {
        test(new IImmGenTester(new ImmGenSimple(xlen), count, IImm)).runUntilStop()
    }
    "U-type immediates" should "pass" in {
        test(new IImmGenTester(new ImmGenSimple(xlen), count, UImm)).runUntilStop()
    }
    "S-type immediates" should "pass" in {
        test(new IImmGenTester(new ImmGenSimple(xlen), count, SImm)).runUntilStop()
    }
}
class BasicBImmGenTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "(B-type) create a 0 from 0" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(BImm)

            // value 0
            r.io.inst.poke(0.U)
            r.io.imm.expect(0.U)
        }
    }
    it should "(B-type) create a positive UInt from a positive value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(BImm)

            // only bit 1 set => imm 2
            r.io.inst.poke(1 << 8)
            r.io.imm.expect(2)
        }
    }
    it should "(B-type) create the biggest positive number" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(BImm)

            // biggest positive value (doubled)
            r.io.inst.poke(0x3f << 25 | 0x1f << 7)
            r.io.imm.expect(((1 << 11) - 1) << 1)
        }
    }
    it should "(B-type) create a small negative number" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(BImm)

            // small negative value (doubled)
            r.io.inst.poke("hfe00_0f80".U)
            r.io.imm.expect("hffff_fffe".U)
        }
    }
    it should "(B-type) create the biggest negative number" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(BImm)

            // biggest negative number (doubled)
            r.io.inst.poke("h8000_0000".U)
            r.io.imm.expect("hffff_f000".U)
        }
    }
}
class BasicJImmGenTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "(J-type) create a 0 from 0" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(JImm)

            // value 0
            r.io.inst.poke(0.U)
            r.io.imm.expect(0.U)
        }
    }
    it should "(J-type) not be changed by non-immediate bits" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(JImm)

            // value 0
            r.io.inst.poke((1 << 12) - 1)
            r.io.imm.expect(0.U)
        }
    }
    it should "(J-type) create a small positive value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(JImm)

            // value 1 (doubled)
            r.io.inst.poke(1 << 21)
            r.io.imm.expect(2.U)
        }
    }
    it should "(J-type) create the biggest positive value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(JImm)

            // value 1 (doubled)
            r.io.inst.poke(0x7fff_f000)
            r.io.imm.expect(0x7ffff << 1)
        }
    }
    it should "(J-type) create a small negative value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(JImm)

            // value -1 (doubled)
            r.io.inst.poke("hffff_f000".U)
            r.io.imm.expect("hffff_fffe".U)
        }
    }
    it should "(J-type) create the biggest negative value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(JImm)

            r.io.inst.poke("h8000_0000".U)
            r.io.imm.expect("hfff0_0000".U)
        }
    }
}
