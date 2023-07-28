package eek

import chisel3._
import chisel3.testers._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Operation._

class AluGenTester(brgen: => AluGen, count: => Int, oper: => UInt) extends BasicTester with TestUtils {
    val dut = Module(brgen)
    val xlen = dut.xlen

    val rnd = new scala.util.Random

    val (cntr, done) = Counter(true.B, count)

    // Create a bunch of random values
    val rs1Values = (Seq.fill(count)(rnd.nextInt())).map(toBigInt)
    val rs2Values = (Seq.fill(count)(rnd.nextInt())).map(toBigInt)

    val rs1 = VecInit(rs1Values.map { x => ((x.toInt).asSInt).asUInt })
    val rs2 = VecInit(rs2Values.map { x => ((x.toInt).asSInt).asUInt })

    def add(x: UInt, y: UInt): UInt = { (x + y) & "hffff_ffff".U }
    def sub(x: UInt, y: UInt): UInt = { (x - y) & "hffff_ffff".U }
    def sll(x: UInt, y: UInt): UInt = { (x << y(4, 0)) & "hffff_ffff".U }
    def slt(x: UInt, y: UInt): UInt = { Mux(x.asSInt < y.asSInt, 1.U, 0.U) }

    val out = MuxLookup(
        oper,
        add(rs1(0), rs2(0)),
        Seq(
            ADD -> add(rs1(cntr), rs2(cntr)),
            SUB -> sub(rs1(cntr), rs2(cntr)),
            SLL -> sll(rs1(cntr), rs2(cntr)),
            SLT -> slt(rs1(cntr), rs2(cntr)),
        )
    )

    dut.io.rs1Data := rs1(cntr).asUInt
    dut.io.rs2Data := rs2(cntr).asUInt
    dut.io.oper := oper

    when(done) { stop() }
    assert(dut.io.rdData === out)
    printf("Counter: %d, rs1: %x, rs2: %x, sel = %x, rd: %x ?= %x\n", cntr, rs1(cntr), rs2(cntr), oper, dut.io.rdData, out)
}
class AluGenTests extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32
    val count = 50
    "ALU ADD" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, ADD)).runUntilStop()
    }
    "ALU SUB" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, SUB)).runUntilStop()
    }
    "ALU SLL" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, SLL)).runUntilStop()
    }
    "ALU SLT" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, SLT)).runUntilStop()
    }
}


class BasicALUSLTTest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(SLT)
            r.io.rs1Data.poke(13)
            r.io.rs2Data.poke(15)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke(27)
            r.io.rdData.expect(0)

            r.io.rs2Data.poke(27)
            r.io.rdData.expect(0)
        }
    }
    it should "compare negative numbers" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(SLT)

            // -3 < -2 => True
            r.io.rs1Data.poke("hffff_fffd".U)
            r.io.rs2Data.poke("hffff_fffe".U)
            r.io.rdData.expect(1)

            // -2 < -1 => False
            r.io.rs1Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0)

            // -1 < 0 => True
            r.io.rs2Data.poke(0)
            r.io.rdData.expect(1)
        }
    }
}
class BasicALUSLTUTest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(SLTU)

            // 13 < 15 => True
            r.io.rs1Data.poke(13)
            r.io.rs2Data.poke(15)
            r.io.rdData.expect(1)

            // 27 < 15 => False
            r.io.rs1Data.poke(27)
            r.io.rdData.expect(0)

            // 27 < 27 => False
            r.io.rs2Data.poke(27)
            r.io.rdData.expect(0)

            // (2^32 - 1) < 27 => False
            r.io.rs1Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0)
        }
    }
}
class BasicALUXORTest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "do XOR correctly" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(XOR)

            // 0xaa ^ 0xff => 0x55
            r.io.rs1Data.poke("haaaa_aaaa".U)
            r.io.rs2Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0x5555_5555)

            // 0xff ^ 0xff => 0x00
            r.io.rs1Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0)
        }
    }
}
class BasicALUSRLTest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "half value when shifted to right by 1" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(SRL)
            r.io.rs1Data.poke(26)
            r.io.rs2Data.poke(1)
            r.io.rdData.expect(13)
        }
    }
    it should "become zero when shifted too far" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(SRL)
            r.io.rs1Data.poke(12)
            r.io.rs2Data.poke(5)
            r.io.rdData.expect(0)
        }
    }
    it should "not sign extend" in {
        test(new AluSimple(xlen)) { r=>
            r.io.oper.poke(SRL)
            r.io.rs1Data.poke("hf842_8421".U)
            r.io.rs2Data.poke(4)
            r.io.rdData.expect("h0f84_2842".U)
        }
    }
}
class BasicALUSRATest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "half value when shifted to right by 1" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(SRA)
            r.io.rs1Data.poke(26)
            r.io.rs2Data.poke(1)
            r.io.rdData.expect(13)
        }
    }
    it should "become zero when shifted too far" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(SRA)
            r.io.rs1Data.poke(12)
            r.io.rs2Data.poke(5)
            r.io.rdData.expect(0)
        }
    }
    it should "sign extend a negative number" in {
        test(new AluSimple(xlen)) { r=>
            r.io.oper.poke(SRA)
            r.io.rs1Data.poke("hf842_8421".U)
            r.io.rs2Data.poke(4)
            r.io.rdData.expect("hff84_2842".U)
        }
    }
}
class BasicALUORTest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "do OR correctly" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(OR)

            // 0xaa | 0xff => 0xff
            r.io.rs1Data.poke("haaaa_aaaa".U)
            r.io.rs2Data.poke("hffff_ffff".U)
            r.io.rdData.expect("hffff_ffff".U)

            // 0x0105_0a0a | 0x0000_0555 => 0x0105_0f5f
            r.io.rs1Data.poke("h0105_0a0a".U)
            r.io.rs2Data.poke("h0000_0555".U)
            r.io.rdData.expect(0x0105_0f5f)
        }
    }
}
class BasicALUANDTest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "do AND correctly" in {
        test(new AluSimple(xlen)) { r =>
            r.io.oper.poke(AND)

            // 0xaa & 0xff => 0xaa
            r.io.rs1Data.poke("haaaa_aaaa".U)
            r.io.rs2Data.poke("hffff_ffff".U)
            r.io.rdData.expect("haaaa_aaaa".U)

            // 0x0105_0faf & 0x0000_0555 => 0x0000_0505
            r.io.rs1Data.poke("h0105_0faf".U)
            r.io.rs2Data.poke("h0000_0555".U)
            r.io.rdData.expect(0x0000_0505)
        }
    }
}
