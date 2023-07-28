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
    def sltu(x: UInt, y: UInt): UInt = { Mux(x.asUInt < y.asUInt, 1.U, 0.U) }
    def xor(x: UInt, y: UInt): UInt = { (x ^ y) & "hffff_ffff".U }
    def srl(x: UInt, y: UInt): UInt = { (x >> y(4, 0)) & "hffff_ffff".U }
    def sra(x: UInt, y: UInt): UInt = { (x.asSInt >> y(4, 0)).asUInt & "hffff_ffff".U }

    val out = MuxLookup(
        oper,
        add(rs1(0), rs2(0)),
        Seq(
            ADD -> add(rs1(cntr), rs2(cntr)),
            SUB -> sub(rs1(cntr), rs2(cntr)),
            SLL -> sll(rs1(cntr), rs2(cntr)),
            SLT -> slt(rs1(cntr), rs2(cntr)),
            SLTU -> sltu(rs1(cntr), rs2(cntr)),
            XOR -> xor(rs1(cntr), rs2(cntr)),
            SRL -> srl(rs1(cntr), rs2(cntr)),
            SRA -> sra(rs1(cntr), rs2(cntr)),
        )
    )

    dut.io.rs1Data := rs1(cntr).asUInt
    dut.io.rs2Data := rs2(cntr).asUInt
    dut.io.oper := oper

    when(done) { stop() }
    assert(dut.io.rdData === out)
    printf("Counter: %d, rs1: %x, rs2: %x, sel = %x, rd: %x ?= %x\n", cntr, rs1(cntr), rs2(cntr), oper, dut.io.rdData, out(31, 0))
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
    "ALU SLTU" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, SLTU)).runUntilStop()
    }
    "ALU XOR" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, XOR)).runUntilStop()
    }
    "ALU SRL" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, SRL)).runUntilStop()
    }
    "ALU SRA" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, SRA)).runUntilStop()
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
