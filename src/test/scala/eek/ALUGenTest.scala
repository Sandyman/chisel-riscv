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

    val rs1Data = VecInit(rs1Values.map { x => ((x.toInt).asSInt).asUInt })
    val rs2Data = VecInit(rs2Values.map { x => ((x.toInt).asSInt).asUInt })

    def add(x: UInt, y: UInt): UInt = { (x + y) & "hffff_ffff".U }
    def sub(x: UInt, y: UInt): UInt = { (x - y) & "hffff_ffff".U }
    def sll(x: UInt, y: UInt): UInt = { (x << y(4, 0)) & "hffff_ffff".U }
    def slt(x: UInt, y: UInt): UInt = { Mux(x.asSInt < y.asSInt, 1.U, 0.U) }
    def sltu(x: UInt, y: UInt): UInt = { Mux(x.asUInt < y.asUInt, 1.U, 0.U) }
    def xor(x: UInt, y: UInt): UInt = { (x ^ y) & "hffff_ffff".U }
    def srl(x: UInt, y: UInt): UInt = { (x >> y(4, 0)) & "hffff_ffff".U }
    def sra(x: UInt, y: UInt): UInt = { (x.asSInt >> y(4, 0)).asUInt & "hffff_ffff".U }
    def or(x: UInt, y: UInt): UInt = { (x | y) & "hffff_ffff".U }
    def and(x: UInt, y: UInt): UInt = { (x & y) & "hffff_ffff".U }

    val rs1 = rs1Data(cntr)
    val rs2 = rs2Data(cntr)

    val out = MuxLookup(
        oper,
        add(rs1(0), rs2(0)),
        Seq(
            OP_ADD -> add(rs1, rs2),
            OP_SUB -> sub(rs1, rs2),
            OP_SLL -> sll(rs1, rs2),
            OP_SLT -> slt(rs1, rs2),
            OP_SLTU -> sltu(rs1, rs2),
            OP_XOR -> xor(rs1, rs2),
            OP_SRL -> srl(rs1, rs2),
            OP_SRA -> sra(rs1, rs2),
            OP_OR -> or(rs1, rs2),
            OP_AND -> and(rs1, rs2),
        )
    )

    dut.io.rs1Data := rs1.asUInt
    dut.io.rs2Data := rs2.asUInt
    dut.io.oper := oper

    when(done) { stop() }
    assert(dut.io.rdData === out)
    printf("Counter: %d, rs1: %x, rs2: %x, sel = %x, rd: %x ?= %x\n", cntr, rs1, rs2, oper, dut.io.rdData, out(31, 0))
}
class AluGenTests extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32
    val count = 50
    "ALU ADD" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_ADD)).runUntilStop()
    }
    "ALU SUB" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_SUB)).runUntilStop()
    }
    "ALU SLL" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_SLL)).runUntilStop()
    }
    "ALU SLT" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_SLT)).runUntilStop()
    }
    "ALU SLTU" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_SLTU)).runUntilStop()
    }
    "ALU XOR" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_XOR)).runUntilStop()
    }
    "ALU SRL" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_SRL)).runUntilStop()
    }
    "ALU SRA" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_SRA)).runUntilStop()
    }
    "ALU OR" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_OR)).runUntilStop()
    }
    "ALU AND" should "pass" in {
        test(new AluGenTester(new AluSimple(xlen), count, OP_AND)).runUntilStop()
    }
}
