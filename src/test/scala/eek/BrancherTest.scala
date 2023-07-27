package eek

import chisel3._
import chisel3.testers._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import BranchType._

class BrancherTester(brgen: => BrancherGen, count: => Int, sel: => UInt) extends BasicTester {
    val dut = Module(brgen)
    val xlen = dut.xlen

    val rnd = new scala.util.Random

    assert(count > 0, "Count must be at least 1.")

    val (cntr, done) = Counter(true.B, count)

    // Create a bunch of random values starting with 0
    val rs1Values = Seq(0, 1, 2, 3) ++ Seq.fill(count - 4)(rnd.nextInt())
    val rs2Values = Seq(0, 1, 2, 3) ++ Seq.fill(count - 4)(rnd.nextInt())

    val eq = VecInit(rs1Values.zip(rs2Values).map { case (a, b) =>
        (if (a == b) 1 else 0 ).U(xlen.W)
    })
    val ne = VecInit(rs1Values.zip(rs2Values).map { case (a, b) =>
        (if (a != b) 1 else 0 ).U(xlen.W)
    })
    val lt = VecInit(rs1Values.zip(rs2Values).map { case (a, b) =>
        (if (a < b ) 1 else 0 ).U(xlen.W)
    })
    val ge = VecInit(rs1Values.zip(rs2Values).map { case (a, b) =>
        (if (a >= b) 1 else 0 ).U(xlen.W)
    })

    val rs1 = VecInit(rs1Values.map { x => (x.asSInt).asUInt })(cntr)
    val rs2 = VecInit(rs2Values.map { x => (x.asSInt).asUInt })(cntr)

    dut.io.rs1 := rs1
    dut.io.rs2 := rs2
    dut.io.sel := sel

    val out = MuxLookup(
        sel,
        eq(0),
        Seq(
            EQ -> eq(cntr),
            NE -> ne(cntr),
            LT -> lt(cntr),
            GE -> ge(cntr),
        )
    )(0)

    when(done) { stop() }
    assert(dut.io.br_assert === out)
    printf("Counter: %d, rs1: 0x%x, rs2: 0x%x, sel: %x, Out: %x ?= %x\n", 
        cntr, rs1, rs2, sel, dut.io.br_assert, out)
}
class BrancherTests extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32
    val count = 50
    "Branch on equal" should "pass" in {
        test(new BrancherTester(new BrancherGenSimple(xlen), count, EQ)).runUntilStop()
    }
    "Branch on not equal" should "pass" in {
        test(new BrancherTester(new BrancherGenSimple(xlen), count, NE)).runUntilStop()
    }
    "Branch on less than" should "pass" in {
        test(new BrancherTester(new BrancherGenSimple(xlen), count, LT)).runUntilStop()
    }
    "Branch on greater than or equal" should "pass" in {
        test(new BrancherTester(new BrancherGenSimple(xlen), count, GE)).runUntilStop()
    }
}
