import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import BranchType._

class BasicBrancherEQNETest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "branch if both inputs are 0" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(EQ)

            r.io.rs1.poke(0.U)
            r.io.rs2.poke(0.U)
            r.io.br_assert.expect(1.B)
        }
    }
    it should "branch if both inputs are equal and non-zero" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(EQ)

            r.io.rs1.poke(897.U)
            r.io.rs2.poke(897.U)
            r.io.br_assert.expect(1.B)
        }
    }
    it should "not branch if inputs are not equal" in {        
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(EQ)

            r.io.rs1.poke(23.U)
            r.io.rs2.poke(123.U)
            r.io.br_assert.expect(0.B)
        }
    }
    it should "not branch if both inputs are 0" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(NE)

            r.io.rs1.poke(0.U)
            r.io.rs2.poke(0.U)
            r.io.br_assert.expect(0.B)
        }
    }
    it should "not branch if both inputs are equal and non-zero" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(NE)

            r.io.rs1.poke(897.U)
            r.io.rs2.poke(897.U)
            r.io.br_assert.expect(0.B)
        }
    }
    it should "branch if inputs are not equal" in {        
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(NE)

            r.io.rs1.poke(23.U)
            r.io.rs2.poke(123.U)
            r.io.br_assert.expect(1.B)
        }
    }
}
class BasicBrancherLTGEUTest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "branch if rs1 < rs2" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(LTU)

            r.io.rs1.poke(0.U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(1.B)
        }
    }
    it should "not branch if rs1 >= rs2" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(LTU)

            r.io.rs1.poke(1.U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(0.B)

            r.io.rs1.poke(8000.U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(0.B)
        }
    }
    it should "not branch if rs1 < rs2" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(GEU)

            r.io.rs1.poke(0.U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(0.B)
        }
    }
    it should "branch if rs1 >= rs2" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(GEU)

            r.io.rs1.poke(1.U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(1.B)

            r.io.rs1.poke(8000.U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(1.B)

            r.io.rs1.poke("hffff_ffff".U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(1.B)
        }
    }
}
class BasicBrancherLTGESTest extends AnyFlatSpec with ChiselScalatestTester {
    val xlen = 32 // Width of register in bits
    it should "branch if rs1 < rs2 (signed)" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(LT)

            r.io.rs1.poke(0.U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(1.B)

            r.io.rs1.poke("hffff_fffe".U)
            r.io.rs2.poke("hffff_ffff".U)
            r.io.br_assert.expect(1.B)

            r.io.rs1.poke("hffff_ffff".U)
            r.io.rs2.poke(0.U)
            r.io.br_assert.expect(1.B)
        }
    }
    it should "branch if rs1 >= rs2 (signed)" in {
        test(new BrancherGenSimple(xlen)) { r =>
            r.io.sel.poke(GE)

            r.io.rs1.poke(1.U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(1.B)

            r.io.rs1.poke(8000.U)
            r.io.rs2.poke(1.U)
            r.io.br_assert.expect(1.B)

            r.io.rs1.poke("hffff_ffff".U)
            r.io.rs2.poke(0.U)
            r.io.br_assert.expect(0.B)
        }
    }
}
