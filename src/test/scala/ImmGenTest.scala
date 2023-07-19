import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import ImmType._

class BasicImmGenTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "build UInt of value 0 from 12-bit 0 value" in {
        test(new ImmGenSimple(xlen)) { r => 
            r.io.sel.poke(IImm)
            r.io.inst.poke(0 << 20)
            r.io.imm.expect(0)
        }
    }
    it should "build UInts from 12-bit a positive value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(IImm)
            r.io.inst.poke(12 << 20)
            r.io.imm.expect(12)
        }
    }
    it should "build UInts from the biggest positive value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(IImm)
            r.io.inst.poke(0x7ff << 20)
            r.io.imm.expect(0x7ff)
        }
    }
    it should "build UInt from a negative 12-bit value" in {
        test(new ImmGenSimple(xlen)) { r => 
            r.io.sel.poke(IImm)
            r.io.inst.poke("h8000_0000".U)
            r.io.imm.expect("hffff_f800".U)
        }
    }
    it should "built UInt of value -1 from 12-bit -1 value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(IImm)
            r.io.inst.poke("hfff0_0000".U)
            r.io.imm.expect("hffff_ffff".U)
        }
    }
}
