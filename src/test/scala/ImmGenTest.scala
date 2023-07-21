import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import ImmType._

class BasicIImmGenTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "(I-Type) build UInt of value 0 from 12-bit 0 value" in {
        test(new ImmGenSimple(xlen)) { r => 
            r.io.sel.poke(IImm)
            r.io.inst.poke(0 << 20)
            r.io.imm.expect(0)
        }
    }
    it should "(I-Type) build UInt from 12-bit a positive value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(IImm)
            r.io.inst.poke(12 << 20)
            r.io.imm.expect(12)
        }
    }
    it should "(I-Type) build UInt from the biggest positive value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(IImm)
            r.io.inst.poke(0x7ff << 20)
            r.io.imm.expect(0x7ff)
        }
    }
    it should "(I-Type) build UInt from a negative 12-bit value" in {
        test(new ImmGenSimple(xlen)) { r => 
            r.io.sel.poke(IImm)

            // value -2048
            r.io.inst.poke("h8000_0000".U)
            r.io.imm.expect("hffff_f800".U)
        }
    }
    it should "(I-Type) built UInt of value -1/-2 from 12-bit -1/-2 value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(IImm)
            // value -1
            r.io.inst.poke("hfff0_0000".U)
            r.io.imm.expect("hffff_ffff".U)

            // value -2
            r.io.inst.poke("hffe0_0000".U)
            r.io.imm.expect("hffff_fffe".U)
        }
    }
    it should "(I-Type) not be changed by non-immediate bits" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(IImm)

            // set all other bits to 1
            r.io.inst.poke((1 << 20) - 1)
            r.io.imm.expect(0)

            // value -2
            r.io.inst.poke("hffef_ffff".U)
            r.io.imm.expect("hffff_fffe".U)
        }
    }
}
class BasicSImmGenTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "(S-type) build a positive UInt value from 12 bits" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(SImm)

            // value 1
            r.io.inst.poke(1 << 7)
            r.io.imm.expect(1)
        }
    }
    it should "(S-type) sign-extend a 12-bit negative value into a UInt" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(SImm)

            // value -1
            r.io.inst.poke("hfe00_0f80".U)
            r.io.imm.expect("hffff_ffff".U)
        }
    }
    it should "{S-type) create 0 from 0" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(SImm)

            // value 0
            r.io.inst.poke(0.U)
            r.io.imm.expect(0.U)
        }
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
class BasicUImmGenTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "(U-type) create a 0 from 0" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(UImm)

            // value 0
            r.io.inst.poke(0.U)
            r.io.imm.expect(0.U)
        }
    }
    it should "(U-Type) create the smallest positive value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(UImm)

            r.io.inst.poke(1 << 12)
            r.io.imm.expect(1 << 12)
        }
    }
    it should "(U-Type) create the biggest positive value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(UImm)

            r.io.inst.poke("h7fff_f000".U)
            r.io.imm.expect("h7fff_f000".U)
        }
    }
    it should "(U-Type) create the smallest negative value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(UImm)

            r.io.inst.poke("hffff_f000".U)
            r.io.imm.expect("hffff_f000".U)
        }
    }
    it should "(U-Type) create the biggest negative value" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(UImm)

            r.io.inst.poke("h8000_0000".U)
            r.io.imm.expect("h8000_0000".U)
        }
    }
    it should "(U-type) not be changed by non-immediate bits" in {
        test(new ImmGenSimple(xlen)) { r =>
            r.io.sel.poke(UImm)

            r.io.inst.poke("hffff_ffff".U)
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
