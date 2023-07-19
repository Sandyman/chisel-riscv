import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Operation._

class BasicALUADDITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "add positive immediate to register" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(ADD)
            r.io.rs1Data.poke(0)
            r.io.rs2Data.poke(1)
            r.io.rdData.expect(1)
        }
    }
    it should "add negative immediate to register" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(ADD)
            r.io.rs1Data.poke(1)
            r.io.rs2Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0)
        }
    }
    it should "return negative number when added to 0" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(ADD)
            r.io.rs1Data.poke(0)
            r.io.rs2Data.poke("hffff_fffe".U)
            r.io.rdData.expect("hffff_fffe".U)
        }
    }
    it should "allow adding a positive number to a negative number" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(ADD)
            r.io.rs1Data.poke("hffff_fffe".U)
            r.io.rs2Data.poke(15)
            r.io.rdData.expect(13)
        }
    }
}
class BasicALUSUBTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "subtract numbers" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(SUB)
            r.io.rs1Data.poke(13)
            r.io.rs2Data.poke(12)
            r.io.rdData.expect(1)
        }
    }
    it should "subtract numbers and yield negative result" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(SUB)
            r.io.rs1Data.poke(12)
            r.io.rs2Data.poke(13)
            r.io.rdData.expect("hffff_ffff".U)
        }
    }
    it should "negate any value when subtracted from 0" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(SUB)
            r.io.rs1Data.poke(0)
            r.io.rs2Data.poke(13)
            r.io.rdData.expect("hffff_fff3".U)
        }
    }
}
class BasicALUSLLTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "double value when shifted to left by 1" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(SLL)
            r.io.rs1Data.poke(13)
            r.io.rs2Data.poke(1)
            r.io.rdData.expect(26)
        }
    }
    it should "become zero when shift too far" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(SLL)
            r.io.rs1Data.poke(12)
            r.io.rs2Data.poke(31)
            r.io.rdData.expect(0)
        }
    }
}
class BasicALUSLTTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(m, xlen)) { r =>
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
        test(new AluSimple(m, xlen)) { r =>
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
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(m, xlen)) { r =>
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
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do XOR correctly" in {
        test(new AluSimple(m, xlen)) { r =>
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
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "half value when shifted to right by 1" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(SRL)
            r.io.rs1Data.poke(26)
            r.io.rs2Data.poke(1)
            r.io.rdData.expect(13)
        }
    }
    it should "become zero when shifted too far" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(SRL)
            r.io.rs1Data.poke(12)
            r.io.rs2Data.poke(5)
            r.io.rdData.expect(0)
        }
    }
    it should "not sign extend" in {
        test(new AluSimple(m, xlen)) { r=>
            r.io.oper.poke(SRL)
            r.io.rs1Data.poke("hf842_8421".U)
            r.io.rs2Data.poke(4)
            r.io.rdData.expect("h0f84_2842".U)
        }
    }
}
class BasicALUSRATest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "half value when shifted to right by 1" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(SRA)
            r.io.rs1Data.poke(26)
            r.io.rs2Data.poke(1)
            r.io.rdData.expect(13)
        }
    }
    it should "become zero when shifted too far" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.oper.poke(SRA)
            r.io.rs1Data.poke(12)
            r.io.rs2Data.poke(5)
            r.io.rdData.expect(0)
        }
    }
    it should "sign extend a negative number" in {
        test(new AluSimple(m, xlen)) { r=>
            r.io.oper.poke(SRA)
            r.io.rs1Data.poke("hf842_8421".U)
            r.io.rs2Data.poke(4)
            r.io.rdData.expect("hff84_2842".U)
        }
    }
}
class BasicALUORTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do OR correctly" in {
        test(new AluSimple(m, xlen)) { r =>
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
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do AND correctly" in {
        test(new AluSimple(m, xlen)) { r =>
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
