import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Funct3._

class BasicALUADDITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val n = 32 // Width of register in bits
    it should "add positive immediate to register" in {
        test(new ALU(m, n)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke(1)
            r.io.funct3.poke(addi)
            r.io.rs1Data.poke(0)
            r.io.rdData.expect(1)
        }
    }
    it should "add negative immediate to register" in {
        test(new ALU(m, n)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke((1 << 12) - 1)
            r.io.funct3.poke(addi)
            r.io.rs1Data.poke(1)
            r.io.rdData.expect(0)
        }
    }
    it should "return negative number when added to 0" in {
        test(new ALU(m, n)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke((1 << 12) - 2)
            r.io.funct3.poke(addi)
            r.io.rs1Data.poke(0)
            r.io.rdData.expect("hfffffffe".U)
        }
    }
    it should "allow adding a positive number to a negative number" in {
        test(new ALU(m, n)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke(15)
            r.io.funct3.poke(addi)
            r.io.rs1Data.poke("hfffffffe".U)
            r.io.rdData.expect(13)
        }
    }
}

class BasicALUSLTITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val n = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new ALU(m, n)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke(15)
            r.io.funct3.poke(slti)
            r.io.rs1Data.poke(13)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke(27)
            r.io.rdData.expect(0)

            r.io.imm12.poke(27)
            r.io.rdData.expect(0)
        }
    }
    it should "compare negative numbers" in {
        test(new ALU(m, n)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke("hffe".U)
            r.io.funct3.poke(slti)
            r.io.rs1Data.poke("hfffffffd".U)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke("hffffffff".U)
            r.io.rdData.expect(0)

            r.io.imm12.poke("hfff".U)
            r.io.rdData.expect(0)
        }
    }
}
