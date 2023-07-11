import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Funct3._

class BasicALUADDITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "add positive immediate to register" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke(1)
            r.io.funct3.poke(addi)
            r.io.rs1Data.poke(0)
            r.io.rdData.expect(1)
        }
    }
    it should "add negative immediate to register" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke((1 << 12) - 1)
            r.io.funct3.poke(addi)
            r.io.rs1Data.poke(1)
            r.io.rdData.expect(0)
        }
    }
    it should "return negative number when added to 0" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke((1 << 12) - 2)
            r.io.funct3.poke(addi)
            r.io.rs1Data.poke(0)
            r.io.rdData.expect("hfffffffe".U)
        }
    }
    it should "allow adding a positive number to a negative number" in {
        test(new AluSimple(m, xlen)) { r =>
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
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(m, xlen)) { r =>
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
        test(new AluSimple(m, xlen)) { r =>
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
class BasicALUSLTIUTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke(15)
            r.io.funct3.poke(slti)
            r.io.rs1Data.poke(13)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke(27)
            r.io.rdData.expect(0)

            r.io.imm12.poke(27)
            r.io.rdData.expect(0)

            r.io.imm12.poke("hffe".U)
            r.io.rs1Data.poke("hffffffff".U)
            r.io.rdData.expect(0)
        }
    }
}
class BasicALUXORITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do XORI correctly" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke("hfff".U)
            r.io.funct3.poke(xori)
            r.io.rs1Data.poke("haaaaaaaa".U)
            r.io.rdData.expect(0x55555555)

            r.io.rs1Data.poke("hffffffff".U)
            r.io.rdData.expect(0)
        }
    }
}
class BasicALUORITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do ORI correctly" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke("hfff".U)
            r.io.funct3.poke(ori)
            r.io.rs1Data.poke("haaaaaaaa".U)
            r.io.rdData.expect("hffffffff".U)

            r.io.imm12.poke("h555".U)
            r.io.rs1Data.poke("h01050a0a".U)
            r.io.rdData.expect(0x01050f5f)
        }
    }
}
class BasicALUANDITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do ORI correctly" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.enabled.poke(1)
            r.io.imm12.poke("hfff".U)
            r.io.funct3.poke(andi)
            r.io.rs1Data.poke("haaaaaaaa".U)
            r.io.rdData.expect("haaaaaaaa".U)

            r.io.imm12.poke("h555".U)
            r.io.rs1Data.poke("h01050faf".U)
            r.io.rdData.expect(0x000000505)
        }
    }
}
