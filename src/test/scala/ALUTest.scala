import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BasicALUADDITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "add positive immediate to register" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_0001_0000_0000_0000_0001_0011".U)
            r.io.rs1Data.poke(0)
            r.io.rdData.expect(1)
        }
    }
    it should "add negative immediate to register" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b1111_1111_1111_0000_0000_0000_0001_0011".U)
            r.io.rs1Data.poke(1)
            r.io.rdData.expect(0)
        }
    }
    it should "return negative number when added to 0" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b1111_1111_1110_0000_0000_0000_0001_0011".U)
            r.io.rs1Data.poke(0)
            r.io.rdData.expect("hfffffffe".U)
        }
    }
    it should "allow adding a positive number to a negative number" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_1111_0000_0000_0000_0001_0011".U)
            r.io.rs1Data.poke("hfffffffe".U)
            r.io.rdData.expect(13)
        }
    }
}
class BasicALUADDTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "add positive immediate to register" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_0000_0000_0000_0000_0011_0011".U)
            r.io.rs1Data.poke(0)
            r.io.rs2Data.poke(1)
            r.io.rdData.expect(1)
        }
    }
    it should "add negative immediate to register" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_0000_0000_0000_0000_0011_0011".U)
            r.io.rs1Data.poke(1)
            r.io.rs2Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0)
        }
    }
    it should "return negative number when added to 0" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_0000_0000_0000_0000_0011_0011".U)
            r.io.rs1Data.poke(0)
            r.io.rs2Data.poke("hffff_fffe".U)
            r.io.rdData.expect("hffff_fffe".U)
        }
    }
    it should "allow adding a positive number to a negative number" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_0000_0000_0000_0000_0011_0011".U)
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
            r.io.inst.poke("b0100_0000_0000_0000_0000_0000_0011_0011".U)
            r.io.rs1Data.poke(13)
            r.io.rs2Data.poke(12)
            r.io.rdData.expect(1)
        }
    }
    it should "subtract numbers and yield negative result" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0100_0000_0000_0000_0000_0000_0011_0011".U)
            r.io.rs1Data.poke(12)
            r.io.rs2Data.poke(13)
            r.io.rdData.expect("hffff_ffff".U)
        }
    }    
}
//
// TODO: SLLI test
//

//
// TODO: SLL test
//

class BasicALUSLTITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_1111_0000_0010_0000_0001_0011".U)
            r.io.rs1Data.poke(13)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke(27)
            r.io.rdData.expect(0)

            r.io.inst.poke("b0000_0001_1011_0000_0010_0000_0001_0011".U)
            r.io.rdData.expect(0)
        }
    }
    it should "compare negative numbers" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b1111_1111_1110_0000_0010_0000_0001_0011".U)
            r.io.rs1Data.poke("hfffffffd".U)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke("hffffffff".U)
            r.io.rdData.expect(0)

            r.io.inst.poke("b1111_1111_1111_0000_0010_0000_0001_0011".U)
            r.io.rdData.expect(0)
        }
    }
}
class BasicALUSLTTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_0000_0000_0010_0000_0011_0011".U)
            r.io.rs1Data.poke(13)
            r.io.rs2Data.poke(15)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke(27)
            r.io.rdData.expect(0)

            r.io.inst.poke("b0000_0000_0000_0000_0010_0000_0011_0011".U)
            r.io.rs2Data.poke(27)
            r.io.rdData.expect(0)
        }
    }
    it should "compare negative numbers" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_0000_0000_0010_0000_0011_0011".U)
            r.io.rs1Data.poke("hffff_fffd".U)
            r.io.rs2Data.poke("hffff_fffe".U)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0)

            r.io.inst.poke("b0000_0000_0000_0000_0010_0000_0011_0011".U)
            r.io.rdData.expect(0)
        }
    }
}
class BasicALUSLTIUTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_1111_0000_0011_0000_0001_0011".U)
            r.io.rs1Data.poke(13)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke(27)
            r.io.rdData.expect(0)

            r.io.inst.poke("b0000_0001_1011_0000_0011_0000_0001_0011".U)
            r.io.rdData.expect(0)

            r.io.inst.poke("b1111_1111_1110_0000_0011_0000_0001_0011".U)
            r.io.rs1Data.poke("hffffffff".U)
            r.io.rdData.expect(0)
        }
    }
}
class BasicALUSLTUTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "compare positive numbers" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_0000_0000_0011_0000_0011_0011".U)
            r.io.rs1Data.poke(13)
            r.io.rs2Data.poke(15)
            r.io.rdData.expect(1)

            r.io.rs1Data.poke(27)
            r.io.rdData.expect(0)

            r.io.inst.poke("b0000_0000_0000_0000_0011_0000_0011_0011".U)
            r.io.rs2Data.poke(27)
            r.io.rdData.expect(0)

            r.io.inst.poke("b0000_0000_0000_0000_0011_0000_0011_0011".U)
            r.io.rs1Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0)
        }
    }
}
class BasicALUXORITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do XORI correctly" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b1111_1111_1111_0000_0100_0000_0001_0011".U)
            r.io.rs1Data.poke("haaaa_aaaa".U)
            r.io.rdData.expect(0x5555_5555)

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
            r.io.inst.poke("b0000_0000_0000_0000_0100_0000_0011_0011".U)
            r.io.rs1Data.poke("haaaa_aaaa".U)
            r.io.rs2Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0x5555_5555)

            r.io.rs1Data.poke("hffff_ffff".U)
            r.io.rdData.expect(0)
        }
    }
}

//
// TODO: Create SR{A|L}I test
//

//
// TODO: Create SR{A|L} test
//

class BasicALUORITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do ORI correctly" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b1111_1111_1111_0000_0110_0000_0001_0011".U)
            r.io.rs1Data.poke("haaaa_aaaa".U)
            r.io.rdData.expect("hffff_ffff".U)

            r.io.inst.poke("b0101_0101_0101_0000_0110_0000_0001_0011".U)
            r.io.rs1Data.poke("h0105_0a0a".U)
            r.io.rdData.expect(0x0105_0f5f)
        }
    }
}
class BasicALUORTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do OR correctly" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b1111_1111_1111_0000_0110_0000_0011_0011".U)
            r.io.rs1Data.poke("haaaa_aaaa".U)
            r.io.rs2Data.poke("hffff_ffff".U)
            r.io.rdData.expect("hffff_ffff".U)

            r.io.inst.poke("b0101_0101_0101_0000_0110_0000_0011_0011".U)
            r.io.rs1Data.poke("h0105_0a0a".U)
            r.io.rs2Data.poke("h0000_0555".U)
            r.io.rdData.expect(0x01050f5f)
        }
    }
}
class BasicALUANDITest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do ANDI correctly" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b1111_1111_1111_0000_0111_0000_0001_0011".U)
            r.io.rs1Data.poke("haaaaaaaa".U)
            r.io.rdData.expect("haaaaaaaa".U)

            r.io.inst.poke("b0101_0101_0101_0000_0111_0000_0001_0011".U)
            r.io.rs1Data.poke("h01050faf".U)
            r.io.rdData.expect(0x0000_0505)
        }
    }
}
class BasicALUANDTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val xlen = 32 // Width of register in bits
    it should "do AND correctly" in {
        test(new AluSimple(m, xlen)) { r =>
            r.io.inst.poke("b0000_0000_0000_0000_0111_0000_0011_0011".U)
            r.io.rs1Data.poke("haaaa_aaaa".U)
            r.io.rs2Data.poke("hffff_ffff".U)
            r.io.rdData.expect("haaaa_aaaa".U)

            r.io.inst.poke("b0000_0000_0000_0000_0111_0000_0011_0011".U)
            r.io.rs1Data.poke("h0105_0faf".U)
            r.io.rs2Data.poke("h0000_0555".U)
            r.io.rdData.expect(0x0000_0505)
        }
    }
}
