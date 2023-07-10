import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BasicRegFileTest extends AnyFlatSpec with ChiselScalatestTester {
    val m = 32 // Number of registers
    val n = 32 // Width of register in bits
    it should "be initialised with all 0s" in {
        test(new RegFile(m, n)) { r =>
            for (t <- 0 until m) {
                // Check that initialised value is 0
                r.io.rdAddr1.poke(t)
                r.io.rdAddr2.poke(t)
                r.io.rdData1.expect(0)
                r.io.rdData2.expect(0)
            }
        }
    }
    it should "always return 0 for register address 0" in {
        test(new RegFile(m, n)) { r =>
            // Write a non-0 value into register 0
            r.io.wen.poke(1)
            r.io.wrAddr.poke(0)
            r.io.wrData.poke(0x7fffffff)

            // Progress time
            r.clock.step(1)

            // Check that both outputs return 0 for address 0
            r.io.wen.poke(0)
            r.io.rdAddr1.poke(0)
            r.io.rdAddr2.poke(0)
            r.io.rdData1.expect(0)
            r.io.rdData2.expect(0)
        }
    }
    it should "write and read back random values" in {
        val rnd = new scala.util.Random
        test(new RegFile(m, n)) { r =>
            for (t <- 1 until m) {
                // Create some random value
                val num = rnd.nextInt((1 << (n - 1)) - 1)

                // And write to register        
                r.io.wen.poke(1)
                r.io.wrAddr.poke(t)
                r.io.wrData.poke(num)

                // Ensure write actually happens
                r.clock.step(1)

                // Read back and check it is what we expect
                r.io.wen.poke(0)
                r.io.rdAddr1.poke(t)
                r.io.rdAddr2.poke(t)
                r.io.rdData1.expect(num)
                r.io.rdData2.expect(num)
            }
        }
    }
}
