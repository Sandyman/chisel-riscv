package problems

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BasicTest extends AnyFlatSpec with ChiselScalatestTester {
    it should "Write and read back random values" in {
        val m = 32
        val n = 32
        val rnd = new scala.util.Random
        test(new RegFile(m, n)) { r => 
            for (t <- 0 until m) {
                // Check that initialised value is 0
                r.io.rdAddr1.poke(t)
                r.io.rdAddr2.poke(t)
                r.io.rdData1.expect(0)
                r.io.rdData2.expect(0)

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
                if (t == 0) {
                    // But r0 always returns 0!
                    r.io.rdData1.expect(0)
                    r.io.rdData2.expect(0)
                } else {
                    r.io.rdData1.expect(num)
                    r.io.rdData2.expect(num)
                }
            }
        }
    }
}
