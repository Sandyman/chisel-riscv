package problems

import chisel3.iotesters.PeekPokeTester

class RegFileTest(r: RegFile, m: Int, n: Int) extends PeekPokeTester(r) {

    for (t <- 0 until m) {
        // Check that initialised value is 0
        poke(r.io.rdAddr1, t)
        poke(r.io.rdAddr2, t)
        expect(r.io.rdData1, 0)
        expect(r.io.rdData2, 0)

        // Create some random value
        val num = rnd.nextInt((1 << (n - 1)) - 1)

        // And write to register        
        poke(r.io.wen, 1)
        poke(r.io.wrAddr, t)
        poke(r.io.wrData, num)

        // Ensure write actually happens
        step(1)

        // Read back and check it is what we expect
        poke(r.io.wen, 0)
        poke(r.io.rdAddr1, t)
        poke(r.io.rdAddr2, t)
        if (t == 0) {
            // But r0 always returns 0!
            expect(r.io.rdData1, 0)
            expect(r.io.rdData2, 0)
        } else {
            expect(r.io.rdData1, num)
            expect(r.io.rdData2, num)
        }
    }
}
