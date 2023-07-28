package eek

trait TestUtils {
    // Convert Int into BigInt keeping its sign
    def toBigInt(x: Int): BigInt = (BigInt(x >>> 1) << 1) | (x & 1)
}
