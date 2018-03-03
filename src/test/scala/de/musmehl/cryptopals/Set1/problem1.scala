package de.musmehl.cryptopals.Set1

import org.scalatest.FunSuite

class problem1 extends FunSuite {

    test("Challenge 1") {
        val input = HexString("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
        val output = Base64String("SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

        assert(input.toBase64String == output)
    }
}
