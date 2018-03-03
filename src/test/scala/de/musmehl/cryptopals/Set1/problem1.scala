package de.musmehl.cryptopals.Set1

import org.scalatest.FunSuite

class CryptopalsProblems extends FunSuite {

    test("Challenge 1") {
        val input = HexString("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
        val output = Base64String("SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

        assert(input.toBase64String == output)
    }

    test("Challenge 2") {
        val input1 = HexString("1c0111001f010100061a024b53535009181c")
        val input2 = HexString("686974207468652062756c6c277320657965")
        val output = HexString("746865206b696420646f6e277420706c6179")

        assert(input1.xor(input2) == output)
    }
}
