package de.musmehl.cryptopals.set1

import org.scalatest.FunSuite
import resource._

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

    test("Challenge 3") {
        val input = HexString("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
        val (minimum, result, _) = decodeXorLetterEncryption(input)

        println(s"Found key: $minimum with resulting string: ${result.toAsciiString}")
        assert(minimum == 'X')
        assert(result.toAsciiString == "Cooking MC's like a pound of bacon")
    }

    test("Challenge 4") {
        for (source <- managed(scala.io.Source.fromURL(getClass.getResource("/4.txt")))) {
            val (minimum, original, decoded) = findXorLine(source)
            println(s"Found key: $minimum for line: $original with decoded result: $decoded")
            assert(minimum == '5')
            assert(decoded == "Now that the party is jumping\n")
        }
    }

    test("Challenge 5") {
        val input = """Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal"""

        val key = "ICE"

        val output = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

        assert(repeatingKeyXor(input, key) == output)
    }

    test("Challenge 6 Hamming Distance") {
        val input1 = "this is a test"
        val input2 = "wokka wokka!!!"

        val hammingDistance = calculateHammingDistance(input1, input2)

        assert(hammingDistance == 37)
    }

    test("Challenge 6 Extract HexStrings") {
        val input = "this isnt a test"
        val result = HexString.fromAsciiString(input).extractHexString(2,3).toAsciiString

        assert(result == "s i")
    }

    test("Convert ByteArray to HexString") {
        val input = HexString("0123456789abcde")
        val result = HexString.fromByteArray(input.toByteArray)

        assert(result == input)
    }
}
