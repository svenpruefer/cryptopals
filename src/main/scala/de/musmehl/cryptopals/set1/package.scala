package de.musmehl.cryptopals

import scala.io.BufferedSource

package object set1 {
    val base64ToByteMap: Map[Char, Byte] = Map(
        ('A', 0), ('Q', 16), ('g', 32), ('w', 48),
        ('B', 1), ('R', 17), ('h', 33), ('x', 49),
        ('C', 2), ('S', 18), ('i', 34), ('y', 50),
        ('D', 3), ('T', 19), ('j', 35), ('z', 51),
        ('E', 4), ('U', 20), ('k', 36), ('0', 52),
        ('F', 5), ('V', 21), ('l', 37), ('1', 53),
        ('G', 6), ('W', 22), ('m', 38), ('2', 54),
        ('H', 7), ('X', 23), ('n', 39), ('3', 55),
        ('I', 8), ('Y', 24), ('o', 40), ('4', 56),
        ('J', 9), ('Z', 25), ('p', 41), ('5', 57),
        ('K', 10), ('a', 26), ('q', 42), ('6', 58),
        ('L', 11), ('b', 27), ('r', 43), ('7', 59),
        ('M', 12), ('c', 28), ('s', 44), ('8', 60),
        ('N', 13), ('d', 29), ('t', 45), ('9', 61),
        ('O', 14), ('e', 30), ('u', 46), ('+', 62),
        ('P', 15), ('f', 31), ('v', 47), ('/', 63)
    )

    val byteToBase64Map: Map[Byte, Char] = Map(
        (0, 'A'), (16, 'Q'), (32, 'g'), (48, 'w'),
        (1, 'B'), (17, 'R'), (33, 'h'), (49, 'x'),
        (2, 'C'), (18, 'S'), (34, 'i'), (50, 'y'),
        (3, 'D'), (19, 'T'), (35, 'j'), (51, 'z'),
        (4, 'E'), (20, 'U'), (36, 'k'), (52, '0'),
        (5, 'F'), (21, 'V'), (37, 'l'), (53, '1'),
        (6, 'G'), (22, 'W'), (38, 'm'), (54, '2'),
        (7, 'H'), (23, 'X'), (39, 'n'), (55, '3'),
        (8, 'I'), (24, 'Y'), (40, 'o'), (56, '4'),
        (9, 'J'), (25, 'Z'), (41, 'p'), (57, '5'),
        (10, 'K'), (26, 'a'), (42, 'q'), (58, '6'),
        (11, 'L'), (27, 'b'), (43, 'r'), (59, '7'),
        (12, 'M'), (28, 'c'), (44, 's'), (60, '8'),
        (13, 'N'), (29, 'd'), (45, 't'), (61, '9'),
        (14, 'O'), (30, 'e'), (46, 'u'), (62, '+'),
        (15, 'P'), (31, 'f'), (47, 'v'), (63, '/')
    )

    val hexToByteMap: Map[Char, Byte] = Map(
        ('0', 0),
        ('1', 1),
        ('2', 2),
        ('3', 3),
        ('4', 4),
        ('5', 5),
        ('6', 6),
        ('7', 7),
        ('8', 8),
        ('9', 9),
        ('a', 10),
        ('b', 11),
        ('c', 12),
        ('d', 13),
        ('e', 14),
        ('f', 15)
    )

    val byteToCountOfBinaryOnes: Map[Byte, Int] = Map(
        (0, 0),
        (1, 1),
        (2, 1),
        (3, 2),
        (4, 1),
        (5, 2),
        (6, 2),
        (7, 3),
        (8, 1),
        (9, 2),
        (10, 2),
        (11, 3),
        (12, 2),
        (13, 3),
        (14, 3),
        (15, 4)
    )

    val byteToHexMap: Map[Byte, Char] = Map(
        (0, '0'),
        (1, '1'),
        (2, '2'),
        (3, '3'),
        (4, '4'),
        (5, '5'),
        (6, '6'),
        (7, '7'),
        (8, '8'),
        (9, '9'),
        (10, 'a'),
        (11, 'b'),
        (12, 'c'),
        (13, 'd'),
        (14, 'e'),
        (15, 'f')
    )

    def scoreEnglishPlainTextString(string: String, nonLetterWeigth: Double = 0, controlWeigth: Double = 1): Double = {
        val relativeFrequency = Map[Char, Double](
            ('a', 0.08167), ('b', 0.01492),
            ('c', 0.02782), ('d', 0.04253),
            ('e', 0.12702), ('f', 0.02228),
            ('g', 0.02015), ('h', 0.06094),
            ('i', 0.06966), ('j', 0.00153),
            ('k', 0.00772), ('l', 0.04025),
            ('m', 0.02406), ('n', 0.06749),
            ('o', 0.07507), ('p', 0.01929),
            ('q', 0.00095), ('r', 0.05987),
            ('s', 0.06327), ('t', 0.09056),
            ('u', 0.02758), ('v', 0.00978),
            ('w', 0.02360), ('x', 0.00150),
            ('y', 0.01974), ('z', 0.00074)
        )

        val totalNumberOfLetters = string.count(x => (64.toByte < x.toByte && x.toByte < 91.toByte)
            || (96.toByte < x.toByte && x.toByte < 123.toByte))
        val frequencyInString =
            (for (letter <- relativeFrequency.keys)
                yield (letter, string.count(_.toLower == letter).toDouble / totalNumberOfLetters)).toMap[Char, Double]

        relativeFrequency.keys.map(x => math.pow(relativeFrequency(x) - frequencyInString(x), 2)).sum +
            nonLetterWeigth * math.pow((totalNumberOfLetters - string.length).toDouble / totalNumberOfLetters, 2) +
            controlWeigth * math.pow((string.count(!_.isControl) - string.length).toDouble / totalNumberOfLetters, 2)
    }

    def decodeXorLetterEncryption(hexString: HexString): (Char, HexString, Double) = {
        require(hexString.stringContent.length % 2 == 0, "HexString should contain an even number of hex characters")

        val n = hexString.stringContent.length / 2

        val characters: List[Char] = (0 to 127).map(_.toByte.toChar).toList

        val deviations = (for (letter <- characters) yield
            (letter,
                scoreEnglishPlainTextString(
                    HexString.fromAsciiString(List.fill(n)(letter).mkString).xor(hexString).toAsciiString, 1)))
            .toMap[Char, Double]

        val minDeviation = deviations.values.minBy(identity)

        val (minimum, _) = deviations.find(x => deviations(x._1) == minDeviation).get

        (minimum, HexString.fromAsciiString(List.fill(n)(minimum).mkString).xor(hexString), minDeviation)
    }

    def findXorLine(source: BufferedSource): (Char, String, String) = {
        val results: List[(Char, String, Double)] = source.getLines().foldLeft(List.empty[(Char, String, Double)])(
            (agg, el) => {
                val result = decodeXorLetterEncryption(HexString(el))
                (result._1, el, result._3) :: agg
            })
        val minDeviation = results.unzip3._3.minBy(identity)
        val (minimum, string, _) = results.find(x => x._3 == minDeviation).get
        (minimum, string, HexString.fromAsciiString(List.fill(string.length / 2)(minimum).mkString).xor(HexString(string)).toAsciiString)
    }

    def repeatingKeyXor(argument: String, key: String): String = {
        val hexArgument = HexString.fromAsciiString(argument)
        val hexKey = HexString.fromAsciiString(key)

        hexArgument.xorWithRepeatingKey(hexKey).stringContent
    }

    def calculateHammingDistance(arg1: String, arg2: String): Int =
        HexString.fromAsciiString(arg1).xor(HexString.fromAsciiString(arg2)).countBinaryOnes
}
