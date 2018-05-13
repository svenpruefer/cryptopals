package de.musmehl.cryptopals.set1

/**
  * Class of a hex string supplying various methods for dealing with hex encoded strings
  *
  * @param stringContent the hex string
  */
case class HexString(stringContent: String) {

    /**
      * xor bitwise with that
      *
      * @param that the second argument as a HexString
      * @return the bitwise xor'd HexString
      */
    def xor(that: HexString): HexString = {
        val xoredSequence = this.toByteArray.zip(that.toByteArray).map(x => x._1 ^ x._2).map(_.toByte)
        HexString.fromByteArray(xoredSequence)
    }

    /**
      * yields the hex string as a sequence of bytes according to the hexToByteMap.
      */
    val toByteArray: Seq[Byte] = stringContent.map(hexToByteMap).grouped(2).toList.map(_.padTo(2, 0.toByte)).toSeq
        .map(HexString.mapPairOfHexBytesToByte)

    /**
      * converts a hex string to a base64 encoded string
      *
      * @return the hex string as a base64 string
      */
    def toBase64String: Base64String = {
        val sequenceOfHexBytes: Seq[Seq[Byte]] = toByteArray.grouped(3).toList.map(_.padTo(3, 0.toByte))
        val sequenceOfBase64Bytes: Seq[Seq[Byte]] = sequenceOfHexBytes.map(HexString.mapTripleOfHexBytesToQuadrupleOfBase64Bytes)
        Base64String(sequenceOfBase64Bytes.flatten.map(byteToBase64Map).mkString)
    }

    val toAsciiString: String = {
       toByteArray.map(_.toChar).mkString
    }

    def xorWithRepeatingKey(key: HexString): HexString = {
        val (m, n) = BigInt(stringContent.length) /% BigInt(key.stringContent.length)
        val actualKey = HexString(
            List.fill(m.toInt)(key.stringContent).flatten.mkString + key.stringContent.substring(0,n.toInt))
        this.xor(actualKey)
    }

    def countBinaryOnes: Int = toByteArray.flatMap(HexString.mapAsciiByteToPairOfHexBytes).map(byteToCountOfBinaryOnes).sum

    private def extractBytes(position: Int, numberOfBytes: Int): Seq[Byte] = {
        require(position * numberOfBytes <= stringContent.length, "Cannot extract these bytes as the HexString" +
            " contains not enough bytes.")
        toByteArray.grouped(numberOfBytes).toIndexedSeq.apply(position - 1)
    }

    /**
      * extracts the group of 'numberOfBytes' bytes at 'position' place (counting from one) of the byte representation
      * of the HexString. Note that two hex characters are saved as one byte
      *
      * @param position         position of byte-group to extract, starting at one
      * @param numberOfBytes    size of groups of bytes which the hex string gets partitioned into
      * @return                 extracted HexString
      */
    def extractHexString(position: Int, numberOfBytes: Int): HexString = {
        require(position * numberOfBytes <= stringContent.length, "Cannot extract these bytes as the HexString" +
            " contains not enough bytes.")
        HexString.fromByteArray(extractBytes(position, numberOfBytes))
    }
}

object HexString {

    def fromAsciiString(string: String): HexString = {
        require(string.forall(_.toByte < 128), "String needs to consist of Ascii characters only")

        HexString(string.map(_.toByte).flatMap(mapAsciiByteToPairOfHexBytes).map(byteToHexMap).mkString)
    }

    def fromByteArray(byteArray: Seq[Byte]): HexString = {
        HexString(byteArray.flatMap(mapAsciiByteToPairOfHexBytes).map(byteToHexMap).mkString)
    }

    /**
      * takes a sequence of six (!) bytes corresponding to six hex characters, concatenates their bits and splits this
      * into four bytes corresponding to four base64 characters
      *
      * @param array the six hex characters as bytes
      * @return the quadruple of base64 characters as bytes
      * @example suppose array is given by the hex string '3f0a11', this corresponds to bytes 0x03, 0x0f, 0x00, 0x0a,
      *          0x01 and 0x01 which in turn is in bits 0000 0011, 0000 1111, 0000 0000, 0000 1011, 0000 0001
      *          and 0000 0001. Concatenating the corresponding last four bits each we get the 24 bits
      *          001111110000101100010001 splitting into 001111, 110000, 101100 and 010001, yielding the integers 15,
      *          48, 34 and 17, corresponding to bytes 0x0f, 0x30, 0x2c and 0x11 or base64 characters P, w, i and R
      */
    def mapTripleOfHexBytesToQuadrupleOfBase64Bytes(array: Seq[Byte]): Seq[Byte] = {
        require(array.length == 3, "Only works for triples of hex bytes")

        def fixBytes(byte: Byte): Int = if (byte < 0) byte + 256 else byte
        val fixedArray = array.map(fixBytes)

        val piece1 = (fixedArray.head >>> 2).toByte
        val piece2 = (((fixedArray.head % 4) << 4) + (fixedArray(1) >>> 4)).toByte
        val piece3 = (((fixedArray(1) % 16) << 2) + (fixedArray(2) >>> 6)).toByte
        val piece4 = (fixedArray(2) % 64).toByte

        Seq(piece1, piece2, piece3, piece4)
    }

    def mapPairOfHexBytesToByte(array: Seq[Byte]): Byte = {
        require(array.length == 2, "Only works for pairs of bytes")
        require(array.forall(_ < 16), "Every byte needs to correspond to a hex character, i.e. needs to be less than 16")

        ((array.head << 4).toByte + array(1)).toByte
    }

    def mapAsciiByteToPairOfHexBytes(byte: Byte): Seq[Byte] = {

        val correctedByte = if (byte < 0) byte + 256 else byte

        Seq((correctedByte >>> 4).toByte, (correctedByte % 16).toByte)
    }

}
