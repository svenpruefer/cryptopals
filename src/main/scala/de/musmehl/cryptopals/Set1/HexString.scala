package de.musmehl.cryptopals.Set1

import de.musmehl.cryptopals.Set1

/**
  * Class of a hex string supplying various methods for dealing with hex encoded strings
  *
  * @param stringContent the hex string
  */
case class HexString(stringContent: String) {

    /**
      * yields the hex string as a sequence of bytes according to the hexToByteMap.
      *
      * Notice that every hex character is an actual byte, i.e. 8 bits instead of the necessary 4 bits.
      */
    val toByteArray: Seq[Byte] = stringContent.map(Set1.hexToByteMap)

    def toBase64String: Base64String = {
        val sequenceOfHexBytes: Seq[Seq[Byte]] = toByteArray.grouped(6).toList.map(_.padTo[Byte, Seq[Byte]](6, 0))
        val sequenceOfBase64Bytes: Seq[Seq[Byte]] = sequenceOfHexBytes.map(mapTripleOfBytesToQuadrupleOfBase64Bytes)
        Base64String(sequenceOfBase64Bytes.flatten.map(byteToBase64Map).mkString)
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
    private def mapTripleOfBytesToQuadrupleOfBase64Bytes(array: Seq[Byte]): Seq[Byte] = {
        require(array.length == 6, "Only works for sixtets of bytes")
        require(array.forall(_ < 16), "Every byte needs to correspond to a hex character, i.e. needs to be less than 16")

        val piece1 = ((array.head << 2) + (array(1) >>> 2)).toByte
        val piece2 = (((array(1) % 4) << 4) + array(2)).toByte
        val piece3 = ((array(3) << 2) + (array(4) >>> 2)).toByte
        val piece4 = (((array(4) % 4) << 4) + array(5)).toByte

        Seq(piece1, piece2, piece3, piece4)
    }
}