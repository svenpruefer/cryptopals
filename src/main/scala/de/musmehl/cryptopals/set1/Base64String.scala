package de.musmehl.cryptopals.set1

/**
  * Class of a base64 string supplying various methods for dealing with base64 encoded strings
  *
  * @param stringContent the base64 string
  */
case class Base64String(stringContent: String) {

    // todo fix padding issue with '=' sign

    /**
      * yields the base64 string as a sequence of bytes according to the base64ToByteMap.
      *
      * Notice that every base64 character is one scala byte, i.e. 8 bits instead of the necessary 6 bits.
      */
    val toByteArray: Seq[Byte] = stringContent.map(base64ToByteMap)

    /**
      * converts a base64 string to a hex encoded string
      *
      * @return the base64 string as a hex string
      */
    def toHexString: HexString = {
        val sequenceOfBase64Bytes: Seq[Seq[Byte]] = toByteArray.grouped(2).toList.map(_.padTo[Byte, Seq[Byte]](2, 0))
        val sequenceOfHexBytes: Seq[Seq[Byte]] = sequenceOfBase64Bytes.map(mapPairOfBase64BytesToTripleOfBytes)
        HexString(sequenceOfHexBytes.flatten.map(byteToHexMap).mkString)
    }

    private def mapPairOfBase64BytesToTripleOfBytes(array: Seq[Byte]): Seq[Byte] = {
        require(array.length == 2, "Only works for pairs of bytes")
        require(array.forall(_ < 64), "Every byte needs to correspond to a base64 character, i.e. needs to be less than 64")

        val byte1 = ((array.head >>> 2).toByte % 16).toByte
        val byte2 = (((array.head % 4).toByte << 2) + ((array(1) >>> 4).toByte % 4)).toByte
        val byte3 = (array(1) % 16).toByte
        Seq(byte1, byte2, byte3)
    }



}
