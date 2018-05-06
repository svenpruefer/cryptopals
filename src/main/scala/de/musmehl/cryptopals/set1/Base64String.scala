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
      * Notice that this gives the actual bit pattern as a sequence of bytes, in particular it pads the stringContent
      * with zeros such that the number of Base64 characters is divisible by three
      */
    val toByteArray: Seq[Byte] = stringContent.map(base64ToByteMap).grouped(4).map(_.padTo(4, 0.toByte))
        .flatMap(Base64String.mapQuadrupleOfBase64BytesToTripleOfBytes).toSeq

    /**
      * converts a base64 string to a hex encoded string
      *
      * @return the base64 string as a hex string
      */
    def toHexString: HexString = {
        HexString.fromByteArray(toByteArray)
    }

}

object Base64String {

    protected def mapQuadrupleOfBase64BytesToTripleOfBytes(array: Seq[Byte]): Seq[Byte] = {
        require(array.length == 4, "Only works for quadruples of bytes")
        require(array.forall(_ < 64), "Every byte needs to correspond to a base64 character, i.e. needs to be less than 64")

        val byte1 = ((array.head << 2) + (array(1) >>> 4)).toByte
        val byte2 = (((array(1) % 16) << 4) + array(2) >>> 2).toByte
        val byte3 = (((array(2) % 4) << 6) + array(3) % 64).toByte

        Seq(byte1, byte2, byte3)
    }

    protected def mapPairOfBase64BytesToTripleOfBytes(array: Seq[Byte]): Seq[Byte] = {
        require(array.length == 2, "Only works for pairs of bytes")
        require(array.forall(_ < 64), "Every byte needs to correspond to a base64 character, i.e. needs to be less than 64")

        val byte1 = ((array.head >>> 2).toByte % 16).toByte
        val byte2 = (((array.head % 4).toByte << 2) + ((array(1) >>> 4).toByte % 4)).toByte
        val byte3 = (array(1) % 16).toByte
        Seq(byte1, byte2, byte3)
    }
}

