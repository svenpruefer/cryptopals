package de.musmehl.cryptopals.set1

/**
  * Class of a base64 string supplying various methods for dealing with base64 encoded strings
  *
  * @param stringContent the base64 string
  */
case class Base64String(stringContent: String) {
    def toByteArray: Array[Byte] = {
        Array.emptyByteArray
    }

    def toHexString: HexString = {
        HexString("")
    }



}
