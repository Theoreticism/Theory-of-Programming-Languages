/**
 * Caesar cipher program in Scala, functional
 * Author: Christopher Lee
 */

object CaesarCipher {

	/**
	 * Rotates the char given based on lower or upper case and shift value.
	 */
	private def rotate(base: Char, char: Char, offset: Int) = 
		(base.toInt + ((char.toInt - base.toInt + offset) % 26)).toChar
	
	/**
	 * Encrypt in a functional manner using pattern matching.
	 * The toInt function allows char to ASCII conversion, toChar allows ASCII to char.
	 */
	def encrypt(sentence: String, shift: Int) = sentence.map {
		case c if c isLower => rotate('a', c, shift)
		case c if c isUpper => rotate('A', c, shift)
		case c => c
	}
	
	/**
	 * Decrypt taking encryption with a negative shift.
	 */
	def decrypt(sentence: String, shift: Int) = encrypt(sentence, -shift)
	
	def solve(sentence: String, maxshift: Int) : Unit = {
		if (maxshift > 0) {
			println("Caesar " + maxshift + ": " + encrypt(sentence, maxshift))
			solve(sentence, (maxshift-1))
		} else {
			println("Caesar 0: " + sentence)
		}
	}
}

object Main extends App {
	val sentence = "Hello world"
	val shift = 3
	val maxshift = 26
	println("Original sentence: " + sentence)
	val encrypted = CaesarCipher.encrypt(sentence, shift)
	println("Shift amount: " + shift)
	println("Encrypted sentence: " + encrypted)
	val decrypted = CaesarCipher.decrypt(encrypted, shift)
	println("Decrypted sentence: " + decrypted)
	println("Solving...")
	CaesarCipher.solve("HAL", maxshift)
}