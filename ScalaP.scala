/**
 * Author: Christopher Lee
 */

object CaesarCipher {

	def encrypt(str: String, shiftkey: Int) : String = {
		var lowerstr = str.toLowerCase()
		var stringbuilder = new StringBuilder("")
		var shift = shiftkey % 26
		var i = 0
		
		if (shift < 0) {
			shift = shift + 26
		}
		
		while (i < str.length) {
			var c = lowerstr.charAt(i)
			if ((c >= 'a') && (c <= 'z')) {
				if ((c + shift) > 'z') {
					stringbuilder.append((c + shift - 26).toChar)
				} else {
					stringbuilder.append((c + shift).toChar)
				}
			} else {
				stringbuilder.append(' ');
			}
			i = i + 1
		}
		return stringbuilder.toString()
	}
	
	def decrypt(str: String, shiftkey: Int) : String = {
		var lowerstr = str.toLowerCase()
		var stringbuilder = new StringBuilder("")
		var shift = shiftkey % 26
		var i = 0
		
		if (shift < 0) {
			shift = shift + 26
		}
		
		while (i < str.length) {
			var c = lowerstr.charAt(i)
			if ((c >= 'a') && (c <= 'z')) {
				if ((c - shift) < 'a') {
					stringbuilder.append((c - shift + 26).toChar)
				} else {
					stringbuilder.append((c - shift).toChar)
				}
			} else {
				stringbuilder.append(' ');
			}
			i = i + 1
		}
		return stringbuilder.toString()
	}
	
	def solve(str: String, maxshiftvalue: Int) = {
		var countup = 0
		var countdown = maxshiftvalue
		while (countup <= maxshiftvalue) {
			println("Caesar $countdown: " + decrypt(str, countup))
			countup = countup + 1
			countdown = countdown - 1
		}
	}
	
}

object Main extends App {
	val sentence = "Hello world"
	val shift = 3
	val maxshift = 26
	println("Original sentence: " + sentence)
	val encrypted = CaesarCipher.encrypt(sentence, shift)
	println("Encrypted sentence: " + encrypted)
	val decrypted = CaesarCipher.decrypt(encrypted, shift)
	println("Decrypted sentence: " + decrypted)
	println("Solving...")
	CaesarCipher.solve(decrypted, maxshift)
}