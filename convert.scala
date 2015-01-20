/**
 * Coversion script for decimal, hexadecimal, and binary.
 *
 * @author John Paul Welsh
 */

val hexToBinValues = Map(
  "0" -> "0000", "1" -> "0001", "2" -> "0010", "3" -> "0011",
  "4" -> "0100", "5" -> "0101", "6" -> "0110", "7" -> "0111",
  "8" -> "1000", "9" -> "1001", "A" -> "1010", "B" -> "1011",
  "C" -> "1100", "D" -> "1101", "E" -> "1110", "F" -> "1111"
)

def powersOfX(x: Int) = {
  def loop(n: Int): Stream[Int] = n #:: loop(x*n)
  loop(1)
}

def powersOf2UpToN(n: Int) = powersOfX(2) takeWhile (_ <= n)

def nthPowersOf2(n: Int) = powersOfX(2) take n

def trimLeadingZeroes(s: String) = (s.toList dropWhile (_ == '0')).mkString

def decimalToBinary(decimal: String) = {
  def calculate(num: Int, powers: Stream[Int], accum: String): String = {
    if      (powers.isEmpty)     accum
    else if (powers.head <= num) calculate(num - powers.head, powers.tail, accum + "1")
    else                         calculate(num, powers.tail, accum + "0")
  }

  val pwrs   = powersOf2UpToN(decimal.toInt).reverse
  val answer = calculate(decimal.toInt, pwrs, "")
  println(decimal + " in binary: " + answer)
}

def binaryToDecimal(binary: String) = {
  
  def calculate(num: String, powers: Stream[Int], accum: Int): String = {
    if (powers.isEmpty) accum.toString
    else calculate(num.substring(1),
                   powers.tail,
                   accum + (if (num.charAt(0) == '1') powers.head else 0)
                  )
  }

  val pwrs   = nthPowersOf2(binary.length).reverse
  val answer = calculate(binary, pwrs, 0)
  println(binary + " in decimal: " + answer)
}

def decimalToHex(decimal: String) = {

}

def hexToDecimal(hex: String) = {

}

def binaryToHex(binary: String) = {
  val splitBin = binary.split("").filter(_ != "")

}

def hexToBinary(hex: String) = {
  def calculate(num: List[String], accum: String): String = {
    if (num.length <= 0) accum
    else {
      val currBin = hexToBinValues.get(num.head) match {
        case Some(s) => s
        case None    => "0000"
      }
      calculate(num.tail, accum + currBin)
    }
  }

  val splitHex = hex.split("").filter(_ != "").toList
  val answer   = calculate(splitHex, "")
  println(hex + " in binary: " + trimLeadingZeroes(answer))
}

val value = args(1)
args(0) match {
  case "db" => decimalToBinary(value)
  case "bd" => binaryToDecimal(value)
  case "dh" => decimalToHex(value)
  case "hd" => hexToDecimal(value)
  case "bh" => binaryToHex(value)
  case "hb" => hexToBinary(value)
}
