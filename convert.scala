/**
 * Coversion script for decimal, hexadecimal, and binary.
 *
 * @author John Paul Welsh
 */

 /*
  * Helper functions and values.
  */

val hexToBinValues = Map(
  "0" -> "0000", "1" -> "0001", "2" -> "0010", "3" -> "0011",
  "4" -> "0100", "5" -> "0101", "6" -> "0110", "7" -> "0111",
  "8" -> "1000", "9" -> "1001", "A" -> "1010", "B" -> "1011",
  "C" -> "1100", "D" -> "1101", "E" -> "1110", "F" -> "1111"
)

val revHexToBin = hexToBinValues map (_.swap)

val hexToDecValues = Map(
  "0" -> 0,  "1" -> 1,  "2" -> 2,  "3" -> 3,
  "4" -> 4,  "5" -> 5,  "6" -> 6,  "7" -> 7,
  "8" -> 8,  "9" -> 9,  "A" -> 10, "B" -> 11,
  "C" -> 12, "D" -> 13, "E" -> 14, "F" -> 15
)

val revHexToDec = hexToDecValues map (_.swap)

def powersOfX(x: Int, start: Int = 1): Stream[Int] = Stream.cons(start, powersOfX(x, start * x))

def powersOf2UpToN(n: Int) = powersOfX(2) takeWhile (_ <= n)

def powersOf16UpToN(n: Int) = powersOfX(16) takeWhile (_ <= n)

def nthPowersOf2(n: Int) = powersOfX(2) take n

def nthPowersOf16(n: Int) = powersOfX(16) take n

def trimLeadingZeroes(s: String) = (s.toList dropWhile (_ == '0')).mkString

def superSplit(s: String) = s.split("").filter(_ != "").toList

def extractStr(x: Option[String]) = x match { case Some(str) => str; case _ => "0" }

def extractInt(x: Option[Int]) = x match { case Some(i) => i; case _ => 1 }

/*
 * The actual functions.
 */

def decimalToBinary(decimal: String, is2sComp: Boolean) = {
  def calculate(num: Int, powers: Stream[Int], accum: String): String = {
    if      (powers.isEmpty)     accum
    else if (powers.head <= num) calculate(num - powers.head, powers.tail, accum + "1")
    else                         calculate(num, powers.tail, accum + "0")
  }

  val pwrs   = powersOf2UpToN(decimal.toInt).reverse
  val answer = calculate(decimal.toInt, pwrs, "")
  println(decimal + " in binary: " + answer)
}

def binaryToDecimal(binary: String, is2sComp: Boolean) = {
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
  def calculate(num: Int, powers: Stream[Int], accum: String): String = {
    if (powers.isEmpty) accum
    else if (powers.head <= num) {
      val dividesBy = num / powers.head
      val hexSymbol = extractStr(revHexToDec.get(dividesBy))
      calculate(num - (powers.head * dividesBy), powers.tail, accum + hexSymbol)
    } else calculate(num, powers.tail, accum + "0")
  }
  val pwrs   = powersOf16UpToN(decimal.toInt).reverse
  val answer = calculate(decimal.toInt, pwrs, "")
  println(decimal + " in hecidecimal: " + answer)
}

def hexToDecimal(hex: String) = {
  val pwrs       = nthPowersOf16(hex.length).reverse
  val zippedList = (pwrs, superSplit(hex)).zipped.toList
  val extrList   = zippedList map (x => (x._1, extractInt(hexToDecValues.get(x._2))))
  val answer     = (extrList map (x => x._1 * x._2)).sum
  println(hex + " in decimal: " + answer)
}

def binaryToHex(binary: String) = {
  def splitBy4(str: List[String], accum: List[String]): List[String] = {
    if (str.isEmpty) accum
    else splitBy4(str drop 4, (str take 4).mkString :: accum)
  }

  def calculate(groupings: List[String], accum: String): String = {
    if (groupings.isEmpty) accum
    else calculate(groupings.tail, accum + extractStr(revHexToBin.get(groupings.head)))
  }

  val paddedBinary = binary.reverse + ((binary.length % 4) match {
    case 0 => ""
    case 1 => "0"
    case 2 => "00"
    case 3 => "000"
    case _ => ""
  })

  val groupedBinary = splitBy4(superSplit(paddedBinary), List()) map (_.reverse)
  val answer = calculate(groupedBinary, "")
  println(binary + " in hexadecimal: " + answer)
}

def hexToBinary(hex: String) = {
  def calculate(num: List[String], accum: String): String = {
    if (num.length <= 0) accum
    else calculate(num.tail, accum + extractStr(hexToBinValues.get(num.head)))
  }

  val splitHex = superSplit(hex)
  val answer   = calculate(splitHex, "")
  println(hex + " in binary: " + trimLeadingZeroes(answer))
}

/*
 * The entry point.
 */

val convType = args(0)
val is2sComp = args(1) == "2s"
val value    = args(2)

convType match {
  case "db" => decimalToBinary(value, is2sComp)
  case "bd" => binaryToDecimal(value, is2sComp)
  case "dh" => decimalToHex(value)
  case "hd" => hexToDecimal(value)
  case "bh" => binaryToHex(value)
  case "hb" => hexToBinary(value)
}