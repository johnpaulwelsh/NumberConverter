val powersOf2 = {
  def loop(n: Int): Stream[Int] = n #:: loop(2*n)
  loop(1)
}

def powersOf2UpToN(n: Int) = powersOf2 takeWhile (_ <= n)

def nthPowersOf2(n: Int) = powersOf2 take n

def decimalToBinary(decimal: String) = {
  def calculate(num: Int, powers: Stream[Int], accum: String): String = {
    if      (powers.isEmpty)     accum
    else if (powers.head <= num) calculate(num - powers.head, powers.tail, accum + "1")
    else                         calculate(num, powers.tail, accum + "0")
  }

  val pwrs = powersOf2UpToN(decimal.toInt).reverse
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

  val pwrs = nthPowersOf2(binary.length).reverse
  val answer = calculate(binary, pwrs, 0)
  println(binary + " in decimal: " + answer)
}

def decimalToHex(decimal: String) = {

}

def hexToDecimal(hex: String) = {

}

def binaryToHex(binary: String) = {

}

def hexToBinary(hex: String) = {

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