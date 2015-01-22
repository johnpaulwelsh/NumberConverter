Binary-Decimal-Hex Converter Script
===================================

An attempt to write a decimal, hexadecimal, and binary converter script in a functional and "Scala-ish" style.

Instructions
============

- Download the .scala file
- Install Scala on the command line if you haven't already.
- Run the file like this:

    scala convert.scala [X][Y] [NUMBER]

where X is the letter of the format you wish to start in, Y is the format you wish to convert to, and NUMBER is the number you're starting with (in its proper format).
- Hex letters must be capitalized. No spaces or commas. Binary does not need leading zeroes. No values that exceed Max-Int. Positive numbers only. The program does not handle 2s-complement conversions.
