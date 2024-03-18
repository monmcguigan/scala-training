package com.jpmc.parsecs

import com.jpmc.parsecs.Parser.{pure, string}
import com.jpmc.parsecs.Result.{Failure, Success}

object Parser {
  def from[A](f: String => Result[A]): Parser[A] = new Parser[A] {
    override def parse(input: String): Result[A] = f(input)
  }

  // you had a non exhaustive pattern match

  def pred(cond: Char => Boolean): Parser[Char] = Parser.from {
    input =>
      if (input.nonEmpty && cond(input.head))
        Success(input.head, input.drop(1))
      else
        Failure(s"Unable to parse input: ${input}")
  }
  // for the boolean parser id like the err message to be better

  def char(expected: Char): Parser[Char] = pred(_ == expected)

  def string(expected: String): Parser[String] = {
    expected.map(Parser.char).foldLeft(Parser.pure("")) {
      (headParser, tailParser) =>
        headParser.map2(tailParser) {
          (head, tail) => head + tail
        }
    }
  }

  def pure[A](a: A): Parser[A] = from { in => Success(a, in) }

  val digit: Parser[Int] = pred(('0' to '9').toSet.contains(_)).map(c => c.toString.toInt)

  def number: Parser[Int] = digit.rep.map(_.foldLeft(0) { (curr, digit) => curr * 10 + digit })

  def boolean: Parser[Boolean] = {
    string("true").as(true) | string("false").as(false)
  }

  def whitespace: Parser[Char] = pred(_.isWhitespace)

  def listContent: Parser[List[String]] = alphaNumeric.repSep0(", ")

  def alphaNumeric: Parser[String] = (alphaNumericChar | whitespace).rep0.map(_.mkString)

  def alphaNumericChar: Parser[Char] = pred(c => c.isLetterOrDigit)
}

sealed trait Parser[A] {
  def parse(input: String): Result[A]

  def map[B](f: A => B): Parser[B] = Parser.from {
    input => parse(input).map(f)
  }

  def map2[B, C](other: => Parser[B])(f: (A, B) => C): Parser[C] = Parser.from {
    input =>
      parse(input) match {
        case Success(a, restA) => other.parse(restA) match {
          case Success(b, restB) => Result.Success(f(a, b), restB)
          case Failure(errMessage) => Result.Failure(errMessage)
        }
        case Failure(errMessage) => Result.Failure(errMessage)
      }
  }

  def as[B](b: B): Parser[B] = this.map(_ => b)

  // or - parse with Parser[A], if that fails, parse with Parser[B]
  def |[B >: A](other: => Parser[B]): Parser[B] = Parser.from {
    input =>
      parse(input) match {
        case Success(a, rest) => Success(a, rest)
        case Failure(_) => other.parse(input) match {
          case Success(value, rest) => Success(value, rest)
          case Failure(errMessage) => Failure(errMessage)
        }
      }
  }

  // andThen - parse with Parser[A] and then Parser[B]
  def ~[B](other: => Parser[B]): Parser[(A, B)] = map2(other)((a, b) => (a, b))

  def rep: Parser[List[A]] = this.map2(rep0) { (a, as) => a :: as }

  def rep0: Parser[List[A]] = rep | pure(List.empty)

  def *>[B](other: => Parser[B]): Parser[B] = map2(other)((_, b) => b)

  def <*[B](other: => Parser[B]): Parser[A] = map2(other)((a, _) => a)

  def opt: Parser[Option[A]] = this.map(Option.apply) | pure(Option.empty)

  def repSep(sep: String): Parser[List[A]] = (this ~ (string(sep) *> this).rep0) map {
    case (a, as) => a :: as
  }

  def repSep0(sep: String): Parser[List[A]] = repSep(sep) | pure(List.empty)
}

sealed trait Result[A] extends Product with Serializable {
  def map[B](f: A => B): Result[B] = this match {
    case Result.Success(value, rest) => Result.Success(f(value), rest)
    case Result.Failure(errMessage) => Result.Failure(errMessage)
  }
}

object Result {
  case class Success[A](value: A, rest: String) extends Result[A]

  case class Failure[A](errMessage: String) extends Result[A]
}

