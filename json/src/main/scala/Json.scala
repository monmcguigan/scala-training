package com.jpmc.json

sealed trait Json extends Product with Serializable

object Json {

  case class String(str: java.lang.String) extends Json

  case class Boolean(value: java.lang.Boolean) extends Json

  case class Number(num: Int) extends Json

  case object Null extends Json

  def apply(str: java.lang.String): Json = String(str)

  def apply(bool: java.lang.Boolean): Json = Boolean(bool)

  def apply(num: Int): Json = Number(num)

  sealed trait Doc extends Json

  case class Array(data: List[Json]) extends Doc

  case class Object(data: Map[java.lang.String, Json]) extends Doc

  def apply(data: Json*): Doc = Array(data.toList)

  def prettyPrint(json: Json): java.lang.String =
    json match {
      case jsonDoc: Doc => prettyPrintDoc(jsonDoc)
      case Json.String(str) => "\"" + str + "\""
      case Json.Boolean(bool) => bool.toString
      case Json.Number(num) => num.toString
      case Json.Null => "null"
    }

  private def prettyPrintDoc(jsonDoc: Doc): java.lang.String =
    jsonDoc match {
      case Array(data) =>
        data.map(prettyPrint).mkString("[", ",", "]")
      case Object(data) =>
        data.map {
          case (key, value) => val quote = "\""
            s"$quote$key$quote:${prettyPrint(value)}"
        }.mkString("{", ",", "}")
    }


  def removeNullValues(jsonDoc: Doc): Doc =
    jsonDoc match {
      case Array(_) => jsonDoc
      case Object(data) => Object(data = data.collect {
        case (key, obj: Json.Object) => key -> removeNullValues(obj)
        case (key, json: Json) if json != Null => key -> json
      })
    }
}