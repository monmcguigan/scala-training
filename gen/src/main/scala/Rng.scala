package com.jpmc.gen

// https://en.wikipedia.org/wiki/Linear_congruential_generator
object Rng {
  private val intsCount = (Int.MaxValue.toLong - Int.MinValue.toLong).toDouble

  def nextSeed(seed: Long): Long =
    (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL

  def nextInt(seed: Long): (Int, Long) = {
    val next = nextSeed(seed)
    val i    = (next >> 16).toInt

    (i, next)
  }

  def nextInt(seed: Long, min: Int, max: Int): (Int, Long) = {
    val (i, nextSeed) = nextInt(seed)
    val length        = max.toLong - min.toLong

    (((i - Int.MinValue.toLong) / intsCount * length + min).toInt, nextSeed)
  }
}
