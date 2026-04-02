package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics
import it.unibo.pps.util.Sequences.Sequence, Sequence.*

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private val random = scala.util.Random
  private var cells: Sequence[(Int, Int)] = Nil()
  private val center: (Int, Int) = (random.nextInt(size - 2) + 1, random.nextInt(size - 2) + 1)
  private var offset: Int = 1

  override def tick(): Unit =
    for i <- -1 to 1 do
      for j <- -1 to 1 do
        if i != 0 || j != 0 then cells = Cons((center._1 + i * offset, center._2 + j * offset), cells)
    offset = offset + 1

  override def isOver: Boolean =
    cells.filter((x, y) => x < 0 || x >= size || y < 0 || y >= size) != Nil()

  override def hasElement(x: Int, y: Int): Boolean =
    cells.contains((x, y)) || center == (x, y)
