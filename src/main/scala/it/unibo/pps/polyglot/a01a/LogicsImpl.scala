package it.unibo.pps.polyglot.a01a
import it.unibo.pps.polyglot.a01a.Direction.{HORIZONTAL, VERTICAL}

enum Direction:
  case HORIZONTAL, VERTICAL

trait MyLogics:
  def hit(x: Int, y: Int): Logics.Result

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val mapSize: Int, private val boatSize: Int) extends MyLogics:
  val random = scala.util.Random()
  val boatDirection: Direction = getDirection(random.nextInt(2))
  private var correctBoatHit: Int = 0
  private var missCounter: Int = 0

  private def computeBoatStartingPos(): (Int, Int) = boatDirection match
    case HORIZONTAL => (random.nextInt(mapSize), random.nextInt(mapSize - boatSize + 1))
    case VERTICAL => (random.nextInt(mapSize - boatSize + 1), random.nextInt(mapSize))

  val boatStartingPos: (Int, Int) = computeBoatStartingPos()

  private def getDirection(value: Int): Direction = value match
    case 0 => HORIZONTAL
    case 1 => VERTICAL

  private def checkHitWithDirection(x: Int, y: Int): Boolean =
    val (bx, by) = boatStartingPos
    boatDirection match
      case HORIZONTAL => bx <= x && x <= bx + boatSize - 1 && by == y
      case VERTICAL => by <= y && y <= by + boatSize - 1 && bx == x

  override def hit(x: Int, y: Int): Logics.Result =
    if checkHitWithDirection(x,y)
    then
      correctBoatHit += 1
      if correctBoatHit == boatSize
      then Logics.Result.WON
      else Logics.Result.HIT
    else
      missCounter += 1
      if missCounter == 5
      then Logics.Result.LOST
      else Logics.Result.MISS