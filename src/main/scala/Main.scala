
object Main {

  import scala.collection.mutable.ListBuffer

  case class Table(rows:ListBuffer[Row])
  case class Row(cells:ListBuffer[Cell[_]])
  case class Cell[T](t:T)

  def table(body : implicit Table => Unit):Table = {
    val t = new Table(ListBuffer())
    body(t)
    t
  }

  def row[T](body :implicit Row => Unit)(implicit t: Table) = {
    val r = new Row(ListBuffer())
    t.rows.append(r)
    body(r)
    t
  }

  def cell[T](t:T)(implicit row: Row) = {
      row.cells.append(Cell(t))
  }

  enum Colors{
    case Red, Green, Blue
  }

  def main1(args: Array[String]): Unit = {
    val m = table {
      row{
        cell(1)
        cell(2)
      }
      row{
        cell(3)
        cell("mert")
      }
    }
    println(m)
  }

  case class Boxy(x: Int)
  def someContextCreator(x: Int)(block: implicit Boxy ⇒ Unit): Unit = {
    block(Boxy(x))
  }
  def someFunc(m: String)(implicit t: Boxy) = println(m.length + t.x)
  someContextCreator(1) {
      someFunc("mert")
    }


  class Transaction
  def current : Transactional[Transaction] = implicitly[Transaction]
  type Transactional[T] = implicit Transaction => T
  def f1(x:Int) : Transactional[Int] = f2(x) + 1
  def f2(x:Int) : Transactional[Int] =  x + 2
  def transaction[T](block: Transactional[T])= {
    implicit val t = new Transaction
    block
  }

  val t1 = transaction {
    f1(1)
  }

  println(t1)

  def main(args:Array[String]):Unit = {
  
   }

}
