import scala.collection.immutable.ListMap

/* Part 2 */

def incr(x: Int): Int = x + 1
def double(x: Int): Int = x + x
def square(x: Int): Int = x * x

def factorial(n: Int): Int = 
  if (n == 0) {1} else {n * factorial(n-1)}


def power(x: Int, n: Int): Int =
  if (n == 0) {1} else {x * power(x,n-1)}

def factorial1(n: Int): Int = {
  val m = n-1 ; if (n == 0) {1} else {n * factorial1(m)}
}


def factorial2(n: Int): Int = {
  val m = n-1;
  if (n == 0) {1} else {n * factorial2(m)}
}


def factorial3(n: Int): Int = {
  val m = n-1;
  if (n == 0) {
    return 1;
  } else {
    return n * factorial3(m);
  }
}

/* Exercise 1 */
def p(x: Int, y:Int): Int = {
  return x*x + 2*x*y + y*y*y-1;
}

/* Exercise 2 */
def sum(n: Int): Int = {
  if(n == 0) return 0;
  else return n + sum(n-1);
}

/* Part 3 */

/* Exercise 3 */
def cycle(q:(Int,Int,Int)): (Int,Int,Int) = {
  return (q._2, q._3, q._1);
}

/* Part 4 */


def nameFromNum(presidentNum: Int): String = presidentNum match {
  case 41 => "George H. W. Bush"
  case 42 => "Bill Clinton"
  case 43 => "George W. Bush"
  case 44 => "Barack Obama"
  case 45 => "Donald J. Trump"
  case n => "I don't know who president number " + n + " is"
}

def numFromName(presidentName: String): Int = presidentName match {
  case "George H. W. Bush" => 41
  case "Bill Clinton" => 42
  case "George W. Bush" => 43
  case "Barack Obama" => 44
  case "Donald J. Trump" => 44
}

/* Exercise 4 */
def suffix(n: Int): String = {
  var s = "th";
  if(n % 10 == 1 && n % 100 != 11) s = "st";
  if(n % 10 == 2 && n % 100 != 12) s = "nd";
  if(n % 10 == 3 && n % 100 != 13) s = "rd";
  return String.valueOf(n) + s;
}


abstract class Colour
case class Red() extends Colour
case class Green() extends Colour
case class Blue() extends Colour

/* Exercise 5 */
def favouriteColour(c: Colour): Boolean = c match { 
  case Red() => return false;
  case Blue() => return true;
  case Green() => return false;
}


abstract class Shape
case class Circle(r: Double, x: Double, y: Double) extends Shape
case class Rectangle(llx: Double, lly: Double, w:Double, h:Double) extends Shape

def center(s: Shape): (Double,Double) = s match {
  case Rectangle(llx,lly,w,h) => (llx+w/2, lly+h/2)
  case Circle(r,x,y) => (x,y)
}

/* Exercise 6 */
def boundingBox(s: Shape): Rectangle = s match{
  case Rectangle(llx,lly,w,h) => Rectangle(llx,lly,w,h)
  case Circle(r,x,y) => Rectangle(x-r,y-r,2*r,2*r)
}

/* Exercise 7 */
def intervalOverlap(x1: Double, x2: Double,y1: Double, y2: Double): Boolean = {
    y1 <= x2 && x1 <= y2 
}

def mayOverlap(s1: Shape, s2: Shape) =  {
    val bb1 = boundingBox(s1);
    val bb2 = boundingBox(s2);
    (bb1, bb2) match {
      case (Rectangle(llx1, lly1, w1, h1),
        Rectangle(llx2, lly2, w2, h2)) =>
        intervalOverlap(llx1,llx1+w1,llx2,llx2+w2) &&
          intervalOverlap(lly1,lly1+h1,lly2,lly2+h2)
    }
}



/* Part 5 */

val anonIncr = {x: Int => x+1} // anonymous version of incr
val anonAdd = {x: Int => {y: Int => x + y}}

/* Exercise 8 */
def compose1[A, B, C](f: A => B, g: B => C)(x: A) = g(f(x))

/* Exercise 9 */
def compose[A, B, C](f: A => B, g: B => C) = {x:A => g(f(x))}

/* Exercise 10 */
def e1 = {x: Int => x.toString}
def e2 = {x: String => true}

def isEmpty[A](l: List[A]) = l match { 
  case Nil => true
  case x :: y => false
}


/* Exercise 11 */
def map[A, B](f: A => B, l: List[A]): List[B] = l match {
  case Nil => Nil
  case x :: xs => f(x) :: map(f,xs)
}
/* Exercise 12 */
def filter[A](f: A => Boolean, l: List[A]): List[A] = l match{
  case Nil => Nil
  case x :: xs => if (f(x)) {x :: filter(f,xs)} else {filter(f,xs)}
}

/* Exercise 13 */
def reverse[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case x :: xs => reverse(l) :+ x;
}


/* Part 6 */

def empty[K,V]: List[(K,V)] = List()

/* Exercise 14 */
def lookup[K, V](m: List[(K, V)], k: K): V = m match {
  case (k2,v)::m2 => if (k == k2) {v} else {lookup(m2,k)}
  case Nil => sys.error("Match failure")
}

/* Exercise 15 */
def update[K, V](m: List[(K, V)], k: K, v: V): List[(K, V)] = m match {
  case Nil => List((k,v))
  case (k2,v2)::m2 => if (k == k2) { (k,v)::m2 } else { (k2,v2)::update(m2,k,v) }
}


/* Exercise 16 */
def keys[K,V](m: List[(K,V)]): List[K] = m match {
  case Nil => Nil
  case (k, v) :: m2 => k :: keys(m2)
}

/* Exercise 17 */
val presidentListMap =  ListMap(
  41 -> "George H. W. Bush",
  42 -> "Bill Clinton",
  43 -> "George W. Bush",
  44 -> "Barack Obama",
  45 -> "Donald J. Trump")

/* Exercise 18 */
val empty = scala.collection.immutable.ListMap[Int,String]()

def map12_withUpdate = empty + (1 -> "a") + (2 -> "b")

/* Exercise 19 */
def list2map[K,V](l: List[(K,V)]): ListMap[K,V] = 

/* Exercise 20 */
def election(votes: List[String]): ListMap[String,Int] = sys.error("todo")
