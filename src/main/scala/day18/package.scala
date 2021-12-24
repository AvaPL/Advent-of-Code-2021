import java.util

package object day18 {
  case class Node(value: Int, depth: Int)

  type SnailfishNumber = util.LinkedList[Node]

  type SnailfishNumberIterator = util.ListIterator[Node]

  type MagnitudeIterator = util.ListIterator[Node]
}
