import scala.collection.mutable.ArrayBuffer

// Hash Map Implementation
class HashMap[K, V](initialSize: Int = 16) {
  // Initialize an array of ArrayBuffer to store key-value pairs
  private val buckets: Array[ArrayBuffer[(K, V)]] = Array.fill(initialSize)(ArrayBuffer.empty)

  // Hash function to determine the index of the key in the array
  private def hash(key: K): Int = key.hashCode() % buckets.length

  // Insert a key-value pair into the hash map
  def put(key: K, value: V): Unit = {
    val index = hash(key)
    val bucket = buckets(index)
    val existingIndex = bucket.indexWhere { case (k, _) => k == key }
    if (existingIndex != -1) {
      // Update value if key already exists
      bucket(existingIndex) = (key, value)
    } else {
      // Add new key-value pair
      bucket += (key -> value)
    }
  }

  // Get the value associated with the given key
  def get(key: K): Option[V] = {
    val index = hash(key)
    val bucket = buckets(index)
    // Find the key-value pair in the bucket and return the value
    bucket.find { case (k, _) => k == key }.map { case (_, v) => v }
  }
}

// Stack Implementation
class Stack[A] {
  // List to store elements of the stack
  private var elements: List[A] = Nil

  // Push an element onto the stack
  def push(element: A): Unit = {
    elements = element :: elements
  }

  // Pop an element from the stack
  def pop(): Option[A] = elements match {
    case Nil => None
    case head :: tail =>
      elements = tail
      Some(head)
  }

  // Peek at the top element of the stack
  def peek(): Option[A] = elements match {
    case Nil => None
    case head :: _ => Some(head)
  }

  // Check if the stack is empty
  def isEmpty: Boolean = elements.isEmpty
}

// Queue Implementation
class Queue[A] {
  // Lists to store elements for enqueue and dequeue operations
  private var in: List[A] = Nil
  private var out: List[A] = Nil

  // Enqueue an element into the queue
  def enqueue(element: A): Unit = {
    in = element :: in
  }

  // Dequeue an element from the queue
  def dequeue(): Option[A] = {
    if (out.isEmpty) {
      out = in.reverse
      in = Nil
    }
    out match {
      case Nil => None
      case head :: tail =>
        out = tail
        Some(head)
    }
  }

  // Check if the queue is empty
  def isEmpty: Boolean = in.isEmpty && out.isEmpty
}

// Binary Search Tree Implementation
sealed trait BinaryTree[+A]
case object EmptyTree extends BinaryTree[Nothing]
case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

class BinarySearchTree[A <% Ordered[A]] {
  private var root: BinaryTree[A] = EmptyTree

  // Insert a value into the binary search tree
  def insert(value: A): Unit = {
    root = insertRec(root, value)
  }

  // Helper method to recursively insert a value into the binary search tree
  private def insertRec(tree: BinaryTree[A], value: A): BinaryTree[A] = tree match {
    case EmptyTree => Node(value, EmptyTree, EmptyTree)
    case Node(v, left, right) if value < v => Node(v, insertRec(left, value), right)
    case Node(v, left, right) if value > v => Node(v, left, insertRec(right, value))
    case _ => tree // value already exists
  }

  // Check if the binary search tree contains a value
  def contains(value: A): Boolean = {
    containsRec(root, value)
  }

  // Helper method to recursively check if the binary search tree contains a value
  private def containsRec(tree: BinaryTree[A], value: A): Boolean = tree match {
    case EmptyTree => false
    case Node(v, left, right) if value < v => containsRec(left, value)
    case Node(v, left, right) if value > v => containsRec(right, value)
    case Node(v, _, _) => true
  }
}

// Example usage
object Main extends App {
  // Create and use instances of the data structures
  val hashMap = new HashMap[String, Int]()
  hashMap.put("key1", 1)
  hashMap.put("key2", 2)
  println(hashMap.get("key1")) // Some(1)
  println(hashMap.get("key3")) // None

  val stack = new Stack[Int]()
  stack.push(1)
  stack.push(2)
  println(stack.pop()) // Some(2)
  println(stack.peek()) // Some(1)

  val queue = new Queue[Int]()
  queue.enqueue(1)
  queue.enqueue(2)
  println(queue.dequeue()) // Some(1)
  println(queue.dequeue()) // Some(2)

  val bst = new BinarySearchTree[Int]()
  bst.insert(5)
  bst.insert(3)
  println(bst.contains(3)) // true
  println(bst.contains(7)) // false
}
