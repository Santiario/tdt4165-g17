import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  private var queue: Array[Transaction] = Array()

  // Remove and return the first element from the queue
  def pop: Transaction = {
    val element = queue(0)
    queue = queue.slice(1, queue.length)
    return element
  }

  // Return whether the queue is empty
  def isEmpty: Boolean = {
    return queue.length == 0
  }

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = {
    queue = queue :+ t
  }

  // Return the first element from the queue without removing it
  def peek: Transaction = {
    queue(0)
  }

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = {
    queue.toIterator
  }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {

    var attempts: Int = 0

    def doTransaction() = {
      from withdraw amount
      to deposit amount
    }

    def executeTransaction: Unit = {
      try {
        if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction
          }
        } else to synchronized {
          from synchronized {
            doTransaction
          }
        }

        // Extend this method to satisfy new requirements.
        status = TransactionStatus.SUCCESS
        processedTransactions push this
      } catch {
        case _: Throwable => {
          if (attempts == allowedAttemps) {
            status = TransactionStatus.FAILED
            processedTransactions push this
          } else {
            attempts += 1
            executeTransaction
          }
        }
      }
    }

    executeTransaction
  }

}
