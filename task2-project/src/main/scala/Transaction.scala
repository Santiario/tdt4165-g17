import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  private var queue: Array[Transaction] = Array()

  // Remove and return the first element from the queue
  def pop: Transaction = synchronized {
    val element = queue(0)
    queue = queue.slice(1, queue.length)
    return element
  }

  // Return whether the queue is empty
  def isEmpty: Boolean = synchronized {
    return queue.length == 0
  }

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = synchronized {
    queue = t +: queue
  }

  // Return the first element from the queue without removing it
  def peek: Transaction = synchronized {
    queue(0)
  }

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = synchronized {
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

  var attempts: Int = 0

  override def run: Unit = {

    def doTransaction() = {
      from withdraw amount
      to deposit amount
    }

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
        case iae: IllegalAmountException => {

        attempts += 1
        this.status = TransactionStatus.FAILED
        processedTransactions.push(this)
      }
      case nsfe: NoSufficientFundsException => {
        attempts += 1
        if (attempts < allowedAttemps) {
          transactionsQueue.push(this)}
        else {
          this.status = TransactionStatus.FAILED
          processedTransactions.push(this)
        }
      }
    }
  }

}
