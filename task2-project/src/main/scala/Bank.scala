import java.util.concurrent._
import java.util.concurrent.atomic._

import scala.concurrent._
import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {


  private val uid: AtomicInteger = new AtomicInteger()
  val transactionsQueue: TransactionQueue = new TransactionQueue()
  private val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executorContext: ExecutionContext = scala.concurrent.ExecutionContext.global

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    this.synchronized {
        transactionsQueue push new Transaction(
          transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
    }
  }

  def generateAccountId: Int = {
    uid.incrementAndGet()
  }

  private def processTransactions: Unit = {
    // println("starting processing")
    val thread = Main.thread {
      // println("running thread")
      // println(transactionsQueue isEmpty)
      while (!(transactionsQueue isEmpty)) {
        // println("popping trans")
        // println(transactionsQueue)
        val transaction: Transaction = transactionsQueue.pop
        // println(transactionsQueue)
        executorContext.execute(transaction)
      }

      Thread.sleep(500)
      processTransactions
    }
  }

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    // println("Bank: getting processed transactions: " + processedTransactions.iterator.toList)
    processedTransactions.iterator.toList
  }

  processTransactions

}
