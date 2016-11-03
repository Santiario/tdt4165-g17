import java.util.concurrent._
import java.util.concurrent.atomic._

object Bank {

  // private var idCounter: Int = 0
  private var idCounter: AtomicInteger = new AtomicInteger()

  def transaction(from: Account, to: Account, amount: Double): Unit = {
    from.withdraw(amount)
    to.deposit(amount)
  } // Implement

  def getUniqueId: Int = {
    // idCounter += 1 // Can this be improved?
    idCounter.incrementAndGet() // Yes it can
  }

}
