import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

  val accountCounter = new AtomicInteger(1000)

  def createAccount(initialBalance: Double): ActorRef = {
    // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
    return BankManager.createAccount(accountCounter.incrementAndGet().toString, bankId, initialBalance)
  }

  def findAccount(accountId: String): Option[ActorRef] = {
    // Use BankManager to look up an account with ID accountId
    try { 
      return Some(BankManager.findAccount(bankId, accountId))
    } catch {
      case _: NoSuchElementException => None
    }
  }

  def findOtherBank(bankId: String): Option[ActorRef] = {
    // Use BankManager to look up a different bank with ID bankId
    try { 
      return Some(BankManager.findBank(bankId))
    } catch {
      case _: NoSuchElementException => None
    }
  }

  override def receive = {
    case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance) // Create a new account
    case GetAccountRequest(id) => {
      findAccount(id) match {
        case Some(a) => sender ! a
        case None => println("receive: No internal account found.")
      }
    } // Return account
    case IdentifyActor => sender ! this
    case t: Transaction => processTransaction(t)

    case t: TransactionRequestReceipt => {
      // Forward receipt
      findAccount(t.toAccountNumber) match {
        case Some(a) => a ! t // Internal
        case None => findOtherBank(t.transaction.from.substring(0, 4)) match {
          case Some(b) => b ! t
          case None => println("receive: No other bank found.")
        } // External
      }
    }

    case msg => ???
  }

  def processTransaction(t: Transaction): Unit = {
    implicit val timeout = new Timeout(5 seconds)
    val isInternal = t.to.length <= 4
    val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
    val toAccountId = if (isInternal) t.to else t.to.substring(4)
    val transactionStatus = t.status
    
    // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
    // HINT: Make use of the variables that have been defined above.
    if (isInternal) {
      findAccount(toAccountId) match {
        case Some(a) => a ! t
        case None => {
          println("processTransaction: No internal account found.")
          // TODO: Send a receipt with failed status back to sender
        }
      }
    } else {
      findOtherBank(toBankId) match {
        case Some(b) => b ! t
        case None => {
          println("processTransaction: No other bank found.")
          // TODO: Send a receipt with failed status back to sender
        }
      }
    }
  }

}
