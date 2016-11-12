import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {
    def +=(value: Double) = {
        this.amount += value
    }

    def -=(value: Double) = {
        this.amount -= value
    }

    def <(that: Double): Boolean = {
        return this.amount < that
    }
  }

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

  def withdraw(amount: Double): Unit = synchronized {
    if(!isPositive(amount)) {
      throw new IllegalAmountException();
    } else if (this.balance < amount) {
      throw new NoSufficientFundsException();
    } else {
      this.balance -= amount
    }
  }

  def deposit(amount: Double): Unit = synchronized {
    if(!isPositive(amount)) {
      throw new IllegalAmountException();
    } else {
      this.balance += amount;
    }
  }

  private def isPositive(number: Double): Boolean = {
    return number > 0.0
  }

  def getBalanceAmount: Double = {
    return this.balance.amount
  }

  def transferTo(account: Account, amount: Double) = {
    // println("Account: transfering to another account")
    bank addTransactionToQueue(this, account, amount)
  }

}
