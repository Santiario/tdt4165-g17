import exceptions._

class Account(initialBalance: Double, val uid: Int = Bank getUniqueId) {

  private var balance = initialBalance;
  def withdraw(amount: Double): Unit = {
    if(!isPositive(amount)) {
      throw new IllegalAmountException();
    } else if (amount > this.balance) {
      throw new NoSufficientFundsException();
    } else {
      this.balance -= amount
    }
  } // Implement

  private def isPositive(number: Double): Boolean = {
    return number > 0.0
  }

  def deposit(amount: Double): Unit = {
    if(!isPositive(amount)) {
      throw new IllegalAmountException();
    } else {
      this.balance += amount;
    }
  } // Implement
  def getBalanceAmount: Double = {
    return this.balance
  } // Implement
}
