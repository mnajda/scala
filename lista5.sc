// Mateusz Najda

class Pair[A, B](var fst: A, var snd: B) {
  override def toString: String = s"($fst, $snd)"
}

val para = new Pair(1, "napis")
println(para)
para.fst = 2
para.snd = "nowy napis"
println(para)

class BankAccount(initialBalance: Double) {
  private var balance = initialBalance
  def checkBalance = balance
  def deposit(amount: Double) = { balance += amount; balance}
  def withdraw(amount: Double) = { balance -= amount; balance}
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  override def deposit(amount: Double): Double = super.deposit(amount - 1)
  override def withdraw(amount: Double): Double = super.withdraw(amount + 1)
}

val checkingAccount = new CheckingAccount(500)
checkingAccount.checkBalance
checkingAccount.deposit(30)
checkingAccount.checkBalance == 529
checkingAccount.withdraw(30)
checkingAccount.checkBalance == 498

class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  private[this] var transactionCounter = 0
  def earnMonthlyInterest(): Unit = {
    super.deposit(checkBalance * 0.00002)
    transactionCounter = 0
  }
  override def deposit(amount: Double): Double = {
    transactionCounter += 1
    super.deposit(if (transactionCounter <= 3) amount else amount - 1)
  }

  override def withdraw(amount: Double): Double = {
    transactionCounter += 1
    super.withdraw(if (transactionCounter <= 3) amount else amount + 1)
  }
}

val savingsAccount = new SavingsAccount(500)
savingsAccount.checkBalance
savingsAccount.deposit(1)
savingsAccount.deposit(1)
savingsAccount.deposit(1)
savingsAccount.checkBalance == 503
savingsAccount.deposit(1)
savingsAccount.checkBalance == 503

val balanceBeforeAddingInterest = savingsAccount.checkBalance
val balanceAfterAddingInterest = balanceBeforeAddingInterest + (balanceBeforeAddingInterest * 0.00002)
savingsAccount.earnMonthlyInterest()
savingsAccount.checkBalance == balanceAfterAddingInterest
savingsAccount.withdraw(1)
savingsAccount.checkBalance == balanceAfterAddingInterest - 1

abstract class Zwierz {
  val imie: String
  def dajGlos: String
  def rodzaj: String = this.getClass.getSimpleName
  override def toString: String = s"$rodzaj $imie daje głos $dajGlos"
}

class Pies(override val imie: String = "bez imienia") extends Zwierz {
  def dajGlos: String = "Hau, hau!"
}

class Kot(override val imie: String = "bez imienia") extends Zwierz {
  def dajGlos: String = "Miau, miau!"
}

object TestZwierza {
  def main(args: Array[String]): Unit = {
    val animals: Vector[Zwierz] = Vector(new Pies(), new Pies("Burek"), new Kot("Mruczek"))
    for (animal <- animals) println(animal)
  }
}

TestZwierza.main(Array())
