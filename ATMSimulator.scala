import scala.util._
import scala.io.StdIn.{readLine, readInt}

object ATMSimulator {
    type Email = String
    val TAX: Double = 1.5;
    //use of case classes
    case class User(name:String,age:Int,username:String,password:String,email:Email)
    case class Transaction(id:Long, description:String) 
    case class Account(var balance:Int, owner:User, transactions:List[Transaction])
    val activeAccount = Account(owner = User("Jason Osei-Bediako",22,"Any","mySecret","mailto:P2521279@my365.dmu.ac.uk"),transactions = List(Transaction(1,"Anonymous"),Transaction(2,"Anonymously awesome deal")),balance = 1000222)

    def welcomeUser():Unit = {
        println("Hello $user! Welcome to the ATM.")
    }
    def alertUserToGetSelectedOption():Try[Int] = {
        println("""
At the ATM we have the following options:
1 - SmartPay (£100)
2 - Deposit Cash
3 - Withdraw Cash
4 - Request Balance
5 - Request transaction summary
 """)
        Try(readInt())
    }  
    def verifySelectedOption(t:Try[Int]):Unit = {
        t match {
            //Use of pattern match
            case Success(i) => i match {
                case 1 => transactSmartPay()
                case 2 => depositCash()
                case 3 => withdrawCash(TAX)
                case 4 => inquireBalance()
                case 5 => getTransactSummary()
                case _ => verifySelectedOption(alertUserToGetSelectedOption())
            }
            case Failure(e) => println(s"Invalid option selected. Terminating service now with error :: " + e.getMessage())
        }
    }
    def transactSmartPay():Unit = {
        attemptWithdrawal(getSmartFastCashAccount(),3000) match {
            case Right(b) => disburseCash(3000); println(s"Your balance is $b")
            case Left(b) => println(s"Transaction failed. Insufficient funds. Your balance is $b")
        }
    }
    def getSmartFastCashAccount():Account = activeAccount
    def disburseCash(amount:Int):Unit = println(s"Available funds: ${amount/100.0}")
    def depositCash():Unit = println("Do you want to deposit cash?")
    def withdrawCash(taxAmount: Double):Unit = {
        println("Withdraw Amount: ?")
        val tempLine = readLine()
        val requestedWithdrawal = Try(tempLine.toDouble)
        requestedWithdrawal match {
            //Use of Higher Order Function 
            case Success(d:Double) => ((attemptWithdrawal, activeAccount,(d * 100 * taxAmount)))
            case Failure(e) => println("Only numbers are acceptable!"); 
            //Use of recursive call and Partially applied function
            val tax = withdrawCash(_)
            tax(TAX)
        }
    }
    def attemptWithdrawal(anAccount:Account,amountInCents:Int):Either[Int,Int] = { 
        val withdrawalAttempt = anAccount.balance - amountInCents

        if (withdrawalAttempt < 0) {
            Left(anAccount.balance) 
        } else {
            anAccount.balance = withdrawalAttempt 
            println("Withdrawal successful.")
            Right(withdrawalAttempt)
        }
    }
    def inquireBalance():Unit = println("Available balance is  " + activeAccount.balance)
    def getTransactSummary():Unit = {
        println("Transaction Summary --:")
        activeAccount.transactions.foreach(t => println(s"${t.id}\t${t.description}"))
    }
    def customerExitPoint():Unit = {
        println("Thanks for your custom. See you!")
    }
    def main(args:Array[String]) = {
        val SWIFT_CASH = 3000
        welcomeUser()
        verifySelectedOption(alertUserToGetSelectedOption())
        customerExitPoint()
    }
}