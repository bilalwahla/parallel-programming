// Parallelism granularity
/*
- bit-level: processing multiple bits of data in parallel
- instruction level: executing different instructions from the same instruction stream in parallel
- task level: executing separate instruction streams in parallel
*/
class HelloThread extends Thread {
  override def run(): Unit = {
    println("Hello")
    println("World!")
  }
}

def main(): Unit = {
  val t1 = new HelloThread
  val t2 = new HelloThread
  t1.start()
  t2.start()
  t1.join()
  t2.join()
}

main()

/*
Because the two threads run in parallel, each thread's execution of statements may overlap e.g.
above sometimes might execute like below:

Hello
Hello
World!
World!

In some cases we want to ensure that a sequence of statements in a specific thread executes at once.

This is called Atomicity. An operation is atomic if it appears as if it occurred instantaneously
from the point of view of other threads.
*/
private val x = AnyRef
private var uidCount = 0L
def getUniqueId(): Long = x.synchronized {
  uidCount = uidCount + 1
  uidCount
}

def startThread() = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (_ <- 0 until 10) yield getUniqueId()
      println(uids)
    }
  }
  t.start()
  t
}
startThread()
startThread()

/*
Nested synchronisation and a scenario when its needed.
*/
class Account(private var amount: Int = 0) {
  def transfer(target: Account, amount: Int): Unit = {
    this.synchronized {
      target.synchronized {
        this.amount -= amount
        target.amount += amount
      }
    }
  }
}

def startTransferThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      for (_ <- 0 until 10) { a.transfer(b, n) }
    }
  }
  t.start()
  t
}

// Above causes a deadlock. Understand why. And the solution is
class Account2(private var amount: Int = 0) {
  val uid = getUniqueId()

  private def lockAndTransfer(target: Account2, amount: Int): Unit = {
    this.synchronized {
      target.synchronized {
        this.amount -= amount
        println(s"Withdrawn account balance: ${this.amount}")
        target.amount += amount
        println(s"Deposited account balance: ${target.amount}")
      }
    }
  }

  def transfer(target: Account2, amount: Int): Unit =
    if (this.uid < target.uid) this.lockAndTransfer(target, amount)
    else target.lockAndTransfer(this, -amount)
}

def startTransferThread2(a: Account2, b: Account2, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      for (_ <- 0 until 10) { a.transfer(b, n) }
    }
  }
  t.start()
  t
}

val a1 = new Account2(100)
val a2 = new Account2(100)
val t1 = startTransferThread2(a1, a2, 2)
val t2 = startTransferThread2(a2, a1, 1)
t1.join()
t2.join()
