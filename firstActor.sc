import akka.actor._

val system = ActorSystem()

class MyActor extends Actor{
    override def receive = {
       case name: String => 
          println(s"Hello $name")
          context.stop(self)
    }
}

val greeter = system.actorOf(Props(classOf[MyActor], this))

greeter ! "Tima"
system.terminate()
