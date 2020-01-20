import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}

val greetingFuture = Future {
  Thread.sleep(1000) 
  println("calculating..."); 
  "Hello" 
}

println("Friend")

val greeting = Await.result(greetingFuture, Duration.Inf)

println(s"Result: $greeting")
//Friend
//calculating...
//Result: Hello
