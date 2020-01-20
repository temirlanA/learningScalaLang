import akka.actor._

object Storage {
  // in
  final case class Get(key: String)
  final case class Put(key: String, value: String)
  final case class Delete(key: String)

  // out
  final case class GetResult(key: String, value: Option[String])
  case object Ack
}

class Storage extends Actor {

  // перейдем в начальное состояние
  override def receive: Receive = process(Map.empty)

  def process(store: Map[String, String]): Receive = {

    // в ответ на сообщение Get вернем значение ключа в текущем состоянии
    // актор-отправитель сообщения доступен под именем sender
    case Storage.Get(key) =>
      sender ! Storage.GetResult(key, store.get(key))

    // в ответ на сообщение Put перейдем в следующее состояние
    // и отправим подтверждение вызывающему
    case Storage.Put(key, value) =>
      context become process(store + (key -> value))
      sender ! Storage.Ack

    // аналогично
    case Storage.Delete(key) =>
      context become process(store - key)
      sender ! Storage.Ack
  }
}

object Client {
  final case class Connect(storage: ActorRef)
  case object Process
}

class Client extends Actor {

  // обработчик сообщений начального состояния
  override def receive: Actor.Receive = {
    // в начальном состоянии дожидаемся команды присоединиться хранилищу
    case Client.Connect(storage) =>
      // переходим в рабочее состояние
      context become process(storage)
      // посылаем себе сообщение для начала работы в новом состоянии
      self ! Client.Process
  }

  // обработчик сообщений рабочего состояния
  def process(storage: ActorRef): Receive = {
    // считывание команд с клавиатуры
    case Client.Process =>
      println("Enter command:")

      // передача соответствующих команд хранилищу
      scala.io.StdIn.readLine().split(' ') match {
        case Array("get", key) => storage ! Storage.Get(key)
        case Array("put", key, value) => storage ! Storage.Put(key, value)
        case Array("delete", key) => storage ! Storage.Delete(key)
        case Array("stop") => context.system.terminate()
        case _ => println("Unknown command")
      }

      Thread.sleep(100)     // дадим время обработать сообщение
      self ! Client.Process // "рекурсия"

    // прием ответов от хранилища
    case Storage.GetResult(key, value) => println(s"Received: $key -> $value")

    case Storage.Ack => println("Received ack.")
  }
}

object StorageApp extends App {
  // все акторы принадлежат одной из систем акторов
  val actorSystem = ActorSystem()

  val storage: ActorRef = actorSystem.actorOf(Props[Storage])

  val client: ActorRef = actorSystem.actorOf(Props[Client])

  client ! Client.Connect(storage)
}
