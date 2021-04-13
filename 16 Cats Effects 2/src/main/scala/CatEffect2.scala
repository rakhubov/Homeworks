import java.util.concurrent.Executors
import cats.effect._
import scala.concurrent.ExecutionContext
import scala.io.{Source, StdIn}


object CatEffect2 extends IOApp{



  def readFilePath(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[Source] = {
    for {
      _ <- IO(print("Enter file path: "))
      path <- IO(StdIn.readLine())
      validPath <- blocker.delay {Source.fromFile(path)}.handleErrorWith { _ =>
        IO(println("File path don't exist")) *> readFilePath(blocker)
      }
    } yield validPath
  }


  def readSeed(blocker: Blocker)(implicit contextShift: ContextShift[IO]): IO[Int] =
    blocker
      .blockOn(IO(println("Enter seed: ")) *> IO(StdIn.readInt()))
      .handleErrorWith(_ => IO(println("Seed must be a number ")) *> readSeed(blocker))


  def readFile(pach: Source)(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[List[String]] =
    blocker.delay {
      pach.getLines().flatMap(_.split("\\s+")).toList
    }


  def findMinHash(wordFromFile: List[String], seed: Int)(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[Int] =
    for{
      javaHash <- blocker.delay{wordFromFile.map(word => javaHash(word, seed))}
      knuthHash <- blocker.delay{wordFromFile.map(word => knuthHash(word, seed))}
      javaHashMin <- blocker.delay{javaHash.min}
      hashMin<- blocker.delay{knuthHash.foldLeft(javaHashMin){_ min _}}
    }yield hashMin



  override def run(args: List[String]): IO[ExitCode] = {
    val executionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
    val blocker = Blocker.liftExecutionContext(executionContext)


    (for {
      path <- readFilePath(blocker)
      seed <- readSeed(blocker)
      dataFromFile <- readFile(path)(blocker)
      minHash <- findMinHash(dataFromFile, seed)(blocker)
      _ <- IO(println(s"DataFromFile, $dataFromFile "))
      _ <- IO(println(s"MinHash, $minHash "))
      safe <- IO(minHash.toString :: dataFromFile)
    } yield safe) as ExitCode.Success
  }


  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }
  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }
}

