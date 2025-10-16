package bismuth
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.*;
import java.io.File
import scala.util.Using
import scala.jdk.CollectionConverters.*
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import javax.imageio.ImageIO
import scala.util.{Try, Success, Failure}
import fastparse._

import scala.concurrent.Await
import scala.concurrent.duration.*
object Main:
  val defaultThreads = 8
  @main def all(args: String*): Unit =
    args.toList match {
      case directory :: outputDir :: threads :: Nil =>
        allMain(directory, outputDir, threads.toInt)
      case directory :: outputDir :: Nil =>
        allMain(directory, outputDir, defaultThreads)
      case _ =>
        println("Usage: java -jar bismuth.jar <inputDir> <outputDir> [threads]")
    }

  private def allMain(
      directory: String,
      outputDir: String,
      threads: Int
  ): Unit =
    val executor = Executors.newFixedThreadPool(threads)
    given ExecutionContext = ExecutionContext.fromExecutorService(executor)
    println(s"$directory, $outputDir")
    val input = Path.of(directory)
    val futures = bis(input, input, Path.of(outputDir))
    Await.result(Future.sequence(futures), Duration.Inf)

    executor.shutdown()

  private def bis(path: Path, root: Path, output: Path)(using
      ec: ExecutionContext
  ): Seq[Future[Unit]] =
    if Files.isDirectory(path) then readDirectory(path, root, output)
    else if isBi(path) then Seq(readFile(path, root, output))
    else Seq.empty

  private def isBi(path: Path): Boolean =
    path.toString().endsWith(".bi")

  private def readDirectory(
      path: Path,
      input: Path,
      output: Path
  )(using ec: ExecutionContext): Seq[Future[Unit]] =
    Using.resource(Files.newDirectoryStream(path)) { stream =>
      stream.asScala.toSeq.flatMap(entry => bis(entry, input, output))
    }

  private def readFile(path: Path, input: Path, output: Path)(using
      ec: ExecutionContext
  ): Future[Unit] =
    Future {
      val content = Files.readString(path)
      val ast = Parser.parseProgram(content)
      ast match {
        case Parsed.Success(program, _) =>
          run(program) match {
            case Right(img) =>
              val rel = input.relativize(path)
              val outPath = output.resolve(rel).toString.replace(".bi", ".png")
              new File(outPath).getParentFile.mkdirs()
              saveAsPng(img, outPath)
            case Left(RunTimeError.runTimeError(msg)) =>
              println(s"Runtime error in $path: $msg")
            case Left(RunTimeError.parseError(msg)) =>
              println(s"Parse error in $path: $msg")
          }
        case Parsed.Failure(a, b, extra) => {
          println(s"Failed at parsing: $path")
          println(s"$a \n $b")
          println(s"---- ---- ----")
          val trace = extra.trace()
          val msg = trace.longTerminalsMsg
          println(s"$msg")
        }
      }
    }

  private def saveAsPng(img: java.awt.image.BufferedImage, path: String): Unit =
    Try(ImageIO.write(img, "png", new File(path))) match
      case Failure(ex)    => println(s"Failed to save PNG: ${ex.getMessage}")
      case Success(false) => println("Could not find PNG writer.")
      case Success(true)  => () // success, nothing else to print
