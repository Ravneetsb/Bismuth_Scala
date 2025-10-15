// package bismuth
//
// import bismuth.Color
//
// @main def main(): Unit = {
//   val red = Color.red
//   val g = Color.Grayscale(1)
//   println(red)
//   println(g)
// }
package bismuth

import java.io.File
import javax.imageio.ImageIO
import scala.util.{Try, Success, Failure}
import fastparse._

object Main:

  def main(args: Array[String]): Unit =
    args.toList match
      case biFile :: outputFile :: Nil =>
        actualMain(biFile, outputFile)
      case _ =>
        println("usage: runMain bismuth.Main <bismuth-program> <output-file>")

  private def actualMain(biFile: String, outputFile: String): Unit =
    val fileContent = Try(scala.io.Source.fromFile(biFile).mkString)

    fileContent match
      case Failure(ex) =>
        println(s"Error reading file: ${ex.getMessage}")
      case Success(content) =>
        val parsed = Parser.parseProgram(content)
        parsed match
          case f: Parsed.Failure =>
            println(s"Parse error: ${f.msg}")
          case Parsed.Success(program, _) =>
            run(program) match
              case Left(RunTimeError.runTimeError(msg)) =>
                println(s"Runtime error: $msg")
              case Left(RunTimeError.parseError(msg)) =>
                println(s"Parse error: $msg")
              case Right(img) =>
                saveAsPng(img, outputFile)
                println(s"✅ Rendered successfully to $outputFile")

          // run(program) match
          //   case Left(RunTimeError) =>
          //     println(s"Runtime error")
          //   case Right(img) =>
          //     saveAsPng(img, outputFile)
          //     println(s"✅ Rendered successfully to $outputFile")

  private def saveAsPng(img: java.awt.image.BufferedImage, path: String): Unit =
    Try(ImageIO.write(img, "png", new File(path))) match
      case Failure(ex)    => println(s"Failed to save PNG: ${ex.getMessage}")
      case Success(false) => println("Could not find PNG writer.")
      case Success(true)  => () // success, nothing else to print
