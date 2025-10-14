package bismuth
import fastparse._
import Expr._
import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters._

class ParserTest extends munit.FunSuite {

  private def readFile(path: String): String =
    Files.readAllBytes(Paths.get(path)).map(_.toChar).mkString

  private def parseProgramFromFile(path: String): Parsed[Program] =
    parse(readFile(path), p => Parser.program(using p))

  private def passProgramFiles: Seq[String] = {
    val dir = Paths.get("src/test/resources/handwritten/")
    if (Files.exists(dir) && Files.isDirectory(dir)) {
      Files
        .list(dir)
        .toScala(Iterator)
        .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".bi"))
        .map(_.toString)
        .toSeq
    } else Seq.empty
  }

  private def failProgramFiles: Seq[String] = {
    val dir = Paths.get("src/test/resources/handwritten-failure/")
    if (Files.exists(dir) && Files.isDirectory(dir)) {
      Files
        .list(dir)
        .toScala(Iterator)
        .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".bi"))
        .map(_.toString)
        .toSeq
    } else Seq.empty
  }

  passProgramFiles.foreach { file =>
    test(s"Handwritten: $file") {
      val result = parseProgramFromFile(file)
      assert(
        result.isInstanceOf[Parsed.Success[?]],
        s"Parsing failed for $file: $result"
      )
    }
  }
  failProgramFiles.foreach { file =>
    test(s"handwritten-failure: $file") {
      val result = parseProgramFromFile(file)
      assert(
        result.isInstanceOf[Parsed.Failure],
        s"Parsing failed for $file: $result"
      )
    }
  }
}
