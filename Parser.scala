// TODO: Add parsing gifs better
// TODO: rescale the imgs


object Parser {
  import java.io._

  sealed abstract class Element {
    def compile: List[String]
  }
  case class TextBlob(text: List[String]) extends Element {
    override def compile = List("<div>") ::: text.reverse ::: List( "</div>", "<br><br>", "")
  }

  case class WideDiv(text: List[String]) extends Element {
    override def compile = List("<div class='wide'>") ::: text.reverse ::: List( "</div>", "<br><br>", "")
  }


  case class Image(path: Option[String]) extends Element {
    def smallPath: String = path match {
      case Some(aPath) => aPath.split("\\.")(1) match {
        // we don't make gifs smaller
        case "gif" => aPath
        case _ => aPath.split("\\.")(0) + "_small." + aPath.split("\\.")(1)

      }
      case _ => throw new Exception("Malformed object")
    }

    override def compile = path match {
      case Some(path) => List(
        "<div>",
        s"<a href='$path'><img src='$smallPath'></a>",
        "</div>", "<br><br>", ""
      )
      case _ => throw new Exception("Malformed object")
    }
  }

  case class VideoFrame(path: Option[String]) extends Element {
    override def compile: List[String] = path match {
      case Some(path) => List("<div>", s"<iframe src='$path'></iframe>", "</div>", "<br><br>", "")
      case _ => throw new Exception("Malformed object")
    }
  }
  case class Heading(level: Int, text: Option[String]) extends Element {
    override def compile: List[String] = text match {
      case Some(text) => List(s"<h$level>$text</h$level>", "")
      case _ => throw new Exception("Malformed object")
    }
  }

  case class SiteHeader() extends Element {
    def defaultHeaderFname: String = "default_header.html"
    override def compile: List[String] = {
      scala.io.Source.fromFile(defaultHeaderFname).getLines.toList
    }
  }

  case class Line(no: Int, content: String)

  def killDoubleBlank(lines: List[Line]): List[Line] = lines match {
    case Line(_, "") :: Line(no, "") :: rest => killDoubleBlank(Line(no, "") :: rest)
    case x :: xs => x :: killDoubleBlank(xs)
    case List(Line(_, "")) => Nil
    case xs => xs
  }

  def newToken(firstLine: String): Element = firstLine.trim match {
    case "*img*" => Image(None)
    case "*iframe*" => VideoFrame(None)
    case "*h1*" => Heading(1, None)
    case "*h2*" => Heading(2, None)
    case "*h3*" => Heading(3, None)
    case "*h4*" => Heading(4, None)
    case "*h5*" => Heading(5, None)
    case "*wide-div*" => WideDiv(List())
    case x => TextBlob(List(x))
  }

  def appendToCurrentToken(currentToken: Element, line: Line): Element = currentToken match {
    case TextBlob(text) => TextBlob(line.content :: text)
    case WideDiv(text) => WideDiv(line.content :: text)
    case Image(None) => Image(Some(line.content))
    case VideoFrame(None) => VideoFrame(Some(line.content))
    case Heading(level: Int, None) => Heading(level, Some(line.content))
    case _ => throw new Exception(
      s"Error in line ${line.no}: Cannot append to " +
      "current token $currentToken text: ${line.content}"
    )
  }

  def validate(token: Element, lineNo: Int): Element = token match {
    case Image(None) => throw new Exception(s"No source for image in line $lineNo")
    case VideoFrame(None) => throw new Exception(s"No path for video frame in line $lineNo")
    case Heading(level: Int, None) => throw new Exception(s"No text for heading in line $lineNo.")
    case x => x
  }

  def tokenBuilder(lines: List[Line], currentToken: Element, tokens: List[Element]): List[Element] = {
    lines match {
      case Line(lineNo, "") :: x :: xs => tokenBuilder(
        xs, newToken(x.content), validate(currentToken, lineNo) :: tokens
      )
      case x :: xs => tokenBuilder(xs, appendToCurrentToken(currentToken, x), tokens)
      case List() => (validate(currentToken, -1) :: tokens).reverse
    }
  }

  // Tokenize lines
  def tokenize(lines: List[Line]): List[Element] = tokenBuilder(
    killDoubleBlank(Line(0, "") :: lines),
    SiteHeader(),
    List()
  )

  // translate tokens one by one
  def compile(tokens: List[Element]): String = tokens.flatMap(x => x.compile).mkString("\n")

  def getFileLines(fname: String): List[Line] = scala.io.Source.fromFile(fname)
    .getLines
    .toList
    .zipWithIndex
    .map{ case (lineText: String, idx:Int) => Line(idx + 1, lineText) }


  def writeToFile(content: String, fname: String) = {
    val pw = new PrintWriter(new File(fname))
    pw.write(content)
    pw.close
  }

  def main(args: Array[String]): Unit = {
    val name = args(0)

    val lines = getFileLines(s"raw_text/$name.txt")
    val tokens = tokenize(lines)
    val html = compile(tokens)
    writeToFile(html, s"$name.html")
  }
}
