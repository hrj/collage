/* Copyright 2013 Harshad RJ
 * License: Apache License
 */
package com.lavadip.collage

import org.apache.fop.apps.FopFactory
import java.io.FileOutputStream
import org.xml.sax.helpers.XMLReaderFactory
import org.xml.sax.helpers.XMLReaderAdapter
import java.io.StringReader
import org.xml.sax.InputSource
import FOP._
import scala.xml.Node
import scala.xml.transform.RewriteRule
import scala.xml.transform.RuleTransformer
import scala.xml.Elem

case class Img(fName: String, scaleOpt: Option[Int] = None, rotationOpt: Option[Int] = None)

object Main extends App {
  def parseImgs(f: String) = {
    val lines = io.Source.fromFile(f).getLines.toList
    var group = Seq[Img]()
    var imgGroups = Seq[Seq[Img]]()
    lines.foreach { line =>
      line match {
        case "" =>
          imgGroups :+= group
          group = Seq[Img]()
        case s if (s.startsWith(";")) => // Comment Ignored
        case s =>
          val fields = s.split(",").map(_.trim).toSeq
          val sizeOpt = if (fields.length > 1) Some(fields(1)) else None
          group :+= Img(fields(0), sizeOpt.map(_.toInt))
      }

    }
    imgGroups :+ group
  }

  val fName = "imgs.csv" // TODO: Config
  val imgs = parseImgs(fName) // Seq(Img("test.png"), Img("photo.jpg", Some(25)), Img("test.png", Some(50)))
  val collage = new Collage(imgs).mkCollage

  if (false) {
    val pp = new xml.PrettyPrinter(120, 4)
    val writer = new java.io.FileWriter("collage.fop")
    writer.write(pp.format(collage))
    writer.close
  }
  mkPDF("collage.pdf", collage)
}

class Collage(imgs: Seq[Seq[Img]]) {

  private val emptyCell = Cell("")
  private val thinLine = "grey .5pt solid"
  private val topThinLineAttr = Map("border-top" -> thinLine)
  private val boldAttr = Map("font-weight" -> "bold")
  private val alignRightAttr = Map("text-align" -> "right")

  private def mkImg(img: Img) = {
    val attrs = emptyAttribs ++ img.scaleOpt.map(s => "content-width" -> (s + "%"))
    mkInline(
      <fo:external-graphic src={ img.fName } id={ img.fName }/> % attrs
    )
  }

  def mkCollage = {
    val footer = mkTable(Seq(Row(Seq(
      Cell(mkInline("Generated on " + new java.util.Date, Map("padding-left" -> "4pt", "color" -> "#555"))),
      Cell(mkInline(xml.Unparsed("Page # <fo:page-number font-weight='bold'/>"), Map("padding-right" -> "4pt")), alignRightAttr)),
      topThinLineAttr + ("background-color" -> "#eee"))))

    val allImgs = imgs.flatten
    val toc = allImgs.map { img =>
      mkBlock(xml.Group(Seq(
        <fo:basic-link internal-destination={ img.fName }>{ mkInline(img.fName) % boldAttr }</fo:basic-link>,
        <fo:leader leader-pattern="dots" color="grey"/>,
        <fo:page-number-citation ref-id={ img.fName }/>))) % Map("text-align-last" -> "justify", "margin-bottom" -> "4pt")
    }
    val tocBlock = mkBlock(xml.Group(toc)) % Map("padding" -> "8pt", "border" -> "grey 1pt solid", "margin-right" -> "1in", "margin-left" -> "1in")

    val doc = mkDoc(
      xml.Group(
        tocBlock +:
          imgs.map(imgseq =>
            mkBlock(
              imgseq.map { mkImg }))),
      footer, allImgs.map(_.fName))

    doc
  }
}