/* Copyright 2013 Harshad RJ
 * License: Apache License
 */
package com.lavadip.collage

import java.io.StringReader
import java.io.FileOutputStream
import org.apache.fop.apps.FopFactory
import org.xml.sax.helpers.XMLReaderFactory
import org.xml.sax.helpers.XMLReaderAdapter
import org.xml.sax.InputSource
import scala.xml.NodeSeq
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Atom

object FOP {
  implicit class nodeHelper(val n: Node) {
    def attrib(s: String) = attribOpt(s).get
    def attribOpt(s: String) = n.attribute(s).map(_.apply(0).toString)
    def text = n.child.toString
  }
  implicit class elemHelper(val n: Elem) {
    def addAttrib(k: String, v: String) = n % new xml.UnprefixedAttribute(k, v, xml.Null)
    def %(attrs: Map[String, String]): Elem = {
      val seq = for ((n, v) <- attrs) yield new xml.UnprefixedAttribute(n, v, xml.Null)
      (n /: seq)(_ % _)
    }
    def %(attr: (String, String)): Elem = {
      val (ns,v) = attr
      val a = new xml.UnprefixedAttribute(ns, v, xml.Null)
      n % a
    }
  }
  implicit def str2NodeSeq(s: String) = xml.Text(s)

  type Attributes = Map[String, String]
  val emptyAttribs = Map.empty[String, String]
  case class Cell(content: Node, attribs: Attributes = emptyAttribs, cellAttribs: Attributes = emptyAttribs) {
    def render(padding:String) = {
        <fo:table-cell padding-top={ padding } padding-bottom={ padding }>{
          mkBlock(content) % attribs
        }</fo:table-cell> % cellAttribs
    }
  }

  case class Row(cells: Seq[Cell], attribs: Attributes = emptyAttribs) {
    def render = mkRow(cells, "1pt") % attribs
  }

  def mkPDF(fName: String, fo: xml.NodeSeq) = {
    val outStream = new FileOutputStream(fName)
    val factory = FopFactory.newInstance
    // factory.setUserConfig("fop_config.xml")
    val fop = factory.newFop("application/pdf", outStream)

    val inputStream = new StringReader(fo.toString)
    val reader = XMLReaderFactory.createXMLReader()
    val readerAdapter = new XMLReaderAdapter(reader)
    val handler = fop.getDefaultHandler
    reader.setContentHandler(handler)
    reader.parse(new InputSource(inputStream))
    outStream.close
    inputStream.close
  }

  def mkRow(cols: Seq[Cell], padding: String) = {
    <fo:table-row keep-together.within-page="always">{
      cols.map { _.render(padding) }
    }</fo:table-row>
  }

  def mkTable(rows: Seq[Row], headers: Seq[Row] = Nil, columnSizes: Map[Int, String] = Map.empty, columnAttrs:Map[Int, Attributes] = Map.empty) = {
    val numColumns = rows.headOption.map(_.cells.length).getOrElse(0)
    val columnDeclarations = (0 until numColumns) map { i =>
      val columnSize = columnSizes.get(i).getOrElse("proportional-column-width(1)")
      val attribs = columnAttrs.get(i).getOrElse(emptyAttribs)
      <fo:table-column column-width={ columnSize }/> % attribs
    }

    <fo:table width="100%" table-layout="fixed" border-collapse="collapse">
      { columnDeclarations }
      {
        if (headers.isEmpty) {
          xml.Null
        } else {
          <fo:table-header>{
            headers.map(_.render)
          }</fo:table-header>
        }
      }
      <fo:table-body>{
        rows.map(_.render)
      }</fo:table-body>
    </fo:table>
  }

  def mkDoc(body: Node, after: Node, ids:Seq[String] = Seq.empty) = {
    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
      <fo:layout-master-set>
        <fo:simple-page-master master-name="simple" page-height="29.7cm" page-width="21cm" margin-top="0cm" margin-bottom="0cm" margin-left=".5cm" margin-right="0.5cm">
          <fo:region-body margin-top=".5cm" margin-bottom="1cm"/>
          <fo:region-after extent="1cm"/>
        </fo:simple-page-master>
      </fo:layout-master-set>
      <fo:bookmark-tree>{
        ids.map(id =>
          <fo:bookmark internal-destination={id}>
            <fo:bookmark-title>{id}</fo:bookmark-title>
          </fo:bookmark>
        )
      }</fo:bookmark-tree>
      <fo:page-sequence master-reference="simple" font-size="90%" >
        <fo:static-content flow-name="xsl-region-after">{
          after
        }</fo:static-content>
        <fo:flow flow-name="xsl-region-body">{
          body
        }</fo:flow>
      </fo:page-sequence>
    </fo:root>
  }

  def mkBlock(content: NodeSeq) = {
    <fo:block>{
      content
    }</fo:block>
  }
  def mkBlockContainer(content: NodeSeq) = {
    <fo:block-container>{
      content
    }</fo:block-container>
  }
  def mkInlineContainer(content: NodeSeq) = {
    <fo:inline-container>{
      content
    }</fo:inline-container>
  }


  def mkHeading(level: Int, title: String) = {
    <fo:block space-before="2cm" font-weight="bold" text-align="center" font-size="120%">{ title }</fo:block>
  }

  def mkInline(s:String, attrs: Attributes) = {
    <fo:inline>{s}</fo:inline> % attrs
  }
  def mkInline(n:NodeSeq, attrs: Attributes = emptyAttribs) = {
    <fo:inline>{n}</fo:inline> % attrs
  }
}