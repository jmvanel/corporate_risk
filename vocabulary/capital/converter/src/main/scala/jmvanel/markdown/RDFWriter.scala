/* 

The utility of plain text is mostly to get the text without any markup, useful
in cases where you're doing something like pulling the header into a title
field, etc.

*/

package jmvanel.markdown

import java.io.{ StringWriter, Writer }
import com.tristanhunt.knockoff._
import scala.util.parsing.input.Position

trait RDFWriter {
  val prefix = "ques:" // http://www.bizinnov.com/ontologies/quest.owl.ttl#"
  /** current index for created URI's */
  var index=0
  
  /** last header for each level */
  val headers = scala.collection.mutable.ArrayBuffer.fill(20)("")
  headers(0) = "ques:capital-0"
  class HeaderResource(h:Header, val uri:String) extends Header(h.level, h.spans, h.position)
  var lastHeader: HeaderResource = _
  var precedingHeader: HeaderResource = new HeaderResource( new Header(0, Seq(), new Position{
    def column: Int = 1
    def line: Int = 1
    protected def lineContents: String = ""    
  } ), headers(0) )
  
  /** Creates a Group representation of the document. */
  def toTTL(blocks: Seq[Block]): String = {
    implicit val writer = new StringWriter
//    if (index == 0) 
    writer.write("""
      @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
      @prefix owl:  <http://www.w3.org/2002/07/owl#>.
      @prefix ques: <http://www.bizinnov.com/ontologies/quest.owl.ttl#> .
      @prefix     : <http://www.bizinnov.com/ontologies/quest.owl.ttl#> .

""")

    blocksToTTL( blocks )
    writer.toString
  }
  
  def blocksToTTL( blocks : Seq[Block] )( implicit writer : Writer ) : Unit =
    blocks.foreach( blockToTTL )
  
  def blockToTTL( block : Block )( implicit writer : Writer ) : Unit = {
    block match {
      case Paragraph( spans, _ ) => spans.foreach( spanToTTL )
//      case Header( _, spans, _ ) => 
      case h:Header =>
        val oldURI = updatecurrentURI()
        headerToTTL(h, currentURI(), oldURI )
      case LinkDefinition( _, _, _, _ ) => {}
      case Blockquote( children, _ ) => children.foreach( blockToTTL )
      case CodeBlock( text, _ ) => writer.write( text.content )
      case HorizontalRule( _ ) => {}
      case OrderedItem( children, _ ) => children.foreach( blockToTTL )
      case UnorderedItem( children, _ ) => children.foreach( blockToTTL )
      case OrderedList( items ) => items.foreach( blockToTTL )
      case UnorderedList( items ) => {
//        updatecurrentURI()
        val uri = currentURI()
        items.foreach( it => itemToTTL(it, uri) )
      }
    }
    writer.write(" ")
  }
  
  def currentURI() = index2URI(index)
  private def index2URI(i:Int)= prefix + "capital-" + i
  def updatecurrentURI() = {val oldIndex=index; index=index+1; index2URI(oldIndex) } 
     
  def headerToTTL(h:Header, uri: String="", oldURI: String="") ( implicit writer : Writer ) = {
    h match {
      case Header(_,List(Text(t)),_) => 
      // Header(1,List(Text(T1)),1.1)
        println(s"Header: level ${h.level} text $t")
        headers(h.level) = uri
        if( h.level > precedingHeader.level ) lastHeader = precedingHeader
        
        writer.write( "\n" )
        writeTriple(uri, prefix+"header", t)
        if( h.level >0 )
          writeTripleURI( lastHeader.uri, prefix+"subheader", uri)
        precedingHeader = new HeaderResource(h, uri)
      case _ =>
    }
  }
  
  def itemToTTL( item: UnorderedItem, uri: String )( implicit writer : Writer ) : Unit = {
    val head = item.children.head
    head match {
      case Paragraph( Seq(Text(t)), _ ) =>
      writeTriple(uri, "ques:item", t)
      case _ =>
    }
  }
  
  /** write a literal Triple; subject and pred are with an Turtle prefix */
  def writeTriple(subject:String, pred:String, objet:String ) ( implicit writer : Writer ) = {
    if( objet.contains("\\"))
      println(""" objet.contains("\\") """)
      val o2 = objet.replaceFirst("""\n$""", "")
      val o3 = """(?s)\\""".r replaceAllIn ( o2, "" )
      // .replaceAll("\\\n", "\n")
    writer.write( s"""$subject $pred ""\"${o3}""\" .\n""")
  }

  /** write a URI Triple; subject and pred are with an Turtle prefix */
  def writeTripleURI(subject:String, pred:String, objet:String ) ( implicit writer : Writer ) =
    writer.write( s"""$subject $pred ${objet} .\n""") 
    
  def spanToTTL( span : Span )( implicit writer : Writer ) : Unit = {
    span match {
//      case Text( content ) => {
      case t:Text => {
        paragraphToTTL(t)
//        writer.write( content )
      }
      case HTMLSpan( html ) => {} 
      case CodeSpan( code ) => writer.write( code )
      case Strong( children ) => children.foreach( spanToTTL )
      case Emphasis( children ) => children.foreach( spanToTTL )
      case Link( children, url, title ) => children.foreach( spanToTTL )
      case IndirectLink( children, definition ) => children.foreach( spanToTTL )
      case ImageLink( children, url, title ) => children.foreach( spanToTTL )
      case IndirectImageLink( children, definition ) => children.foreach( spanToTTL )
    }
    writer.write( " " )
  }
  
  def paragraphToTTL( text : Text )( implicit writer : Writer ) : Unit = {
	  writeTriple( currentURI(), "ques:paragraph", text.content )
  }
}
