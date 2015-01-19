/* 

The utility of plain text is mostly to get the text without any markup, useful
in cases where you're doing something like pulling the header into a title
field, etc.

**TODO** HTML is currently bypassed, we should parse it and strip out the text.

*/

package jmvanel.markdown

import java.io.{ StringWriter, Writer }
import com.tristanhunt.knockoff._

trait RDFWriter {
  
  /** Creates a Group representation of the document. */
  def toTTL( blocks : Seq[Block] ) : String = {
    implicit val writer = new StringWriter
    blocksToTTL( blocks )
    writer.toString
  }
  
  def blocksToTTL( blocks : Seq[Block] )( implicit writer : Writer ) : Unit =
    blocks.foreach( blockToTTL )
  
  def blockToTTL( block : Block )( implicit writer : Writer ) : Unit = {
    block match {
      case Paragraph( spans, _ ) => spans.foreach( spanToTTL )
      case Header( _, spans, _ ) => spans.foreach( spanToTTL )
      case LinkDefinition( _, _, _, _ ) => {}
      case Blockquote( children, _ ) => children.foreach( blockToTTL )
      case CodeBlock( text, _ ) => writer.write( text.content )
      case HorizontalRule( _ ) => {}
      case OrderedItem( children, _ ) => children.foreach( blockToTTL )
      case UnorderedItem( children, _ ) => children.foreach( blockToTTL )
      case OrderedList( items ) => items.foreach( blockToTTL )
      case UnorderedList( items ) => items.foreach( itemToTTL )
    }
    writer.write(" ")
  }
  
  def itemToTTL( item: UnorderedItem )( implicit writer : Writer ) : Unit = {
    val head = item.children.head
    head match {
      case Paragraph( Seq(Text(t)), _ ) =>
        writer.write( s"""_:parent rdfs:label "${t.replaceFirst("\n$", "")}" .\n""")        
      case _ =>
    }
  }
  
  def spanToTTL( span : Span )( implicit writer : Writer ) : Unit = {
    span match {
      case Text( content ) => writer.write( content )
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
}
