package views

import scala.xml.NodeSeq

trait History {
  val content: NodeSeq
  def page() =
    // <!DOCTYPE html>
    <body>
      <h2>Historique</h2>
      { content }
    </body>
}