package models

import deductions.runtime.jena.RDFStoreLocalProvider2
import org.w3.banana.RDF
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import org.w3.banana.jena.Jena
import com.hp.hpl.jena.query.Dataset

/** Links To User Forms - unused ! */
object LinksToUserForms extends RDFStoreLocalJena1Provider with LinksToUserFormsTrait[Jena, Dataset]

trait LinksToUserFormsTrait[Rdf <: RDF, DATASET] extends UserVocab
    with RDFStoreLocalProvider2[Rdf, DATASET] {

  import ops._

  def linksToUserForms(user: User): xml.NodeSeq = {
    val classesAndProperties = UserData.applicationClassesAndProperties()
    val classes = classesAndProperties.map { cp => cp._1 }
    val uris = UserData.getUserData(user)
    // TODO get classes' labels
    val classesAndData = classes zip uris
    <div>
      {
        for { cd <- classesAndData }
          yield <a href="/form?url={cd._2}">cd._1</a> // TODO class' label
      }
    </div>
  }
}
