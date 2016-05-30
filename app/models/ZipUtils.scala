package models

import java.nio.file.Path
import scala.util.Try
import java.io.FileOutputStream
import java.util.zip.ZipOutputStream
import java.nio.file.SimpleFileVisitor
import java.nio.file.FileVisitResult
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.Files
import java.util.zip.ZipEntry
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.File

trait ZipUtils {

  def pack(folder: Path, zipFilePath: Path) = {
    Try {
      val fos = new FileOutputStream(zipFilePath.toFile());
      val zos = new ZipOutputStream(fos)

      class Visitor extends SimpleFileVisitor[Path] {
        override def visitFile(
          file: Path,
          attrs: BasicFileAttributes): FileVisitResult = {
          zos.putNextEntry(new ZipEntry(folder.relativize(file).toString()));
          Files.copy(file, zos);
          zos.closeEntry();
          FileVisitResult.CONTINUE
        }

        override def preVisitDirectory(
          dir: Path,
          attrs: BasicFileAttributes): FileVisitResult = {
          zos.putNextEntry(new ZipEntry(folder.relativize(dir).toString() + "/"));
          zos.closeEntry();
          return FileVisitResult.CONTINUE;
        }
      }
      Files.walkFileTree(folder, new Visitor)
      zos.close()
      fos.close()
    }
  }

  //  val Buffer = 2 * 1024
  //  def zip(out: String, files: Iterable[String], retainPathInfo: Boolean = true) = {
  //    var data = new Array[Byte](Buffer)
  //    val zip = new ZipOutputStream(new FileOutputStream(out))
  //    files.foreach { name =>
  //      if (!retainPathInfo)
  //        zip.putNextEntry(new ZipEntry(name.splitAt(name.lastIndexOf(File.separatorChar) + 1)._2))
  //      else
  //        zip.putNextEntry(new ZipEntry(name))
  //      val in = new BufferedInputStream(new FileInputStream(name), Buffer)
  //      var b = in.read(data, 0, Buffer)
  //      while (b != -1) {
  //        zip.write(data, 0, b)
  //        b = in.read(data, 0, Buffer)
  //      }
  //      in.close()
  //      zip.closeEntry()
  //    }
  //    zip.close()
  //  }

}