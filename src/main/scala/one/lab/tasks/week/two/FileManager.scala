package one.lab.tasks.week.two

import java.nio.file.Files
import java.nio.file.Paths

import scala.jdk.CollectionConverters._
import scala.io.StdIn.readLine

/**
  * Можете реализовать свою логику.
  * Главное чтобы работали команды и выводились ошибки при ошибочных действиях.
  * ll - показать все что есть в тек. папке
  * dir - показать только директории в тек. папке
  * ls - показать только файлы в тек. папке
  * cd some_folder - перейте из тек. папки в другую (учитывайте что путь можно сделать самым простым
  *                  то есть если я сейчас в папке /main и внутри main есть папка scala и я вызову
  *                  cd scala то мы должны просто перейти в папку scala. Реализация cd из текущей папки
  *                  по другому ПУТИ не требуется. Не забудьте только реализовать `cd ..`)
  *
  * Бонусные команды и идеи привествуются.
  */
object FileManager extends App {

  trait Command {
    def isSubstitutive: Boolean = false
  }

  case class PrintErrorCommand(error: String) extends Command
  case class ListDirectoryCommand()           extends Command
  case class ListFilesCommand()               extends Command
  case class DeleteFile(name: String)         extends Command
  case class CreateNewFile(name: String)      extends Command
  case class ListAllContentCommand()          extends Command
  case class ChangeDirectoryCommand(destination: String) extends Command {
    override val isSubstitutive: Boolean = true
  }

  case class ChangePathError(error: String)
  case class FileDoesNotExistError(error: String)
  case class FileAlreadyExistError(error: String)
  case class GetContent(filter: Option[String])


  def getContent(path: String, content: GetContent): List[String] = {
    val res = Files
                    .list(Paths.get(path))
                    .iterator()
                    .asScala
    content.filter match {
      case Some(value) =>
        if (value == "files") res.filter(path  => path.toFile.isFile)
                                 .map(path     => path.toFile.getName)
                                 .map(name     => s"/$path/$name")
                                 .toList
        else                  res.filter(path  => path.toFile.isDirectory)
                                 .map(path     => path.toFile.getName)
                                 .map(name     => s"/$path/$name")
                                 .toList
    }
  }

//  def getDirectories(path: String): List[String] = Files
//    .list(Paths.get(path))
//    .iterator()
//    .asScala
//    .filter(path  => path.toFile.isDirectory)
//    .map(path     => path.toFile.getName)
//    .map(name     => s"/$path/$name")
//    .toList

  def createFile(path: String, name: String): Either[FileAlreadyExistError, String] = {
    if (Files.exists(Paths.get(s"$path/$name")) == false) {
      val newFile = Paths.get(s"$path/$name")
      Files.createFile(newFile)
      Right(s"$name was created")
    } else Left(FileAlreadyExistError(s"File with name $name already exists !"))
  }

  def deleteFile(path: String, name: String): Either[FileDoesNotExistError, String] = {
    Files.deleteIfExists(Paths.get(s"$path/$name")) match {
      case true  => Right(s"$name was deleted")
      case false => Left(FileDoesNotExistError("Not Found: File or directory does not exist"))
    }
  }


  def getAllContent(path: String): List[String] = {
        List.concat(getContent(path, GetContent(Some("files"))), getContent(path, GetContent(Some("dirs"))))
  }

  def changePath(current: String, path: String): Either[ChangePathError, String] = {
    var destination: String = new String()

    if (path.equals("..")) {
      val dirs = current.split('/').init
      destination = dirs.mkString("/")
    } else destination = s"$current/$path"

    Files.isDirectory(Paths.get(destination)) match {
      case true  => Right(destination)
      case false => Left(ChangePathError(s"directory doesn't exist: $path"))
    }
  }


  def parseCommand(input: String): Command =
    input match {
        case command if command.startsWith("ll")  => ListAllContentCommand()
        case command if command.startsWith("dir") => ListDirectoryCommand()
        case command if command.startsWith("ls")  => ListFilesCommand()
        case command if command.startsWith("cd")  => ChangeDirectoryCommand(input.takeRight(input.size - 3))
        case command if command.startsWith("nf")  => CreateNewFile(input.takeRight(input.size - 3))
        case command if command.startsWith("df")  => DeleteFile(input.takeRight(input.size - 3))
    }

  def handleCommand(command: Command, currentPath: String): String =
    command match {
        case ListFilesCommand()                   => getContent(currentPath, GetContent(Some("files"))).mkString(", ")
        case ListDirectoryCommand()               => getContent(currentPath, GetContent(Some("dirs"))).mkString(", ")
        case ListAllContentCommand()              => getAllContent(currentPath).mkString(", ")
        case CreateNewFile(name)                  => createFile(currentPath, name) match {
            case Left(value)  => value.error
            case Right(value) => value
        }
        case DeleteFile(name)                     => deleteFile(currentPath, name) match {
            case Left(value)  => value.error
            case Right(value) => value
        }
        case ChangeDirectoryCommand(destination)  => changePath(currentPath, destination) match {
            case Left(value)  => value.error
            case Right(value) => value
        }
    }
  def main(basePath: String): Unit = {
      def layer(path: String): Unit = {
        val request = readLine()
        val command = parseCommand(request)
        val newPath  = handleCommand(command, path)
        println(newPath)
        if (command.isSubstitutive) layer(newPath)
        else layer(path)
      }
      layer(basePath)
  }

  main("D://")
}
