package one.lab.tasks.week.three

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

import scala.util.{Failure, Success, Try}
import RestClientImpl._
import akka.http.javadsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, Uri}

object Githuber extends App {
  implicit val system: ActorSystem                        = ActorSystem("lalka")
  implicit val materializer: ActorMaterializer            = ActorMaterializer.create(system)
  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
  implicit val defaultFormats: DefaultFormats.type        = DefaultFormats

  // TODO: поля можете добавить какие хотите
  case class GithubUser(id: Int, login: String, avatarUrl: Option[String], reposUrl: String, email: Option[String])
  case class GithubRepository(name: String, stargazersCount: Int, size: Int, forks: Int)

  //  https://api.github.com/users/{$USER}
  def getGithubUser(username: String): Future[GithubUser] = {
    val url: String = s"https://api.github.com/users/$username"
    get(url).map(body => parse(body).camelizeKeys.extract[GithubUser])
  }

  def getUserRepositories(repoUrl: String): Future[List[GithubRepository]] = {
    get(repoUrl).map(body => parse(body).camelizeKeys.extract[List[GithubRepository]])
  }

  def getUserInfo(username: String): Unit = {
    val githubUser = getGithubUser(username)
    githubUser.onComplete {
      case Success(value)  =>
        val userRepos = getUserRepositories(value.reposUrl)
        userRepos onComplete {
          case Success(valueRepos) =>
            println(s"${value.login} has ${valueRepos.size} repositories")
            valueRepos.foreach(body => println(s"${body.name}: has ${body.stargazersCount} stars, ${body.size} KB size, repo forked ${body.forks} times."))
          case Failure(errorRepos) => println(errorRepos.getMessage)
        }
      case Failure(error)  => println(error.getMessage)
    }
  }

  getUserInfo("maxovic")
}
