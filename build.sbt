name := "cryptopals"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.reflections" % "reflections" % "0.9.10"
//resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.0")

//https://bitbucket.org/vetler/minihttpserver/overview
resolvers += "Roeim.net repository" at "http://roeim.net/maven"

libraryDependencies += "net.roeim.minihttpserver" %% "minihttpserver" % "1.0"
libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.3.0"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.4.11"
libraryDependencies += "com.typesafe.akka" % "akka-contrib_2.11" % "2.4.11"