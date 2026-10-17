val Scala3Version = "3.3.6"
val MunitVersion = "1.1.1"
val MunitCatsEffectVersion = "2.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "io.quantharvest",
    name := "munit-extension",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := Scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"             % MunitVersion,
      "org.typelevel" %% "munit-cats-effect" % MunitCatsEffectVersion
    )
  )
