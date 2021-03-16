import sbt._

object Dependencies {
  lazy val mUnit = "org.scalameta" %% "munit" % "0.7.22"
  lazy val mUnitScalaCheck = "org.scalameta" %% "munit-scalacheck" % "0.7.22"
  lazy val mUnitDiscipline = "org.typelevel" %% "discipline-munit" % "1.0.6"
  lazy val cats = "org.typelevel" %% "cats-core" % "2.1.1"
  lazy val spire = "org.typelevel" %% "spire" % "0.17.0"
  lazy val catsLaws = "org.typelevel" %% "cats-laws" % "2.0.0"
  lazy val scalaCheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3"
}
