# munit-extension

Lightweight Given / When / Then BDD extension for [MUnit](https://scalameta.org/munit/) and [`munit-cats-effect`](https://github.com/typelevel/munit-cats-effect) on Scala 3.

## Usage

```scala
libraryDependencies += "io.quantharvest" %% "munit-extension" % "0.1.0-SNAPSHOT" % Test
```

Extend `GivenWhenThenSuite` and write scenarios using either `for`-comprehensions or fluent `.and` / `.When` / `.Then` chaining. Every step accepts pure values (`A`), effects (`IO[A]`), or lifecycle-managed resources (`Resource[IO, A]`):

```scala
import io.quantharvest.munit.GivenWhenThenSuite

class MyScenarioSpec extends GivenWhenThenSuite:

  scenario("VWAP expansion records paper trades") {
    for
      tickStream <- Given("tickstream is running", tickStreamResource)
      app        <- Given("app is running", appResource(tickStream))
      portfolio  <- And("an empty portfolio", emptyPortfolio(app))
      _          <- When("market session executes", app.runSession())
      trades     <- Then("paper trades are queried from the database", app.loadPaperTrades())
      _          <- And("paper trades are non-empty", assert(trades.nonEmpty))
    yield ()
  }
```
