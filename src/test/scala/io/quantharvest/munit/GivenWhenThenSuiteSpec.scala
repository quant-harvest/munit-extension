package io.quantharvest.munit

import cats.effect.{IO, Ref, Resource}
import munit.FailException

class GivenWhenThenSuiteSpec extends GivenWhenThenSuite:

  scenario("for-comprehension Given / When / Then manages Resource lifecycles and validates state") {
    for
      closedRef <- Given("a lifecycle tracker", Ref.of[IO, Boolean](false))
      service   <- Given(
                     "a managed external service resource",
                     Resource.make(IO.pure("http://localhost:8081"))(_ => closedRef.set(true))
                   )
      portfolio <- And("an empty initial portfolio", Ref.of[IO, List[String]](Nil))
      _         <- When("a trade is executed against the service", portfolio.update(_ :+ s"TRADE@$service"))
      trades    <- Then("the trade is recorded in the portfolio", portfolio.get)
      _         <- And("exactly one paper trade is present", assertEquals(trades, List("TRADE@http://localhost:8081")))
      isClosed  <- And("the service resource is still open during Then assertions", closedRef.get)
      _         <- And("resource has not yet closed before scenario exit", assertEquals(isClosed, false))
    yield ()
  }

  scenario("fluent Given.and.When.Then chains flat tuples and releases resources") {
    var released = false
    val tickStreamRes = Resource.make(IO.pure("tickstream-ready"))(_ => IO { released = true })

    Given("tickstream is running", tickStreamRes)
      .and("app is running in-process", IO.pure("app-ready"))
      .and("portfolio is empty", List.empty[String])
      .When("session executes") { (ts, app, portfolio) =>
        IO.pure(portfolio :+ s"$ts:$app:QQQ_CALL")
      }
      .Then("paper trades are persisted") { trades =>
        assertEquals(trades, List("tickstream-ready:app-ready:QQQ_CALL"))
        assertEquals(released, false)
      }
      .andWithGiven("given environment remains accessible") { (givenTuple, trades) =>
        val (ts, app, initialPortfolio) = givenTuple
        assertEquals(ts, "tickstream-ready")
        assertEquals(app, "app-ready")
        assertEquals(initialPortfolio, Nil)
        assertEquals(trades.size, 1)
      }
  }

  scenario("step failures include the failing BDD phase and description") {
    val failingStep =
      for
        x <- Given("an initial number", 10)
        y <- When("multiplied by two", x * 2)
        _ <- Then("it equals 99", assertEquals(y, 99))
      yield ()

    failingStep.run.attempt.map {
      case Left(fe: FailException) =>
        assert(
          fe.getMessage.startsWith("[THEN it equals 99]"),
          s"Expected failure message to start with '[THEN it equals 99]', got: ${fe.getMessage}"
        )
      case other =>
        fail(s"Expected FailException, got: $other")
    }
  }
