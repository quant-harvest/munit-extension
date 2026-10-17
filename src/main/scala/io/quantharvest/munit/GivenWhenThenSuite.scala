package io.quantharvest.munit

import cats.effect.{IO, Resource}
import munit.{CatsEffectSuite, FailException, Location}

/** Phase of a BDD scenario step. */
enum BddPhase(val label: String):
  case Given extends BddPhase("GIVEN")
  case When  extends BddPhase("WHEN")
  case Then  extends BddPhase("THEN")
  case And   extends BddPhase("AND")

/** Typeclass lifting pure values, `IO[A]`, or `Resource[IO, A]` into a `Resource[IO, A]`
  * so that `Given`, `When`, `Then`, and `And` seamlessly compose synchronous values,
  * async effects, and lifecycle-managed resources (e.g. Docker containers, HTTP servers, DB pools).
  */
trait StepLift[T, A]:
  def lift(thunk: => T): Resource[IO, A]

trait LowPriorityStepLift:
  given pureStepLift[A]: StepLift[A, A] with
    def lift(thunk: => A): Resource[IO, A] =
      Resource.eval(IO.delay(thunk))

object StepLift extends LowPriorityStepLift:
  given resourceStepLift[A]: StepLift[Resource[IO, A], A] with
    def lift(thunk: => Resource[IO, A]): Resource[IO, A] =
      Resource.suspend(IO.delay(thunk))

  given ioStepLift[A]: StepLift[IO[A], A] with
    def lift(thunk: => IO[A]): Resource[IO, A] =
      Resource.eval(IO.defer(thunk))

  given stepStepLift[A]: StepLift[BddStep[A], A] with
    def lift(thunk: => BddStep[A]): Resource[IO, A] =
      Resource.suspend(IO.delay(thunk.resource))

/** A composable BDD step backed by `Resource[IO, A]`.
  *
  * Supports both:
  * 1. `for`-comprehension syntax (`Given` -> `When` -> `Then` -> `And`) where all bound fixtures
  *    stay in lexical scope and resources are automatically closed when the scenario finishes.
  * 2. Fluent chained syntax (`Given(...).and(...).When(...).Then(...)`).
  */
final class BddStep[A](val resource: Resource[IO, A]):

  def map[B](f: A => B): BddStep[B] =
    new BddStep(resource.map(f))

  def flatMap[B](f: A => BddStep[B]): BddStep[B] =
    new BddStep(resource.flatMap(a => f(a).resource))

  /** Run the step lifecycle to completion as an `IO[A]`, releasing all acquired `Given` resources afterward. */
  def run: IO[A] =
    resource.use(IO.pure)

  /** Combine another independent `Given` step into a tuple. */
  def and[T, B, Out](description: String, next: => T)(using
      lift: StepLift[T, B],
      loc: Location,
      tupleAppend: TupleAppend.Aux[A, B, Out]
  ): BddStep[Out] =
    flatMap { a =>
      BddStep.step(BddPhase.And, description, next).map(b => tupleAppend.append(a, b))
    }

  /** Combine another `Given` step that depends on the prior `Given` value(s) into a tuple. */
  def andWith[T, B, Out](description: String)(f: A => T)(using
      lift: StepLift[T, B],
      loc: Location,
      tupleAppend: TupleAppend.Aux[A, B, Out]
  ): BddStep[Out] =
    flatMap { a =>
      BddStep.step(BddPhase.And, description, f(a)).map(b => tupleAppend.append(a, b))
    }

  /** Transition from `Given` to `When`, passing the accumulated `Given` context `A` into the action. */
  def When[T, W](description: String)(f: A => T)(using
      lift: StepLift[T, W],
      loc: Location
  ): WhenStep[A, W] =
    new WhenStep[A, W](
      this.flatMap { a =>
        BddStep.step(BddPhase.When, description, f(a)).map(w => (a, w))
      }
    )

object BddStep:
  def step[T, A](phase: BddPhase, description: String, body: => T)(using
      lift: StepLift[T, A],
      loc: Location
  ): BddStep[A] =
    val wrapped = lift
      .lift(body)
      .preAllocate(IO.println(s"  ${phase.label} $description"))
      .handleErrorWith { err =>
        Resource.eval(IO.raiseError(enrichFailure(phase, description, err, loc)))
      }
    new BddStep(wrapped)

  private def enrichFailure(
      phase: BddPhase,
      description: String,
      err: Throwable,
      loc: Location
  ): Throwable =
    val prefix = s"[${phase.label} $description]"
    err match
      case fe: FailException =>
        if fe.getMessage != null && fe.getMessage.startsWith("[") then fe
        else new FailException(s"$prefix ${fe.getMessage}", fe.cause, fe.isStackTracesEnabled, fe.location)
      case other =>
        if other.getMessage != null && other.getMessage.startsWith("[") then other
        else new FailException(s"$prefix ${other.getMessage}", other, true, loc)

/** Represents the completed `When` phase holding both the `Given` environment `G` and `When` outcome `W`. */
final class WhenStep[G, W](val underlying: BddStep[(G, W)]):

  /** Validate the `When` result `W` (or `G` when called via `ThenEnv`). */
  def Then[T, R](description: String)(assertion: W => T)(using
      lift: StepLift[T, R],
      loc: Location
  ): ThenStep[G, W] =
    val next = underlying.flatMap { (g, w) =>
      BddStep.step(BddPhase.Then, description, assertion(w)).map(_ => (g, w))
    }
    new ThenStep[G, W](next)

  /** Validate using both the `Given` context `G` (e.g. app/database handles) and the `When` outcome `W`. */
  def ThenWithGiven[T, R](description: String)(assertion: (G, W) => T)(using
      lift: StepLift[T, R],
      loc: Location
  ): ThenStep[G, W] =
    val next = underlying.flatMap { (g, w) =>
      BddStep.step(BddPhase.Then, description, assertion(g, w)).map(_ => (g, w))
    }
    new ThenStep[G, W](next)

/** Represents the `Then` validation phase, supporting additional `.and(...)` assertions. */
final class ThenStep[G, W](val underlying: BddStep[(G, W)]):

  def and[T, R](description: String)(assertion: W => T)(using
      lift: StepLift[T, R],
      loc: Location
  ): ThenStep[G, W] =
    val next = underlying.flatMap { (g, w) =>
      BddStep.step(BddPhase.And, description, assertion(w)).map(_ => (g, w))
    }
    new ThenStep[G, W](next)

  def andWithGiven[T, R](description: String)(assertion: (G, W) => T)(using
      lift: StepLift[T, R],
      loc: Location
  ): ThenStep[G, W] =
    val next = underlying.flatMap { (g, w) =>
      BddStep.step(BddPhase.And, description, assertion(g, w)).map(_ => (g, w))
    }
    new ThenStep[G, W](next)

  def run: IO[Unit] =
    underlying.run.void

/** Type-level helper that flattens chained `.and` steps into a flat Scala 3 tuple `(A, B, C, ...)`
  * rather than nested `((A, B), C)` pairs.
  */
trait TupleAppend[A, B]:
  type Out
  def append(a: A, b: B): Out

trait LowPriorityTupleAppend:
  given singleToPair[A, B]: TupleAppend.Aux[A, B, (A, B)] =
    new TupleAppend[A, B]:
      type Out = (A, B)
      def append(a: A, b: B): (A, B) = (a, b)

object TupleAppend extends LowPriorityTupleAppend:
  type Aux[A, B, O] = TupleAppend[A, B] { type Out = O }

  given appendToTuple[A <: Tuple, B]: TupleAppend.Aux[A, B, Tuple.Append[A, B]] =
    new TupleAppend[A, B]:
      type Out = Tuple.Append[A, B]
      def append(a: A, b: B): Tuple.Append[A, B] = a :* b

/** Base MUnit suite providing Given / When / Then BDD syntax on top of `CatsEffectSuite`. */
abstract class GivenWhenThenSuite extends CatsEffectSuite:

  /** Register a BDD scenario test in MUnit. */
  def scenario(name: String)(body: => Any)(using loc: Location): Unit =
    test(name)(body)

  /** Define a `Given` precondition or environment resource (`Resource[IO, A]`, `IO[A]`, or pure `A`). */
  def Given[T, A](description: String, setup: => T)(using
      lift: StepLift[T, A],
      loc: Location
  ): BddStep[A] =
    BddStep.step(BddPhase.Given, description, setup)

  /** Define a `When` action step (`Resource[IO, A]`, `IO[A]`, or pure `A`). */
  def When[T, A](description: String, action: => T)(using
      lift: StepLift[T, A],
      loc: Location
  ): BddStep[A] =
    BddStep.step(BddPhase.When, description, action)

  /** Define a `Then` verification step (`Resource[IO, A]`, `IO[A]`, or pure `A`). */
  def Then[T, A](description: String, assertion: => T)(using
      lift: StepLift[T, A],
      loc: Location
  ): BddStep[A] =
    BddStep.step(BddPhase.Then, description, assertion)

  /** Define an `And` continuation step (`Resource[IO, A]`, `IO[A]`, or pure `A`). */
  def And[T, A](description: String, continuation: => T)(using
      lift: StepLift[T, A],
      loc: Location
  ): BddStep[A] =
    BddStep.step(BddPhase.And, description, continuation)

  override def munitValueTransforms: List[ValueTransform] =
    super.munitValueTransforms ++ List(
      new ValueTransform(
        "BddStep",
        {
          case step: BddStep[?]   => step.run.unsafeToFuture()(using munitIORuntime)
          case when: WhenStep[?, ?] => when.underlying.run.unsafeToFuture()(using munitIORuntime)
          case thenStep: ThenStep[?, ?] => thenStep.run.unsafeToFuture()(using munitIORuntime)
        }
      )
    )
