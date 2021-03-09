package io.iohk.ethereum

import cats.effect.Bracket
import cats.effect.Effect
import cats.effect.Resource
import cats.effect.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest._
import org.scalatest.diagrams.Diagrams
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.freespec.AsyncFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpecLike

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait SpecBase extends TypeCheckedTripleEquals with Diagrams with Matchers { self: AsyncTestSuite =>

  override val executionContext = ExecutionContext.global
  implicit val scheduler: Scheduler = Scheduler(executionContext)

  def customTestCaseResourceM[M[_]: Effect, T](
      fixture: Resource[M, T]
  )(theTest: T => M[Assertion])(implicit bracket: Bracket[M, Throwable]): Future[Assertion] =
    fixture.use(theTest).toIO.unsafeToFuture()

  def customTestCaseM[M[_]: Effect, T](fixture: => T)(theTest: T => M[Assertion]): Future[Assertion] =
    customTestCaseResourceM(Resource.pure[M, T](fixture))(theTest)

  def testCaseM[M[_]: Effect](theTest: => M[Assertion]): Future[Assertion] = customTestCaseM(())(_ => theTest)

  def testCase(theTest: => Assertion): Future[Assertion] = testCaseM(Task(theTest))
}

trait FlatSpecBase extends AsyncFlatSpecLike with SpecBase {}

trait FreeSpecBase extends AsyncFreeSpecLike with SpecBase {}

trait WordSpecBase extends AsyncWordSpecLike with SpecBase {}

trait SpecFixtures { self: SpecBase =>
  type Fixture

  def createFixture(): Fixture

  def testCaseM[M[_]: Effect](theTest: Fixture => M[Assertion]): Future[Assertion] =
    customTestCaseM(createFixture())(theTest)

  def testCase(theTest: Fixture => Assertion): Future[Assertion] =
    testCaseM((fixture: Fixture) => Task.pure(theTest(fixture)))
}

trait ResourceFixtures { self: SpecBase =>
  type Fixture

  def fixtureResource: Resource[Task, Fixture]

  def testCaseM[M[_]: Effect](theTest: Fixture => M[Assertion]): Future[Assertion] =
    customTestCaseResourceM(fixtureResource.mapK(Task.liftTo[M]))(theTest)

  /** Task-specific method to avoid type inference issues in [[testCaseM]]
    */
  def testCaseT(theTest: Fixture => Task[Assertion]): Future[Assertion] =
    customTestCaseResourceM(fixtureResource)(theTest)

  def testCase(theTest: Fixture => Assertion): Future[Assertion] =
    customTestCaseResourceM(fixtureResource)(fixture => Task.pure(theTest(fixture)))
}
