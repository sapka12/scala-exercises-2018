package errorhandling.validated

import cats.data.Validated._
import cats.data._
import cats.implicits._
import form.{Female, Male, Sex, User}
import org.scalatest.{FlatSpec, Matchers}

class ValidatedSpec extends FlatSpec with Matchers {

  private def optimalLength(name: String) = 5 < name.length && name.length  < 20
  private def adult(age: Int) = age >= 18
  private def female(sex: Sex) = sex == Female

  private val NAME_ERROR_MSG = "name length is not optimal"
  private val AGE_ERROR_MSG = "age is not valid"
  private val SEX_ERROR_MSG = "should be female"

  private def eitherName(name: String) = if(optimalLength(name)) Right(name) else Left(NAME_ERROR_MSG)
  private def eitherAge(age: Int) = if (adult(age)) Right(age) else Left(AGE_ERROR_MSG)
  private def eitherSex(s: Option[Sex]) = s match {
    case Some(sex) if female(sex) => Right(sex)
    case _ => Left(SEX_ERROR_MSG)
  }


  behavior of "option"

  it should "do a simple validation" in {
    def createAdultFemaleUser(name: String, age: Int, sex: Option[Sex] = None): Option[User] = ???

    createAdultFemaleUser("John", 10) shouldBe None
    createAdultFemaleUser("John Doe", 10) shouldBe None
    createAdultFemaleUser("Jane Doe", 10, Some(Female)) shouldBe None
    createAdultFemaleUser("Jane Doe", 20, Some(Female)) shouldBe Some(User("Jane Doe", 20, Some(Female)))
  }

  behavior of "either"

  it should "do validation with error message" in {

    def createAdultFemaleUser(name: String, age: Int, sex: Option[Sex] = None): Either[String, User] = ???

    createAdultFemaleUser("John", 10) shouldBe Left(NAME_ERROR_MSG)
    createAdultFemaleUser("John Doe", 10) shouldBe Left(AGE_ERROR_MSG)
    createAdultFemaleUser("Jane Doe", 10, Some(Female)) shouldBe Left(AGE_ERROR_MSG)
    createAdultFemaleUser("Jane Doe", 20, Some(Male)) shouldBe Left(SEX_ERROR_MSG)
    createAdultFemaleUser("Jane Doe", 20, Some(Female)) shouldBe Right(User("Jane Doe", 20, Some(Female)))
  }


  behavior of "validated"

  it should "do validation with error message(s)" in {
    def createAdultFemaleUser(name: String, age: Int,  sex: Option[Sex] = None): Validated[NonEmptyList[String], User] = ???

    createAdultFemaleUser("John", 10) shouldBe
      Invalid(NonEmptyList.of(NAME_ERROR_MSG, AGE_ERROR_MSG, SEX_ERROR_MSG))

    createAdultFemaleUser("John Doe", 10) shouldBe
      Invalid(NonEmptyList.of(AGE_ERROR_MSG, SEX_ERROR_MSG))

    createAdultFemaleUser("Jane Doe", 10, Some(Female)) shouldBe
      Invalid(NonEmptyList.of(AGE_ERROR_MSG))

    createAdultFemaleUser("Jane Doe", 20, Some(Male)) shouldBe
      Invalid(NonEmptyList.of(SEX_ERROR_MSG))

    createAdultFemaleUser("Jane Doe", 20, Some(Female)) shouldBe
      Valid(User("Jane Doe", 20, Some(Female)))
  }


}
