package errorhandling.validated

import cats.data._
import cats.implicits._
import cats.data.Validated._
import form.{Female, Male, Sex, User}
import org.scalatest.{FlatSpec, Matchers}

class ValidatedSpec extends FlatSpec with Matchers {

  private def optimalLength(name: String) = 5 < name.length && name.length  < 20
  private def adult(age: Int) = age >= 18
  private def female(sex: Sex) = sex == Female

  private val NAME_ERROR_MSG = "name length is not optimal"
  private val AGE_ERROR_MSG = "age is not valid"
  private val SEX_ERROR_MSG = "should be female"

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

    def validName(name: String): Validated[NonEmptyList[String], String] = if(optimalLength(name)) name.validNel else NAME_ERROR_MSG.invalidNel
    def validAge(age: Int): Validated[NonEmptyList[String], Int] = ???
    def validSex(sex: Option[Sex]): Validated[NonEmptyList[String], Some[Sex]] = ???

    def createAdultFemaleUser(name: String, age: Int,  sex: Option[Sex] = None): Validated[NonEmptyList[String], User] =
      (validName(name), validAge(age), validSex(sex)).mapN(User)


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
