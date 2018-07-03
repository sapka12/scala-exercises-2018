import errorhandling.Option

sealed trait Sex
case object Female extends Sex
case object Male extends Sex

case class User(name: String, age: Int, sex: Option[Sex])
case class Form(user: User, approved: Boolean)
