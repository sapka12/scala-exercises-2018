//https://apiumhub.com/tech-blog-barcelona/scala-type-bounds/
//https://apiumhub.com/tech-blog-barcelona/scala-generics-covariance-contravariance

trait Thing
trait Vehicle extends Thing
class Car extends Vehicle
class Jeep extends Car
class Coupe extends Car
class Motorcycle extends Vehicle
class Vegetable

class Parking[A](val place: A)


object Example {
  new Parking[Motorcycle](new Motorcycle)

//  compile error
//  new Parking[Motorcycle](new Car)

  val p = new Parking[Car](new Jeep)
//  compile error
//  val jeep: Jeep = p.place

  class Parking2[A](val place1: A, val place2: A)
  val p2 = new Parking2(new Car, new Motorcycle)
//  p2 type ???

  class ParkingUpperTypeBound[A <: Vehicle](val place: A)
  new ParkingUpperTypeBound[Vehicle](new Jeep)
  new ParkingUpperTypeBound[Car](new Jeep)
  new ParkingUpperTypeBound[Jeep](new Jeep)
  new ParkingUpperTypeBound[Motorcycle](new Motorcycle)
//  new ParkingUpperTypeBound[Vegetable](new Vegetable)


}
