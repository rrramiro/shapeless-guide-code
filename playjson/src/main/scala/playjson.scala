import play.api.libs.json.{JsObject, _}
import shapeless.{`::` => :#:, _}


object CWrites extends LabelledTypeClassCompanion[Writes] {
  object typeClass extends LabelledTypeClass[Writes] {

    def emptyProduct: Writes[HNil] = Writes(_ => Json.obj())

    def product[F, T <: HList](name: String, FHead: Writes[F], FTail: Writes[T]) = Writes[F :#: T] {
      case head :#: tail =>
        (FHead.writes(head), FTail.writes(tail)) match {
          case (JsNull, t: JsObject)     => t
          case (h: JsValue, t: JsObject) => JsObject(Seq(name -> h)) ++ t
          case _                         => Json.obj()
        }
    }
    def project[F, G](instance: => Writes[G], to: F => G, from: G => F) = Writes[F](f => instance.writes(to(f)))

    def emptyCoproduct: Writes[CNil] = Writes(_ => JsNull)

    def coproduct[L, R <: Coproduct](name: String, cl: => Writes[L], cr: => Writes[R]) = Writes[L :+: R]{ lr =>

      val r = lr match {
        case Inl(left)  => cl writes left
        case Inr(right) => cr writes right
      }
      r match {
        case JsNull => JsString(name)
        case o      => o
      }
    }
  }
}

object CReads extends LabelledTypeClassCompanion[Reads] {
  object typeClass extends LabelledTypeClass[Reads] {

    def emptyProduct: Reads[HNil] = Reads(_ => JsSuccess(HNil))

    def product[F, T <: HList](name: String, FHead: Reads[F], FTail: Reads[T]) = Reads[F :#: T] {
      case obj @ JsObject(_) =>
        obj \ name match {
          case JsDefined(field) =>
            for {
              head <- FHead.reads(field)
              tail <- FTail.reads(obj - name)
            } yield head :: tail
          case _ =>
            JsError(s"Field $name missing")
        }
      case _ => JsError("Json object required")
    }

    def project[F, G](instance: => Reads[G], to: F => G, from: G => F) = Reads[F](instance.map(from).reads)

    def emptyCoproduct: Reads[CNil] = Reads[CNil](_ => JsError("CNil object not available"))

    def coproduct[L, R <: Coproduct](name: String, cl: => Reads[L], cr: => Reads[R]) = Reads[L :+: R] {
      js => cl.reads(js).map(Inl.apply) orElse cr.reads(js).map(Inr.apply)
    }
  }
}

object CFormats extends LabelledTypeClassCompanion[Format] {
  object typeClass extends LabelledTypeClass[Format] {
    def emptyProduct: Format[HNil] = Format(
      CReads.typeClass.emptyProduct,
      CWrites.typeClass.emptyProduct
    )

    def product[F, T <: HList](name: String, FHead: Format[F], FTail: Format[T]) = Format[F :#: T] (
      CReads.typeClass.product[F, T](name, FHead, FTail),
      CWrites.typeClass.product[F, T](name, FHead, FTail)
    )

    def project[F, G](instance: => Format[G], to: F => G, from: G => F) = Format[F](
      CReads.typeClass.project(instance, to, from),
      CWrites.typeClass.project(instance, to, from)
    )

    def emptyCoproduct = Format[CNil](
      CReads.typeClass.emptyCoproduct,
      CWrites.typeClass.emptyCoproduct
    )

    def coproduct[L, R <: Coproduct](name: String, cl: => Format[L], cr: => Format[R]) = Format[L :+: R](
      CReads.typeClass.coproduct(name, cl, cr),
      CWrites.typeClass.coproduct(name, cl, cr)
    )
  }
}

object Main extends Demo {

  implicit val formatsShape: Format[Shape] = {
    implicit val formatsRectangle: Format[Rectangle] = CFormats.deriveInstance
    implicit val formatsCircle: Format[Circle] = CFormats.deriveInstance
    CFormats.deriveInstance
  }

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val rectangle = Rectangle(1, 2)
  val circle = Circle(3)
  val shapes: List[Shape] =
    List(
      Rectangle(1, 2),
      Circle(3),
      Rectangle(4, 5),
      Circle(6)
    )

  val optShapes: List[Option[Shape]] =
    List(
      Some(Rectangle(1, 2)),
      Some(Circle(3)),
      None,
      Some(Rectangle(4, 5)),
      Some(Circle(6)),
      None
    )
  val inputRectangle = """{"width":1,"height":2}"""
  val inputCircle = """{"radius":3}"""

  println("Rectangle " + rectangle)
  println("Rectangle as Json:\n" + Json.toJson(rectangle))
  println("Circle " + circle)
  println("Circle as Json:\n" + Json.toJson(circle))
  println("Shapes " + shapes)
  println("Shapes as Json:\n" + Json.toJson(shapes))
  println("Optional shapes " + optShapes)
  println("Optional shapes as Json:\n" + Json.toJson(optShapes))
  println("Parsed Rectangle " + Json.parse(inputRectangle).as[Shape])
  println("Parsed Circle " + Json.parse(inputCircle).as[Shape])

}