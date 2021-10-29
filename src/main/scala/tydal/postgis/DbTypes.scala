package tydal.postgis

import skunk.Codec
import skunk.data.Type
import tydal.*

trait DbTypes

type geography
type geometry
type point

object geography:
  given DbType[geography] with
    type Out = String
    override val codec: Codec[String] = Codec.simple(identity, Right(_), Type("geography"))
    override val dbName: String = "geography"

object geometry:
  given DbType[geometry] with
    type Out = String
    override val codec: Codec[String] = Codec.simple(identity, Right(_), Type("geometry"))
    override val dbName: String = "geometry"

object point:
  def apply(x: Double, y: Double): Const[point] = Const(dbt, (x, y))

  val PointRegex = "\\(([+-]?[0-9]*[.]?[0-9]+),([+-]?[0-9]*[.]?[0-9]+)\\)".r

  given dbt: DbType[point] with
    type Out = (Double, Double)
    override val codec: Codec[(Double, Double)] = Codec.simple(
      { case (lat, lng) => s"($lat,$lng)" },
      {
        case PointRegex(lat, lng) => Right((lat.toDouble, lng.toDouble))
        case _ => Left("Could not parse a point")
      },
      Type("point")
    )
    override val dbName: String = "point"


trait IsGeography[-T]

object IsGeography:
  given IsGeography[geography] with { }
  given IsGeography[nullable[geography]] with { }
  given[T: IsGeography]: IsGeography[Field[T]] with { }


trait IsGeometry[-T]

object IsGeometry:
  given IsGeometry[geometry] with { }
  given IsGeometry[nullable[geometry]] with { }
  given[T: IsGeometry]: IsGeometry[Field[T]] with { }


trait IsPoint[-T]

trait PointLike:
  // It's possible to get latitude and longitude out of a geometry straight away
  given geometry [T: IsGeometry]: IsPoint[T] with { }

object IsPoint extends PointLike:
  given IsPoint[point] with { }
  given IsPoint[nullable[point]] with { }
  given[T: IsPoint]: IsPoint[Field[T]] with { }

