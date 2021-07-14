package tydal.postgis

import skunk.Codec
import skunk.data.Type
import tydal._

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
  val PointRegex = "\\(([+-]?[0-9]*[.]?[0-9]+),([+-]?[0-9]*[.]?[0-9]+)\\)".r

  given DbType[point] with
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
