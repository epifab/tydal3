package tydal.test

import tydal.SessionAware
import cats.effect.unsafe.IORuntime

trait IntegrationTesting extends SessionAware:
  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global
