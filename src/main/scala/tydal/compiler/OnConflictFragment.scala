package tydal.compiler

import tydal.OnConflict._
import tydal._

import scala.Tuple.Concat

trait OnConflictFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object OnConflictFragment:
  given OnConflictFragment[OnConflict.ThrowException, EmptyTuple] with
    override def build(x: OnConflict.ThrowException): CompiledFragment[EmptyTuple] = CompiledFragment.empty

  given [Fx, T <: Tuple](using fields: ListFragment[FieldFragment, Fx, T]): OnConflictFragment[DoNothing[Fx], T] with
    override def build(x: DoNothing[Fx]): CompiledFragment[T] = fields.build(x.fields, ", ").wrap("ON CONFLICT (", ")") ++ "DO NOTHING"

  given [Fx, T1 <: Tuple, KeyValues, T2 <: Tuple](using fields: ListFragment[FieldFragment, Fx, T1], keyValues: KeyValueFragment[KeyValues, T2]): OnConflictFragment[DoUpdate[Fx, KeyValues], T1 Concat T2] with
    override def build(x: DoUpdate[Fx, KeyValues]): CompiledFragment[T1 Concat T2] = fields.build(x.fields, ", ").wrap("ON CONFLICT (", ")") ++ "DO UPDATE SET" ++ keyValues.build(x.keyValues)
