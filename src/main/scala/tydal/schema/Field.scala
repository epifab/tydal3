package tydal.schema

trait Field[T] extends Taggable:
  def dbType: DbType[T]


trait FieldT[-F, T]:
  def get(f: F): Field[T]

object FieldT:
  given pure[T]: FieldT[Field[T], T] with
    def get(field: Field[T]): Field[T] = field
