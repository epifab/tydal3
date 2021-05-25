package tydal.schema

class Column[Name, T](using val name: DbIdentifier[Name], dbType: DbType[T]) extends Field[T]:
  override def as[Alias <: Singleton](alias: Alias)(using DbIdentifier[Alias]): Tagged[Column[Name, T], Alias] = new Tagged(this)
  override def toString: String = name.value


trait ColumnsBuilder[Columns]:
  def get: Columns

object ColumnsBuilder:
  given empty: ColumnsBuilder[EmptyTuple] with
    def get: EmptyTuple = EmptyTuple

  given head[Name, T, Tail <: Tuple](using dbi: DbIdentifier[Name], dbt: DbType[T], builder: ColumnsBuilder[Tail]): ColumnsBuilder[Column[Name, T] *: Tail] with
    def get: Column[Name, T] *: Tail = new Column[Name, T] *: builder.get
