package tydal.schema

case class CompiledQuery[Input <: Tuple, Output <: Tuple](sql: String, input: Input, output: Output)

case class CompiledQueryFragment[Input <: Tuple](sql: Option[String], input: Input):

  def `++`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Tuple.Concat[Input, I2]] =
    concatenateOptional(other, "")

  def `+ +`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Tuple.Concat[Input, I2]] =
    concatenateOptional(other, " ")

  def `+,+`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Tuple.Concat[Input, I2]] =
    concatenateOptional(other, ", ")

  def wrap(before: String, after: String): CompiledQueryFragment[Input] = map(before + _ + after)

  def append(after: String): CompiledQueryFragment[Input] = map(_ + after)

  def prepend(before: String): CompiledQueryFragment[Input] = map(before + _)

  def map(f: String => String): CompiledQueryFragment[Input] = CompiledQueryFragment(sql.map(f), input)

  def mapInput[I2 <: Tuple](f: Input => I2): CompiledQueryFragment[I2] = CompiledQueryFragment(sql, f(input))

  def concatenateOptional[I2 <: Tuple](other: CompiledQueryFragment[I2], separator: String): CompiledQueryFragment[Tuple.Concat[Input, I2]] =
    CompiledQueryFragment(
      Seq(sql, other.sql)
        .flatten
        .reduceOption(_ ++ separator ++ _),
      input ++ other.input
    )

  def concatenateRequired[I2 <: Tuple](other: CompiledQueryFragment[I2], separator: String): CompiledQueryFragment[Tuple.Concat[Input, I2]] =
    CompiledQueryFragment(
      for {
        s1 <- sql
        s2 <- other.sql
      } yield s1 + separator + s2,
      input ++ other.input
    )

  def getOrElse[Output <: Tuple](default: String, output: Output): CompiledQuery[Input, Output] =
    CompiledQuery(sql.getOrElse(default), input, output)

  def orElse(s: Option[String]): CompiledQueryFragment[Input] =
    new CompiledQueryFragment(sql.orElse(s), input)

  def get[Output <: Tuple](output: Output): CompiledQuery[Input, Output] =
    CompiledQuery(sql.get, input, output)


object CompiledQueryFragment:
  def apply(sql: String): CompiledQueryFragment[EmptyTuple] =
    CompiledQueryFragment(Some(sql), EmptyTuple)

  def apply[P <: Placeholder[_]](sql: String, placeholder: P): CompiledQueryFragment[P *: EmptyTuple] =
    CompiledQueryFragment(Some(sql), placeholder *: EmptyTuple)


trait QueryCompiler[Query, Input <: Tuple, Output <: Tuple]:
  def build(query: Query): CompiledQuery[Input, Output]

trait FragmentType

trait FieldExprAndAliasListFragment extends FragmentType
trait FieldExprListFragment extends FragmentType
trait FieldAliasEqualExprListFragment extends FragmentType
trait FromFragment extends FragmentType
trait WhereFragment extends FragmentType
trait AliasListFragment extends FragmentType
trait SortByFragment extends FragmentType
trait LimitPlaceholderFragment extends FragmentType
trait OffsetPlaceholderFragment extends FragmentType
trait ConflictPolicyFragment extends FragmentType


trait QueryFragmentCompiler[F <: FragmentType, Target, Input <: Tuple]:
  def build(x: Target): CompiledQueryFragment[Input]

  // ------------------------------
  // expr
  // ------------------------------

  given fieldExprTagged[F <: Field[_], A, Output <: Tuple](
    using
    inner: QueryFragmentCompiler[FieldExprListFragment, F, Output]
  ): QueryFragmentCompiler[FieldExprListFragment, Tagged[F, A], Output] with
    def build(field: Tagged[F, A]): CompiledQueryFragment[Output] = inner.build(field.item)

  given fieldExprFieldRef[Src, Alias, T]: QueryFragmentCompiler[FieldExprListFragment, FieldRef[Src, Alias, T], EmptyTuple] with
    def build(field: FieldRef[Src, Alias, T]): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(s"${field.src.value}.${field.name.value}")

  given fieldExprCast[F <: Field[_], Output <: Tuple](
    using
    inner: QueryFragmentCompiler[FieldExprListFragment, F, Output]
  ): QueryFragmentCompiler[FieldExprListFragment, Cast[F, _], Output] with
    def build(field: Cast[F, _]): CompiledQueryFragment[Output] = inner.build(field.field).append("::" + field.dbType.dbName)

  given fieldExprSoftCast[F <: Field[_], T, Output <: Tuple](
    using
    inner: QueryFragmentCompiler[FieldExprListFragment, F, Output]
  ): QueryFragmentCompiler[FieldExprListFragment, SoftCast[F, T], Output] with
    def build(field: SoftCast[F, T]): CompiledQueryFragment[Output] = inner.build(field.field)

  given fieldExprDbFunction[FS <: Tuple, T, Output <: Tuple](
    using
    inner: QueryFragmentCompiler[FieldExprListFragment, FS, Output]
  ): QueryFragmentCompiler[FieldExprListFragment, DbFunction[FS, T], Output] with
    def build(func: DbFunction[FS, T]): CompiledQueryFragment[Output] =
      inner.build(func.params).wrap(s"${func.dbName}(", ")")

  given fieldExprNamedPlaceholder[P <: NamedPlaceholder[_, _]]: QueryFragmentCompiler[FieldExprListFragment, P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(s"?::${placeholder.dbType.dbName}", placeholder)

  given fieldExprLiteral[P <: Literal[_]]: QueryFragmentCompiler[FieldExprListFragment, P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(s"?::${placeholder.dbType.dbName}", placeholder)

  given fieldExprLiteralOption[P <: LiteralOption[_]]: QueryFragmentCompiler[FieldExprListFragment, P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(Option.when(placeholder.value.isDefined)(s"?::${placeholder.dbType.dbName}"), placeholder *: EmptyTuple)

  given fieldExprEmptyTuple: QueryFragmentCompiler[FieldExprListFragment, EmptyTuple, EmptyTuple] with
    def build(et: EmptyTuple): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(None, et)

  given fieldExprNonEmptyTuple[Head, HeadOutput <: Tuple, Tail <: Tuple, TailOutput <: Tuple](
    using
    head: QueryFragmentCompiler[FieldExprListFragment, Head, HeadOutput],
    tail: QueryFragmentCompiler[FieldExprListFragment, Tail, TailOutput]
  ): QueryFragmentCompiler[FieldExprListFragment, Head *: Tail, Tuple.Concat[HeadOutput, TailOutput]] with
    def build(tuple: Head *: Tail): CompiledQueryFragment[Tuple.Concat[HeadOutput, TailOutput]] =
      head.build(tuple.head) `+,+` tail.build(tuple.tail)
