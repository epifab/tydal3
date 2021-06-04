package tydal.schema

case class CompiledQuery[Input <: Tuple, Output <: Tuple](sql: String, input: Input, output: Output)

case class CompiledQueryFragment[Input <: Tuple](sql: Option[String], input: Input):
  def `++`(s: String): CompiledQueryFragment[Input] =
    append(s)

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

  given fieldExprDbFunction1[F <: Field[_], T, Output <: Tuple](
    using
    inner: QueryFragmentCompiler[FieldExprListFragment, F, Output]
  ): QueryFragmentCompiler[FieldExprListFragment, DbFunction1[F, T], Output] with
    def build(func: DbFunction1[F, T]): CompiledQueryFragment[Output] =
      inner.build(func.param).wrap(s"${func.dbName}(", ")")

  given fieldExprDbFunction2[F <: Field[_], G <: Field[_], T, Output1 <: Tuple, Output2 <: Tuple](
    using
    inner1: QueryFragmentCompiler[FieldExprListFragment, F, Output1],
    inner2: QueryFragmentCompiler[FieldExprListFragment, G, Output2]
  ): QueryFragmentCompiler[FieldExprListFragment, DbFunction2[F, G, T], Tuple.Concat[Output1, Output2]] with
    def build(func: DbFunction2[F, G, T]): CompiledQueryFragment[Tuple.Concat[Output1, Output2]] =
      (inner1.build(func.param1) `+,+` inner2.build(func.param2)).wrap(s"${func.dbName}(", ")")

  given fieldExprDbFunction3[F <: Field[_], G <: Field[_], H <: Field[_], T, Output1 <: Tuple, Output2 <: Tuple, Output3 <: Tuple](
    using
    inner1: QueryFragmentCompiler[FieldExprListFragment, F, Output1],
    inner2: QueryFragmentCompiler[FieldExprListFragment, G, Output2],
    inner3: QueryFragmentCompiler[FieldExprListFragment, H, Output3]
  ): QueryFragmentCompiler[FieldExprListFragment, DbFunction3[F, G, H, T], Tuple.Concat[Tuple.Concat[Output1, Output2], Output3]] with
    def build(func: DbFunction3[F, G, H, T]): CompiledQueryFragment[Tuple.Concat[Tuple.Concat[Output1, Output2], Output3]] =
      (inner1.build(func.param1) `+,+` inner2.build(func.param2) `+,+` inner3.build(func.param3)).wrap(s"${func.dbName}(", ")")

  given fieldExprNamedPlaceholder[P <: NamedPlaceholder[_, _]]: QueryFragmentCompiler[FieldExprListFragment, P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(s"?::${placeholder.dbType.dbName}", placeholder)

  given fieldExprLiteral[P <: Literal[_]]: QueryFragmentCompiler[FieldExprListFragment, P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(s"?::${placeholder.dbType.dbName}", placeholder)

  given fieldExprLiteralOption[P <: LiteralOption[_]]: QueryFragmentCompiler[FieldExprListFragment, P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(Option.when(placeholder.value.isDefined)(s"?::${placeholder.dbType.dbName}"), placeholder *: EmptyTuple)
