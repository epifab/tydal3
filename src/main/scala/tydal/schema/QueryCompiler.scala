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

trait QueryFragmentCompiler[-Target, Input <: Tuple]:
  def build(x: Target): CompiledQueryFragment[Input]

trait CommaSeparatedListFragment[BaseCompiler[a, b <: Tuple] <: QueryFragmentCompiler[a, b], Target, Input <: Tuple] extends QueryFragmentCompiler[Target, Input]

trait FieldExprFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]
trait FieldExprAsAliasFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]

//trait FieldAliasEqualExprListFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]
//trait FromFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]
//trait WhereFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]
//trait AliasListFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]
//trait SortByFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]
//trait LimitPlaceholderFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]
//trait OffsetPlaceholderFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]
//trait ConflictPolicyFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]

object CommaSeparatedListFragment:
  given emptyTuple[BaseCompiler[a, b <: Tuple] <: QueryFragmentCompiler[a, b]]: CommaSeparatedListFragment[BaseCompiler, EmptyTuple, EmptyTuple] with
    override def build(x: EmptyTuple): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(None, x)

  given nonEmptyTuple[BaseCompiler[a, b <: Tuple] <: QueryFragmentCompiler[a, b], HeadInput, HeadOutput <: Tuple, TailInput <: Tuple, TailOutput <: Tuple](
    using
    headCompiler: BaseCompiler[HeadInput, HeadOutput],
    tailCompier: CommaSeparatedListFragment[BaseCompiler, TailInput, TailOutput]
  ): CommaSeparatedListFragment[BaseCompiler, HeadInput *: TailInput, Tuple.Concat[HeadOutput, TailOutput]] with
    def build(tuple: HeadInput *: TailInput): CompiledQueryFragment[Tuple.Concat[HeadOutput, TailOutput]] =
      headCompiler.build(tuple.head) `+,+` tailCompier.build(tuple.tail)


object FieldExprFragment:

  given tagged[F <: Field[_], A, Output <: Tuple] (
    using
    inner: FieldExprFragment[F, Output]
  ): FieldExprFragment[Tagged[F, A], Output] with
    def build(field: Tagged[F, A]): CompiledQueryFragment[Output] = inner.build(field.item)

  given fieldRef[Src, Alias, T]: FieldExprFragment[FieldRef[Src, Alias, T], EmptyTuple] with
    def build(field: FieldRef[Src, Alias, T]): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(s"${field.src.value}.${field.name.value}")

  given cast[F <: Field[_], Output <: Tuple] (
    using
    inner: FieldExprFragment[F, Output]
  ): FieldExprFragment[Cast[F, _], Output] with
    def build(field: Cast[F, _]): CompiledQueryFragment[Output] = inner.build(field.field).append("::" + field.dbType.dbName)

  given softCast[F <: Field[_], T, Output <: Tuple] (
    using
    inner: FieldExprFragment[F, Output]
  ): FieldExprFragment[SoftCast[F, T], Output] with
    def build(field: SoftCast[F, T]): CompiledQueryFragment[Output] = inner.build(field.field)

  given dbFunction[FS <: Tuple, T, Output <: Tuple] (
    using
    inner: FieldExprFragment[FS, Output]
  ): FieldExprFragment[DbFunction[FS, T], Output] with
    def build(func: DbFunction[FS, T]): CompiledQueryFragment[Output] =
      inner.build(func.params).wrap(s"${func.dbName}(", ")")

  given namedPlaceholder[P <: NamedPlaceholder[_, _]]: FieldExprFragment[P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(s"?::${placeholder.dbType.dbName}", placeholder)

  given literal[P <: Literal[_]]: FieldExprFragment[P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(s"?::${placeholder.dbType.dbName}", placeholder)

  given literalOption[P <: LiteralOption[_]]: FieldExprFragment[P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(Option.when(placeholder.value.isDefined)(s"?::${placeholder.dbType.dbName}"), placeholder *: EmptyTuple)


object FieldExprAsAliasFragment:

  given[A, F <: Field[_], O <: Tuple](using field: FieldExprFragment[F, O]): FieldExprAsAliasFragment[Tagged[F, A], O] with
    def build(x: Tagged[F, A]): CompiledQueryFragment[O] = field.build(x.item).append(s".${x.tag.value}")

  given[A, F <: Field[_], O <: Tuple](using field: FieldExprFragment[F, O]): FieldExprAsAliasFragment[F, O] with
    def build(x: F): CompiledQueryFragment[O] = field.build(x)
