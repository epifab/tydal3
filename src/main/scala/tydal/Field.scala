package tydal

trait Field[T]:
  given dbType: DbType[T]

  def as[A](tag: A)(using DbIdentifier[tag.type]): Aliased[T, this.type, tag.type] = Aliased(this)
  
  def castTo[B: DbType]: Cast[this.type, B] = Cast(this)

  def ===[G <: Field[_]](right: G)(using AreComparable[this.type, G]): Equals[this.type, G] = Equals(this, right)

  def ===[A <: String with Singleton: DbIdentifier](right: A)(
    using
    AreComparable[this.type, Placeholder[A, T]]
  ): Equals[this.type, Placeholder[A, T]] = Equals(this, Placeholder[A, T])

  def !==[G <: Field[_]](right: G)(using AreComparable[this.type, G]): NotEquals[this.type, G] = NotEquals(this, right)

  def !==[A <: String with Singleton: DbIdentifier](right: A)(
    using
    AreComparable[this.type, Placeholder[A, T]]
  ): NotEquals[this.type, Placeholder[A, T]] = NotEquals(this, Placeholder[A, T])

  def >[G <: Field[_]](right: G)(using AreComparable[this.type, G]): GreaterThan[this.type, G] = GreaterThan(this, right)

  def >[A <: String with Singleton: DbIdentifier](right: A)(
    using
    AreComparable[this.type, Placeholder[A, T]]
  ): GreaterThan[this.type, Placeholder[A, T]] = GreaterThan(this, Placeholder[A, T])

  def <[G <: Field[_]](right: G)(using AreComparable[this.type, G]): LessThan[this.type, G] = LessThan(this, right)

  def <[A <: String with Singleton: DbIdentifier](right: A)(
    using
    AreComparable[this.type, Placeholder[A, T]]
  ): LessThan[this.type, Placeholder[A, T]] = LessThan(this, Placeholder[A, T])

  def >=[G <: Field[_]](right: G)(using AreComparable[this.type, G]): GreaterThanOrEqual[this.type, G] = GreaterThanOrEqual(this, right)

  def >=[A <: String with Singleton: DbIdentifier](right: A)(
    using
    AreComparable[this.type, Placeholder[A, T]]
  ): GreaterThanOrEqual[this.type, Placeholder[A, T]] = GreaterThanOrEqual(this, Placeholder[A, T])

  def <=[G <: Field[_]](right: G)(using AreComparable[this.type, G]): LessThanOrEqual[this.type, G] = LessThanOrEqual(this, right)

  def <=[A <: String with Singleton: DbIdentifier](right: A)(
    using
    AreComparable[this.type, Placeholder[A, T]]
  ): LessThanOrEqual[this.type, Placeholder[A, T]] = LessThanOrEqual(this, Placeholder[A, T])

  def like[G <: Field[_]](right: G)(using IsText[T], IsText[G]): Like[this.type, G] = Like(this, right)

  def like[A <: String with Singleton: DbIdentifier](right: A)(using IsText[T]): Like[this.type, Placeholder[A, T]] =
    Like(this, Placeholder[A, T])

  def ilike[G <: Field[_]](right: G)(using IsText[T], IsText[G]): ILike[this.type, G] = ILike(this, right)

  def ilike[A <: String with Singleton: DbIdentifier](right: A)(using IsText[T]): ILike[this.type, Placeholder[A, T]] =
    ILike(this, Placeholder[A, T])

  def subsetOf[G <: Field[_]](right: G)(using AreComparableArray[this.type, G]): IsSubset[this.type, G] = IsSubset(this, right)

  def subsetOf[A <: String with Singleton: DbIdentifier](right: A)(using AreComparableArray[this.type, Placeholder[A, T]]): IsSubset[this.type, Placeholder[A, T]] =
    IsSubset(this, Placeholder[A, T])

  def supersetOf[G <: Field[_]](right: G)(using AreComparableArray[this.type, G]): IsSuperset[this.type, G] = IsSuperset(this, right)

  def supersetOf[A <: String with Singleton: DbIdentifier](right: A)(using AreComparableArray[this.type, Placeholder[A, T]]): IsSuperset[this.type, Placeholder[A, T]] =
    IsSuperset(this, Placeholder[A, T])

  def overlaps[G <: Field[_]](right: G)(using AreComparableArray[this.type, G]): Overlaps[this.type, G] = Overlaps(this, right)

  def overlaps[A <: String with Singleton: DbIdentifier](right: A)(using AreComparableArray[this.type, Placeholder[A, T]]): Overlaps[this.type, Placeholder[A, T]] =
    Overlaps(this, Placeholder[A, T])

  def anyOf[G <: Field[_]](right: G)(using CanContain[G, this.type]): AnyOf[this.type, G] = AnyOf(this, right)

  def anyOf[A <: String with Singleton: DbIdentifier, U](right: A)(using DbType[array[T]], CanContain[array[T], T]): AnyOf[this.type, Placeholder[A, array[T]]] =
    AnyOf(this, Placeholder[A, array[T]])

  def in[S <: SelectQuery[_, _, _, _, _, _, _, _]](right: S)(using CanContain[S, this.type]): In[this.type, S] = In(this, right)

  def notIn[S <: SelectQuery[_, _, _, _, _, _, _, _]](right: S)(using CanContain[S, this.type]): NotIn[this.type, S] = NotIn(this, right)

  def asc: Asc[this.type] = Asc(this)
  def desc: Desc[this.type] = Desc(this)

  def +[U, F <: Field[U], V](that: F)(
    using
    IsNumerical[T],
    IsNumerical[U],
    AdditionType[T, U, V],
    DbType[V]
  ): Add[T, this.type, U, F, V] = Add(this, that)
