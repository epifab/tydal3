package tydal.schema

trait Field[T]:
  def dbType: DbType[T]

  def as[A](tag: A)(using DbIdentifier[tag.type]): Aliased[T, this.type, tag.type] = Aliased(this)
  
  def castTo[B: DbType]: Cast[this.type, B] = Cast(this)

  def ===[G <: Field[_]](right: G)(using AreComparable[this.type, G]): Equals[this.type, G] = Equals(this, right)

  def ===[A <: String with Singleton](right: A)(
    using
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): Equals[this.type, NamedPlaceholder[A, T]] = Equals(this, NamedPlaceholder(right)(using dbType))

  def !==[G <: Field[_]](right: G)(using AreComparable[this.type, G]): NotEquals[this.type, G] = NotEquals(this, right)

  def !==[A <: String with Singleton](right: A)(
    using
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): NotEquals[this.type, NamedPlaceholder[A, T]] = NotEquals(this, NamedPlaceholder(right)(using dbType))

  def >[G <: Field[_]](right: G)(using AreComparable[this.type, G]): GreaterThan[this.type, G] = GreaterThan(this, right)

  def >[A <: String with Singleton](right: A)(
    using
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): GreaterThan[this.type, NamedPlaceholder[A, T]] = GreaterThan(this, NamedPlaceholder(right)(using dbType))

  def <[G <: Field[_]](right: G)(using AreComparable[this.type, G]): LessThan[this.type, G] = LessThan(this, right)

  def <[A <: String with Singleton](right: A)(
    using
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): LessThan[this.type, NamedPlaceholder[A, T]] = LessThan(this, NamedPlaceholder(right)(using dbType))

  def >=[G <: Field[_]](right: G)(using AreComparable[this.type, G]): GreaterThanOrEqual[this.type, G] = GreaterThanOrEqual(this, right)

  def >=[A <: String with Singleton](right: A)(
    using
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): GreaterThanOrEqual[this.type, NamedPlaceholder[A, T]] = GreaterThanOrEqual(this, NamedPlaceholder(right)(using dbType))

  def <=[G <: Field[_]](right: G)(using AreComparable[this.type, G]): LessThanOrEqual[this.type, G] = LessThanOrEqual(this, right)

  def <=[A <: String with Singleton](right: A)(
    using
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): LessThanOrEqual[this.type, NamedPlaceholder[A, T]] = LessThanOrEqual(this, NamedPlaceholder(right)(using dbType))

  def like[G <: Field[_]](right: G)(using IsText[T], IsText[G]): Like[this.type, G] = Like(this, right)

  def like[A <: String with Singleton](right: A)(using IsText[T]): Like[this.type, NamedPlaceholder[A, T]] =
    Like(this, NamedPlaceholder(right)(using dbType))

  def ilike[G <: Field[_]](right: G)(using IsText[T], IsText[G]): ILike[this.type, G] = ILike(this, right)

  def ilike[A <: String with Singleton](right: A)(using IsText[T]): ILike[this.type, NamedPlaceholder[A, T]] =
    ILike(this, NamedPlaceholder(right)(using dbType))

  def subsetOf[G <: Field[_]](right: G)(using AreComparableArray[this.type, G]): IsSubset[this.type, G] = IsSubset(this, right)

  def subsetOf[A <: String with Singleton](right: A)(using AreComparableArray[this.type, NamedPlaceholder[A, T]]): IsSubset[this.type, NamedPlaceholder[A, T]] =
    IsSubset(this, NamedPlaceholder(right)(using dbType))

  def supersetOf[G <: Field[_]](right: G)(using AreComparableArray[this.type, G]): IsSuperset[this.type, G] = IsSuperset(this, right)

  def supersetOf[A <: String with Singleton](right: A)(using AreComparableArray[this.type, NamedPlaceholder[A, T]]): IsSuperset[this.type, NamedPlaceholder[A, T]] =
    IsSuperset(this, NamedPlaceholder(right)(using dbType))

  def overlaps[G <: Field[_]](right: G)(using AreComparableArray[this.type, G]): Overlaps[this.type, G] = Overlaps(this, right)

  def overlaps[A <: String with Singleton](right: A)(using AreComparableArray[this.type, NamedPlaceholder[A, T]]): Overlaps[this.type, NamedPlaceholder[A, T]] =
    Overlaps(this, NamedPlaceholder(right)(using dbType))

  def contains[G <: Field[_]](right: G)(using CanContain[this.type, G]): Contains[this.type, G] = Contains(this, right)

  def contains[A <: String with Singleton, U](right: A)(using Unnested[T, U], DbType[U], CanContain[this.type, NamedPlaceholder[A, U]]): Contains[this.type, NamedPlaceholder[A, U]] =
    Contains(this, NamedPlaceholder(right))

  def in[S <: SelectQuery[_ <: Relations, _, _, _, _, _, _, _]](right: S)(using CanContain[S, this.type]): IsIn[this.type, S] = IsIn(this, right)

  def asc: Asc[this.type] = Asc(this)
  def desc: Desc[this.type] = Desc(this)
