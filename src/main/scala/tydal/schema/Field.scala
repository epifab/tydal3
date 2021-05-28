package tydal.schema

trait Field[T] extends Taggable:
  def dbType: DbType[T]

  def ===[G <: Field[_]](right: G)(using AreComparable[this.type, G]): Equals[this.type, G] = Equals(this, right)

  def ===[T, A <: String with Singleton](right: A)(
    using
    FieldT[this.type, T],
    DbType[T],
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): Equals[this.type, NamedPlaceholder[A, T]] = Equals(this, NamedPlaceholder(right))

  def !==[G <: Field[_]](right: G)(using AreComparable[this.type, G]): NotEquals[this.type, G] = NotEquals(this, right)

  def !==[T, A <: String with Singleton](right: A)(
    using
    FieldT[this.type, T],
    DbType[T],
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): NotEquals[this.type, NamedPlaceholder[A, T]] = NotEquals(this, NamedPlaceholder(right))

  def >[G <: Field[_]](right: G)(using AreComparable[this.type, G]): GreaterThan[this.type, G] = GreaterThan(this, right)

  def >[T, A <: String with Singleton](right: A)(
    using
    FieldT[this.type, T],
    DbType[T],
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): GreaterThan[this.type, NamedPlaceholder[A, T]] = GreaterThan(this, NamedPlaceholder(right))

  def <[G <: Field[_]](right: G)(using AreComparable[this.type, G]): LessThan[this.type, G] = LessThan(this, right)

  def <[T, A <: String with Singleton](right: A)(
    using
    FieldT[this.type, T],
    DbType[T],
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): LessThan[this.type, NamedPlaceholder[A, T]] = LessThan(this, NamedPlaceholder(right))

  def >=[G <: Field[_]](right: G)(using AreComparable[this.type, G]): GreaterThanOrEqual[this.type, G] = GreaterThanOrEqual(this, right)

  def >=[T, A <: String with Singleton](right: A)(
    using
    FieldT[this.type, T],
    DbType[T],
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): GreaterThanOrEqual[this.type, NamedPlaceholder[A, T]] = GreaterThanOrEqual(this, NamedPlaceholder(right))

  def <=[G <: Field[_]](right: G)(using AreComparable[this.type, G]): LessThanOrEqual[this.type, G] = LessThanOrEqual(this, right)

  def <=[T, A <: String with Singleton](right: A)(
    using
    FieldT[this.type, T],
    DbType[T],
    AreComparable[this.type, NamedPlaceholder[A, T]]
  ): LessThanOrEqual[this.type, NamedPlaceholder[A, T]] = LessThanOrEqual(this, NamedPlaceholder(right))

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

  def in[G <: Field[_]](right: G)(using CanContain[G, this.type]): IsIncluded[this.type, G] = IsIncluded(this, right)

  def in[A <: String with Singleton](right: A)(using DbType[array[T]], CanContain[NamedPlaceholder[A, array[T]], this.type]): IsIncluded[this.type, NamedPlaceholder[A, array[T]]] =
    IsIncluded(this, NamedPlaceholder(right))


trait FieldT[-F, T]:
  def get(f: F): Field[T]

object FieldT:
  given pure[T]: FieldT[Field[T], T] with
    def get(field: Field[T]): Field[T] = field
