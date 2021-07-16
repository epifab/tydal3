package tydal

import skunk.Query
import tydal.compiler.QueryCompiler
import tydal.utils.RelationFieldsFactory

trait QueryDsl[Fields]:
  def fields: Fields

  def compile[Input, Output](using compiler: QueryCompiler[this.type, Input, Output]): Query[Input, Output] =
    compiler.build(this)

  def as[Alias, SubQueryFields](alias: Alias)(
    using
    dbi: DbIdentifier[alias.type],
    fields: RelationFieldsFactory[alias.type, Fields, SubQueryFields]
  ): SubQuery[alias.type, SubQueryFields, this.type] = SubQuery(fields.value, this)

  def union[ThatFields, That <: QueryDsl[ThatFields]](that: That)(using UnifiableFields[Fields, ThatFields]): Union[Fields, this.type, ThatFields, That] =
    Union(this, that, distinct = true)

  def unionAll[ThatFields, That <: QueryDsl[ThatFields]](that: That)(using UnifiableFields[Fields, ThatFields]): Union[Fields, this.type, ThatFields, That] =
    Union(this, that, distinct = false)
