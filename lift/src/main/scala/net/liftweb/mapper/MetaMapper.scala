package net.liftweb.mapper

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import _root_.scala.collection.mutable.{ListBuffer, HashMap}
import _root_.java.lang.reflect.Method
import _root_.java.sql.{ResultSet, Types, PreparedStatement, Statement}
import _root_.scala.xml.{Elem, Node, Text, NodeSeq, Null, TopScope, UnprefixedAttribute, MetaData}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util.{Can, Empty, Full, Failure, NamedPF}
import _root_.net.liftweb.http.{LiftRules, S, SHtml, FieldError}
import _root_.java.util.Date
import _root_.net.liftweb.http.js._

trait BaseMetaMapper {
  type RealType <: Mapper[RealType]

  def beforeSchemifier: Unit
  def afterSchemifier: Unit

  def dbTableName: String
  def mappedFields: Seq[BaseMappedField];
  def dbAddTable: Can[() => Unit]

  def dbIndexes: List[Index[RealType]]
}

/**
 * Rules and functions shared by all Mappers
 */
object MapperRules {
  /**
   * This function converts a header name into the appropriate
   * XHTML format for displaying across the headers of a
   * formatted block.  The default is &lt;th&gt; for use
   * in XHTML tables.  If you change this function, the change
   * will be used for all MetaMappers, unless they've been
   * explicitly changed.
   */
  var displayNameToHeaderElement: String => NodeSeq = in => <th>{in}</th>

  /**
   * This function converts an element into the appropriate
   * XHTML format for displaying across a line
   * formatted block.  The default is &lt;td&gt; for use
   * in XHTML tables.  If you change this function, the change
   * will be used for all MetaMappers, unless they've been
   * explicitly changed.
   */
  var displayFieldAsLineElement: NodeSeq => NodeSeq = in => <td>{in}</td>

  /**
   * This function is the global (for all MetaMappers that have
   * not changed their formatFormElement function) that
   * converts a name and form for a given field in the
   * model to XHTML for presentation in the browser.  By
   * default, a table row ( &lt;tr&gt; ) is presented, but
   * you can change the function to display something else.
   */
  var formatFormElement: (NodeSeq, NodeSeq) => NodeSeq =
  (name, form) =>
  <xml:group><tr>
      <td>{name}</td>
      <td>{form}</td>
             </tr></xml:group>
}

trait MetaMapper[A<:Mapper[A]] extends BaseMetaMapper with Mapper[A] {
  self: A =>

  type RealType = A

  def beforeValidation: List[A => Any] = Nil
  def beforeValidationOnCreate: List[A => Any] = Nil
  def beforeValidationOnUpdate: List[A => Any] = Nil
  def afterValidation: List[A => Any] = Nil
  def afterValidationOnCreate: List[A => Any] = Nil
  def afterValidationOnUpdate: List[A => Any] = Nil

  def beforeSave: List[A => Any] = Nil
  def beforeCreate: List[(A) => Any] = Nil
  def beforeUpdate: List[(A) => Any] = Nil

  def afterSave: List[(A) => Any] = Nil
  def afterCreate: List[(A) => Any] = Nil
  def afterUpdate: List[(A) => Any] = Nil

  def beforeDelete: List[(A) => Any] = Nil
  def afterDelete: List[(A) => Any] = Nil

  /**
   * If there are model-specific validations to perform, override this
   * method and return an additional list of validations to perform
   */
  def validation: List[A => List[FieldError]] = Nil

  private def clearPostCommit(in: A) {
    in.addedPostCommit = false
  }

  def afterCommit: List[A => Unit] = clearPostCommit _ :: Nil

  def dbDefaultConnectionIdentifier: ConnectionIdentifier = DefaultConnectionIdentifier

  def findAll(): List[A] =
  findMapDb(dbDefaultConnectionIdentifier, Nil :_*)(v => Full(v))

  def findAllDb(dbId:ConnectionIdentifier): List[A] =
  findMapDb(dbId, Nil :_*)(v => Full(v))

  def countByInsecureSql(query: String, checkedBy: IHaveValidatedThisSQL): scala.Long =
  countByInsecureSqlDb(dbDefaultConnectionIdentifier, query, checkedBy)

  def countByInsecureSqlDb(dbId: ConnectionIdentifier, query: String, checkedBy: IHaveValidatedThisSQL): scala.Long =
  DB.use(dbId)(DB.prepareStatement(query, _)(DB.exec(_)(rs => if (rs.next) rs.getLong(1) else 0L)))

  def findAllByInsecureSql(query: String, checkedBy: IHaveValidatedThisSQL): List[A] = findAllByInsecureSqlDb(dbDefaultConnectionIdentifier, query, checkedBy)

  /**
   * Execute a PreparedStatement and return a List of Mapper instances. {@code f} is
   * where the user will do the work of creating the PreparedStatement and
   * preparing it for execution.
   *
   * @param f A function that takes a SuperConnection and returns a PreparedStatement.
   * @return A List of Mapper instances.
   */
  def findAllByPreparedStatement(f: SuperConnection => PreparedStatement): List[A] = {
    DB.use(dbDefaultConnectionIdentifier) {
      conn =>
      findAllByPreparedStatement(dbDefaultConnectionIdentifier, f(conn))
    }
  }

  def findAllByPreparedStatement(dbId: ConnectionIdentifier, stmt: PreparedStatement): List[A] = findAllByPreparedStatementDb(dbId, stmt)(a => Full(a))

  def findAllByPreparedStatementDb[T](dbId: ConnectionIdentifier, stmt: PreparedStatement)(f: A => Can[T]): List[T] = {
    DB.exec(stmt) {
      rs => createInstances(dbId, rs, Empty, Empty, f)
    }
  }

  def findAllByInsecureSqlDb(dbId: ConnectionIdentifier, query: String, checkedBy: IHaveValidatedThisSQL): List[A] =
  findMapByInsecureSqlDb(dbId, query, checkedBy)(a => Full(a))


  def findMapByInsecureSql[T](query: String, checkedBy: IHaveValidatedThisSQL)
  (f: A => Can[T]): List[T] =
  findMapByInsecureSqlDb(dbDefaultConnectionIdentifier, query, checkedBy)(f)

  def findMapByInsecureSqlDb[T](dbId: ConnectionIdentifier, query: String, checkedBy: IHaveValidatedThisSQL)(f: A => Can[T]): List[T] = {
    DB.use(dbId) {
      conn =>
      DB.prepareStatement(query, conn) {
        st =>
        DB.exec(st) {
          rs =>
          createInstances(dbId, rs, Empty, Empty, f)
        }
      }
    }
  }

  def dbAddTable: Can[() => Unit] = Empty

  def count: Long = countDb(dbDefaultConnectionIdentifier, Nil :_*)

  def count(by: QueryParam[A]*): Long = countDb(dbDefaultConnectionIdentifier, by:_*)

  def countDb(dbId: ConnectionIdentifier, by: QueryParam[A]*): Long = {
    DB.use(dbId) {
      conn =>
      val bl = by.toList
      val (query, start, max) = addEndStuffs(addFields("SELECT COUNT(*) FROM "+dbTableName+"  ", false, bl), bl, conn)

      DB.prepareStatement(query, conn) {
        st =>
        setStatementFields(st, bl, 1)
        DB.exec(st) {
          rs =>
          if (rs.next) rs.getLong(1)
          else 0
        }
      }
    }
  }

  type KeyDude = T forSome {type T}
  type OtherMapper = T forSome {type T <: KeyedMapper[KeyDude, T]}
  type OtherMetaMapper = T forSome {type T <: KeyedMetaMapper[KeyDude, OtherMapper]}
  //type OtherMapper = KeyedMapper[_, (T forSome {type T})]
  //type OtherMetaMapper = KeyedMetaMapper[_, OtherMapper]

  def findAllFields(fields: Seq[SelectableField],
                    by: QueryParam[A]*): List[A] =
  findMapFieldDb(dbDefaultConnectionIdentifier,
                 fields, by :_*)(v => Full(v))

  def findAllFieldsDb(dbId: ConnectionIdentifier,
                      fields: Seq[SelectableField],
                      by: QueryParam[A]*):
  List[A] = findMapFieldDb(dbId, fields, by :_*)(v => Full(v))

  private def dealWithJoins(ret: List[A], by: Seq[QueryParam[A]]): List[A] = {

    val join = by.flatMap{case j: Join[A] => List(j) case _ => Nil}
    for (j <- join) {
      type FT = j.field.FieldType
    type MT = T forSome {type T <: KeyedMapper[FT, T]}
    

      val ol: List[MT] = j.field.dbKeyToTable.
      asInstanceOf[MetaMapper[A]].
      findAll(new InThing[A]{
          type JoinType = FT
          type InnerType = A

          val outerField: MappedField[JoinType, A] =
          j.field.dbKeyToTable.primaryKeyField.asInstanceOf[MappedField[JoinType, A]]
          val innerField: MappedField[JoinType, A] = j.field.asInstanceOf[MappedField[JoinType, A]]
          val innerMeta: MetaMapper[A] = j.field.fieldOwner.getSingleton

          val queryParams: List[QueryParam[A]] = by.toList
        }.asInstanceOf[QueryParam[A]] ).asInstanceOf[List[MT]]

      val map: Map[FT, MT] =
      Map(ol.map(v => (v.primaryKeyField.is, v)) :_*)

      for (i <- ret) {
        
        val field: MappedForeignKey[FT, A, _] =
        getActualField(i, j.field).asInstanceOf[MappedForeignKey[FT, A, _]]

        map.get(field.is) match {
          case Some(v) => field.primeObj(Full(v))
            case _ => field.primeObj(Empty)
        }
        //field.primeObj(Can(map.get(field.is).map(_.asInstanceOf[QQ])))
      }
    }

    ret
  }

  def findAll(by: QueryParam[A]*): List[A] =
  dealWithJoins(findMapDb(dbDefaultConnectionIdentifier, by :_*)
                (v => Full(v)), by)


  def findAllDb(dbId: ConnectionIdentifier,by: QueryParam[A]*): List[A] =
  dealWithJoins(findMapDb(dbId, by :_*)(v => Full(v)), by)

  def bulkDelete_!!(by: QueryParam[A]*): Boolean = bulkDelete_!!(dbDefaultConnectionIdentifier, by :_*)
  def bulkDelete_!!(dbId: ConnectionIdentifier, by: QueryParam[A]*): Boolean = {
    DB.use(dbId) {
      conn =>
      val bl = by.toList
      val (query, start, max) = addEndStuffs(addFields("DELETE FROM "+dbTableName+" ", false, bl), bl, conn)

      DB.prepareStatement(query, conn) {
        st =>
        setStatementFields(st, bl, 1)
        st.executeUpdate
        true
      }
    }
  }

  def findMap[T](by: QueryParam[A]*)(f: A => Can[T]) =
  findMapDb(dbDefaultConnectionIdentifier, by :_*)(f)

  def findMapDb[T](dbId: ConnectionIdentifier,
                   by: QueryParam[A]*)(f: A => Can[T]): List[T] =
  findMapFieldDb(dbId, mappedFields, by :_*)(f)

  def findMapFieldDb[T](dbId: ConnectionIdentifier, fields: Seq[SelectableField],
                        by: QueryParam[A]*)(f: A => Can[T]): List[T] = {
    DB.use(dbId) {
      conn =>
      val bl = by.toList
      val (query, start, max) = addEndStuffs(addFields("SELECT "+
                                                       fields.map(_.dbSelectString).
                                                       mkString(", ")+
                                                       " FROM "+dbTableName+
                                                       "  ", false, bl), bl, conn)
      DB.prepareStatement(query, conn) {
        st =>
        setStatementFields(st, bl, 1)
        DB.exec(st)(createInstances(dbId, _, start, max, f))
      }
    }
  }

  def create: A = createInstance


  private[mapper] def addFields(what: String, whereAdded: Boolean, by: List[QueryParam[A]]): String = {

    var wav = whereAdded

    def whereOrAnd = if (wav) " AND " else {wav = true; " WHERE "}

    by match {
      case Nil => what
      case x :: xs => {
          var updatedWhat = what
          x match {
            case Cmp(field, opr, Full(_), _) =>
              (1 to field.dbColumnCount).foreach {
                cn =>
                updatedWhat = updatedWhat + whereOrAnd +field.dbColumnNames(field.name)(cn - 1)+" "+opr+" ? "
              }

            case Cmp(field, opr, _, Full(otherField)) =>
              (1 to field.dbColumnCount).foreach {
                cn =>
                updatedWhat = updatedWhat + whereOrAnd +field.dbColumnNames(field.name)(cn - 1)+" "+opr+" "+
                otherField.dbColumnNames(otherField.name)(cn - 1)
              }

            case Cmp(field, opr, Empty, Empty) =>
              (1 to field.dbColumnCount).foreach (cn => updatedWhat = updatedWhat + whereOrAnd +field.dbColumnNames(field.name)(cn - 1)+" "+opr+" ")

              // For vals, add "AND $fieldname = ? [OR $fieldname = ?]*" to the query. The number
              // of fields you add onto the query is equal to vals.length
            case ByList(field, vals) =>
              vals match {
                case Nil =>
                  updatedWhat = updatedWhat + whereOrAnd + " 0 = 1 "

                case _ => {
                    updatedWhat = updatedWhat +
                    vals.map(v => field.dbColumnName+ " = ?").mkString(whereOrAnd+" (", " OR ", ")")
                  }
              }


            case in: InRaw[A, _] =>
              updatedWhat = updatedWhat + whereOrAnd + (in.rawSql match {
                  case null | "" => " 0 = 1 "
                  case sql => " "+in.field.dbColumnName+" IN ( "+sql+" ) "
                })

            case (in: InThing[A]) =>
              updatedWhat = updatedWhat + whereOrAnd +
              in.outerField.dbColumnName+
              " IN ("+in.innerMeta.addFields("SELECT "+
                                             in.innerField.dbColumnName+
                                             " FROM "+
                                             in.innerMeta.dbTableName+" ",false,
                                             in.queryParams)+" ) "


              // Executes a subquery with {@code query}
            case BySql(query, _*) =>
              updatedWhat = updatedWhat + whereOrAnd + " ( "+ query +" ) "
            case _ =>
          }
          addFields(updatedWhat, wav, xs)
        }
    }
  }


  private[mapper] def setStatementFields(st: PreparedStatement, by: List[QueryParam[A]], curPos: Int): Int = {
    by match {
      case Nil => curPos
      case Cmp(field, _, Full(value), _) :: xs =>
        st.setObject(curPos, field.convertToJDBCFriendly(value), field.targetSQLType)
        setStatementFields(st, xs, curPos + 1)

      case ByList(field, vals) :: xs => {
          var newPos = curPos
          vals.foreach(v => {
              st.setObject(newPos,
                           field.convertToJDBCFriendly(v),
                           field.targetSQLType)
              newPos = newPos + 1
            })
          setStatementFields(st, xs, newPos)
        }

      case (in: InThing[A]) :: xs =>
        val newPos = in.innerMeta.setStatementFields(st, in.queryParams,
                                                     curPos)
        setStatementFields(st, xs, newPos)

      case BySql(query, params @ _*) :: xs => {
          params.toList match {
            case Nil => setStatementFields(st, xs, curPos)
            case List(i: Int) =>
              st.setInt(curPos, i)
              setStatementFields(st, xs, curPos + 1)
            case List(lo: Long) =>
              st.setLong(curPos, lo)
              setStatementFields(st, xs, curPos + 1)
            case List(s: String) =>
              st.setString(curPos, s)
              setStatementFields(st, xs, curPos + 1)
            case List(d: Date) =>
              st.setDate(curPos, new _root_.java.sql.Date(d.getTime))
              setStatementFields(st, xs, curPos + 1)
            case List(field: BaseMappedField) => st.setObject(curPos, field.jdbcFriendly, field.targetSQLType)
              setStatementFields(st, xs, curPos + 1)

            case p :: ps =>
              setStatementFields(st, BySql[A](query, p) :: BySql[A](query, ps: _*) :: xs, curPos)
          }
        }
      case _ :: xs => {
          setStatementFields(st, xs, curPos)
        }
    }
  }

  // def find(by: QueryParam): Can[A] = find(List(by))

  private def _addOrdering(in: String, params: List[QueryParam[A]]): String = {
    params.flatMap{
      case OrderBy(field, order) => List(field.dbColumnName+" "+order.sql)
      case OrderBySql(sql) => List(sql)
      case _ => Nil
    } match {
      case Nil => in
      case xs => in + " ORDER BY "+xs.mkString(" , ")
    }
  }

  def addEndStuffs(in: String, params: List[QueryParam[A]], conn: SuperConnection): (String, Can[Long], Can[Long]) = {
    val tmp = _addOrdering(in, params)
    val max = params.foldRight(Empty.asInstanceOf[Can[Long]]){(a,b) => a match {case MaxRows(n) => Full(n); case _ => b}}
    val start = params.foldRight(Empty.asInstanceOf[Can[Long]]){(a,b) => a match {case StartAt(n) => Full(n); case _ => b}}

    if (conn.brokenLimit_?) (tmp, start, max) else {
      val ret = (max, start) match {
        case (Full(max), Full(start)) => tmp + " LIMIT "+max+" OFFSET "+start
        case (Full(max), _) => tmp + " LIMIT "+max
        case (_, Full(start)) => tmp + " LIMIT "+conn.driverType.maxSelectLimit+" OFFSET "+start
        case _ => tmp
      }
      (ret, Empty, Empty)
    }
  }

  def delete_!(toDelete : A): Boolean = indexMap.map(im =>
    DB.use(toDelete.connectionIdentifier) {
      conn =>
      _beforeDelete(toDelete)
      val ret = DB.prepareStatement("DELETE FROM "+dbTableName +" WHERE "+im+" = ?", conn) {
        st =>
        val indVal = indexedField(toDelete)
        indVal.map{indVal =>
          st.setObject(1, indVal.jdbcFriendly(im), indVal.targetSQLType(im))

          st.executeUpdate == 1
        } openOr false
      }
      _afterDelete(toDelete)
      ret
    }
  ).openOr(false)


  type AnyBound = T forSome {type T}

  private[mapper] def ??(meth: Method, inst: A) = meth.invoke(inst).asInstanceOf[MappedField[AnyBound, A]]

  def dirty_?(toTest: A): Boolean = mappedFieldList.exists(
    mft =>
    ??(mft.method, toTest).dirty_?
  )

  def indexedField(toSave : A) : Can[MappedField[Any, A]] = indexMap.map(im => ??(mappedColumns(im), toSave))

  def saved_?(toSave: A): Boolean = (for (im <- indexMap; indF <- indexedField(toSave)) yield (indF.dbIndexFieldIndicatesSaved_?)).openOr(true)

  def whatToSet(toSave : A) : String = {
    mappedColumns.filter{c => ??(c._2, toSave).dirty_?}.map{c => c._1 + " = ?"}.toList.mkString("", ",", "")
  }

  /**
   * Run the list of field validations, etc.  This is the raw validation,
   * without the notifications.  This method can be over-ridden.
   */
  protected def runValidationList(toValidate: A): List[FieldError] =
  mappedFieldList.flatMap(f => ??(f.method, toValidate).validate) :::
  validation.flatMap{
    case pf: PartialFunction[A, List[FieldError]] =>
      if (pf.isDefinedAt(toValidate)) pf(toValidate)
      else Nil

    case f => f(toValidate)
  }

  final def validate(toValidate: A): List[FieldError] = {
    val saved_? = this.saved_?(toValidate)
    _beforeValidation(toValidate)
    if (saved_?) _beforeValidationOnUpdate(toValidate) else _beforeValidationOnCreate(toValidate)

    val ret: List[FieldError] = runValidationList(toValidate)

    _afterValidation(toValidate)
    if (saved_?) _afterValidationOnUpdate(toValidate) else _afterValidationOnCreate(toValidate)

    ret
  }

  val elemName = getClass.getSuperclass.getName.split("\\.").toList.last

  def toXml(what: A): Elem =
  Elem(null,elemName,
       mappedFieldList.foldRight[MetaData](Null) {(p, md) => val fld = ??(p.method, what)
                                                  new UnprefixedAttribute(p.name, Text(fld.toString), md)}
       ,TopScope)

  /**
   * Returns true if none of the fields are dirty
   */
  def clean_?(toCheck: A): Boolean = mappedColumns.foldLeft(true)((bool, ptr) => bool && !(??(ptr._2, toCheck).dirty_?))

  def save(toSave: A): Boolean = {
    /**
     * @return true if there was exactly one row in the result set, false if not.
     */
    def runAppliers(rs: ResultSet) : Boolean = {
      try {
        if (rs.next) {
          val meta = rs.getMetaData
          toSave.runSafe {
            findApplier(indexMap.open_!, rs.getObject(1)) match {
              case Full(ap) => ap.apply(toSave, rs.getObject(1))
              case _ =>
            }
          }
          !rs.next
        } else false
      } finally {
        rs.close
      }
    }

    /**
     * Checks whether the result set has exactly one row.
     */
    def hasOneRow(rs: ResultSet) : Boolean = {
      try {
        val firstRow = rs.next
        (firstRow && !rs.next)
      } finally {
        rs.close
      }
    }

    if (saved_?(toSave) && clean_?(toSave)) true else {
      val ret = DB.use(toSave.connectionIdentifier) {
        conn =>
        _beforeSave(toSave)
        val ret = if (saved_?(toSave)) {
          _beforeUpdate(toSave)
          val ret: Boolean = if (!dirty_?(toSave)) true else {
            val ret: Boolean = DB.prepareStatement("UPDATE "+dbTableName+" SET "+whatToSet(toSave)+" WHERE "+indexMap.open_! +" = ?", conn) {
              st =>
              var colNum = 1

              for (col <- mappedColumns) {
                val colVal = ??(col._2, toSave)
                if (!columnPrimaryKey_?(col._1) && colVal.dirty_?) {
                  colVal.targetSQLType(col._1) match {
                    case Types.VARCHAR => st.setString(colNum, colVal.jdbcFriendly(col._1).asInstanceOf[String])

                    case _ => st.setObject(colNum, colVal.jdbcFriendly(col._1), colVal.targetSQLType(col._1))
                  }
                  colNum = colNum + 1
                }
              }

              indexedField(toSave).foreach(indVal =>  st.setObject(colNum, indVal.jdbcFriendly(indexMap.open_!),
                                                                   indVal.targetSQLType(indexMap.open_!)))
              st.executeUpdate
              true
            }
            ret
          }
          _afterUpdate(toSave)
          ret
        } else {
          _beforeCreate(toSave)

          val query = "INSERT INTO "+dbTableName+" ("+columnNamesForInsert+") VALUES ("+columnQueriesForInsert+")"

          def prepStat(st : PreparedStatement, postQuery: Can[String]) : Boolean = {
            var colNum = 1

            for (col <- mappedColumns) {
              if (!columnPrimaryKey_?(col._1)) {
                val colVal = col._2.invoke(toSave).asInstanceOf[MappedField[AnyRef, A]]
                colVal.targetSQLType(col._1) match {
                  case Types.VARCHAR => st.setString(colNum, colVal.jdbcFriendly(col._1).asInstanceOf[String])

                  case _ => st.setObject(colNum, colVal.jdbcFriendly(col._1), colVal.targetSQLType(col._1))
                }

                // st.setObject(colNum, colVal.getJDBCFriendly(col._1), colVal.getTargetSQLType(col._1))
                colNum = colNum + 1
              }
            }

            val oneRowUpdated : Boolean = (conn.brokenAutogeneratedKeys_?, postQuery, indexMap) match {
              case (true, _, Empty)  => hasOneRow(st.executeQuery)

              case (true, Full(qry), _)     =>
                st.executeUpdate
                DB.prepareStatement(qry, conn)(st => runAppliers(st.executeQuery))

              case (true, _, _)     => runAppliers(st.executeQuery)

              case (false, _, Empty) => st.executeUpdate == 1

              case (false, _, _)    =>
                val uc = st.executeUpdate
                runAppliers(st.getGeneratedKeys)
            }
            oneRowUpdated
          }

          val ret = if (conn.wickedBrokenAutogeneratedKeys_?) {
            DB.prepareStatement(query, conn) {
              st => prepStat(st, Full("SELECT lastval()"))
            }
          } else if (conn.brokenAutogeneratedKeys_?) {
            val pkName = (mappedColumnInfo.filter(_._2.dbPrimaryKey_?).map(_._1)).toList.mkString(",")
            DB.prepareStatement(query + " RETURNING " + pkName, conn) {
              st => prepStat(st, Empty)
            }
          } else {
            DB.prepareStatement(query, Statement.RETURN_GENERATED_KEYS, conn) {
              st => prepStat(st, Empty)
            }
          }

          _afterCreate(toSave)
          ret
        }
        _afterSave(toSave)
        ret
      }

      // clear dirty and get rid of history
      for (col <- mappedColumns) {
        val colVal = ??(col._2, toSave)
        if (!columnPrimaryKey_?(col._1) && colVal.dirty_?) {
          colVal.resetDirty
          colVal.doneWithSave
        }
      }

      ret
    }
  }

  def columnPrimaryKey_?(name : String) = mappedColumnInfo.get(name).map(_.dbPrimaryKey_?) getOrElse false

  def createInstances(dbId: ConnectionIdentifier, rs: ResultSet, start: Can[Long], omax: Can[Long]) : List[A] = createInstances(dbId, rs, start, omax, v => Full(v))


  def createInstances[T](dbId: ConnectionIdentifier, rs: ResultSet, start: Can[Long], omax: Can[Long], f: A => Can[T]) : List[T] = {
    var ret = new ListBuffer[T]
    val bm = buildMapper(rs)
    var pos = (start openOr 0L) * -1L
    val max = omax openOr _root_.java.lang.Long.MAX_VALUE

    while (pos < max && rs.next()) {
      if (pos >= 0L) {
        f(createInstance(dbId, rs, bm._1, bm._2)).foreach(v => ret += v)
      }
      pos = pos + 1L
    }

    ret.toList
  }

  def appendFieldToStrings(in: A): String = mappedFieldList.map(p => ??(p.method, in).asString).mkString(",")

  private val columnNameToMappee = new HashMap[String, Can[(ResultSet, Int, A) => Unit]]

  def buildMapper(rs: ResultSet): (Int, Array[(ResultSet,Int,A) => Unit]) = synchronized {
    val meta = rs.getMetaData
    val colCnt = meta.getColumnCount
    val ar = new Array[(ResultSet, Int, A) => Unit](colCnt + 1)
    for (pos <- 1 to colCnt) {
      val colName = meta.getColumnName(pos).toLowerCase
      val optFunc = columnNameToMappee.get(colName) match {
        case None => {
            val colType = meta.getColumnType(pos)
            val fieldInfo = mappedColumns.get(colName)
            val setTo =
            if (fieldInfo != None) {
              val tField = fieldInfo.get.invoke(this).asInstanceOf[MappedField[AnyRef, A]]
              Some(colType match {
                  case Types.INTEGER | Types.BIGINT => {
                      val bsl = tField.buildSetLongValue(fieldInfo.get, colName)
                      (rs: ResultSet, pos: Int, objInst: A) => bsl(objInst, rs.getLong(pos), rs.wasNull)}
                  case Types.VARCHAR => {
                      val bsl = tField.buildSetStringValue(fieldInfo.get, colName)
                      (rs: ResultSet, pos: Int, objInst: A) => bsl(objInst, rs.getString(pos))}
                  case Types.DATE | Types.TIME | Types.TIMESTAMP =>
                    val bsl = tField.buildSetDateValue(fieldInfo.get, colName)
                    (rs: ResultSet, pos: Int, objInst: A) => bsl(objInst, rs.getTimestamp(pos))
                  case Types.BOOLEAN | Types.BIT =>{
                      val bsl = tField.buildSetBooleanValue(fieldInfo.get, colName)
                      (rs: ResultSet, pos: Int, objInst: A) => bsl(objInst, rs.getBoolean(pos), rs.wasNull)}
                  case _ => {
                      (rs: ResultSet, pos: Int, objInst: A) => {
                        val res = rs.getObject(pos)
                        findApplier(colName, res).foreach(f => f(objInst, res))
                      }
                    }
                })
            } else None

            columnNameToMappee(colName) = Can(setTo)
            Can(setTo)
          }
        case Some(of) => of
      }
      ar(pos) = optFunc openOr null
    }
    (colCnt, ar)
  }

  def createInstance(dbId: ConnectionIdentifier, rs : ResultSet, colCnt: Int, mapFuncs: Array[(ResultSet,Int,A) => Unit]) : A = {
    val ret = createInstance.connectionIdentifier(dbId)
    val ra = ret// .asInstanceOf[Mapper[A]]

    var pos = 1
    while (pos <= colCnt) {
      mapFuncs(pos) match {
        case null =>
        case f => f(rs, pos, ra)
      }
      pos = pos + 1
    }
    ret
  }

  protected def  findApplier(name : String, inst : AnyRef) : Can[((A, AnyRef) => Unit)] = synchronized {
    val clz = inst match {
      case null => null
      case _ => inst.getClass.asInstanceOf[Class[(C forSome {type C})]]
    }
    val look = (name.toLowerCase, if (clz ne null) Full(clz) else Empty)
    Can(mappedAppliers.get(look) orElse {
        val newFunc = createApplier(name, inst)
        mappedAppliers(look) = newFunc
        Some(newFunc)
      })
  }


  private def createApplier(name : String, inst : AnyRef /*, clz : Class*/) : (A, AnyRef) => Unit = {
    val accessor = mappedColumns.get(name)
    if ((accessor eq null) || accessor == None) null else {
      (accessor.get.invoke(this).asInstanceOf[MappedField[AnyRef, A]]).buildSetActualValue(accessor.get, inst, name)
    }
  }

  def fieldMapperPF(transform: (BaseOwnedMappedField[A] => NodeSeq), actual: A): PartialFunction[String, NodeSeq => NodeSeq] = {
    Map.empty ++ mappedFieldList.map ( mf =>
      (mf.name, ((ignore: NodeSeq) => transform(??(mf.method, actual))))
    )
  }

  def checkFieldNames(in: A): Unit = mappedFieldList.foreach(f =>
    ??(f.method, in) match {
      case field if (field.i_name_! eq null) => field.setName_!(f.name)
      case _ =>
    })

  /**
   * Get a field by the field name
   * @param fieldName -- the name of the field to get
   * @param actual -- the instance to get the field on
   *
   * @return Can[The Field] (Empty if the field is not found)
   */
  def fieldByName[T](fieldName: String, actual: A):Can[MappedField[T, A]] = Can(_mappedFields.get(fieldName)).map(meth => ??(meth, actual).asInstanceOf[MappedField[T,A]])

  /**
   * A partial function that takes an instance of A and a field name and returns the mapped field
   */
  lazy val fieldMatcher: PartialFunction[(A, String), MappedField[Any, A]] = {
    case (actual, fieldName) if _mappedFields.contains(fieldName) => fieldByName[Any](fieldName, actual).open_! // we know this is defined
  }

  def createInstance: A = rootClass.newInstance.asInstanceOf[A]

  def fieldOrder: List[BaseOwnedMappedField[A]] = Nil

  protected val rootClass = this.getClass.getSuperclass

  private val mappedAppliers = new HashMap[(String, Can[Class[(C forSome {type C})]]), (A, AnyRef) => Unit];

  private val _mappedFields  = new HashMap[String, Method];

  private[mapper] var mappedFieldList: List[FieldHolder[A]] = Nil; // new Array[Triple[String, Method, MappedField[Any,Any]]]();

  private var mappedCallbacks: List[(String, Method)] = Nil

  private val mappedColumns = new HashMap[String, Method];

  // private val mappedFieldInfo = new HashMap[String, MappedField[AnyRef, A]]
  private val mappedColumnInfo = new HashMap[String, MappedField[AnyRef, A]]



  private var indexMap: Can[String] = Empty

  this.runSafe {
    val tArray = new ListBuffer[FieldHolder[A]]

    def isMagicObject(m: Method) = m.getReturnType.getName.endsWith("$"+m.getName+"$") && m.getParameterTypes.length == 0
    def isMappedField(m: Method) = classOf[MappedField[Nothing, A]].isAssignableFrom(m.getReturnType)
    def isLifecycle(m: Method) = classOf[LifecycleCallbacks].isAssignableFrom(m.getReturnType)

    mappedCallbacks = for (v <- this.getClass.getSuperclass.getMethods.toList if isMagicObject(v) && isLifecycle(v)) yield (v.getName, v)

    for (v <- this.getClass.getSuperclass.getMethods  if isMagicObject(v) && isMappedField(v)) {
      v.invoke(this) match {
        case mf: MappedField[AnyRef, A] if !mf.ignoreField_? =>
          mf.setName_!(v.getName)
          tArray += FieldHolder(mf.name, v, mf)
          for (colName <- mf.dbColumnNames(v.getName)) {
            mappedColumnInfo(colName) = mf
            mappedColumns(colName) = v
          }
          if (mf.dbPrimaryKey_?) {
            indexMap = Full(mf.dbColumnName) // Full(v.getName.toLowerCase)
          }

        case _ =>
      }
    }

    def findPos(in: AnyRef) : Can[Int] = {
      tArray.toList.zipWithIndex.filter(mft => in eq mft._1.field) match {
        case Nil => Empty
        case x :: xs => Full(x._2)
      }
    }

    val resArray = new ListBuffer[FieldHolder[A]];

    fieldOrder.foreach(f => findPos(f).foreach(pos => resArray += tArray.remove(pos)))

    tArray.foreach(mft => resArray += mft)

    mappedFieldList = resArray.toList
    mappedFieldList.foreach(ae => _mappedFields(ae.name) = ae.method)
  }

  val columnNamesForInsert = (mappedColumnInfo.filter(!_._2.dbPrimaryKey_?).map(_._1)).toList.mkString(",")

  val columnQueriesForInsert = {
    (mappedColumnInfo.filter(!_._2.dbPrimaryKey_?).map(p => "?")).toList.mkString(",")
  }

  private def fixTableName(name: String) = clean(name.toLowerCase) match {
    case name if DB.reservedWords.contains(name) => name+"_t"
    case name => name
  }

  private def internalTableName_$_$ =
  getClass.getSuperclass.getName.split("\\.").toList.last;

  /**
   * This function converts a header name into the appropriate
   * XHTML format for displaying across the headers of a
   * formatted block.  The default is &lt;th&gt; for use
   * in XHTML tables.  If you change this function, the change
   * will be used for this MetaMapper unless you override the
   * htmlHeades method
   */
  var displayNameToHeaderElement: String => NodeSeq =
  MapperRules.displayNameToHeaderElement

  def htmlHeaders: NodeSeq =
  mappedFieldList.filter(_.field.dbDisplay_?).
  flatMap(mft => displayNameToHeaderElement(mft.field.displayName))

  def mappedFields: Seq[BaseMappedField] = mappedFieldList.map(f => f.field)

  /**
   * This function converts an element into the appropriate
   * XHTML format for displaying across a line
   * formatted block.  The default is &lt;td&gt; for use
   * in XHTML tables.  If you change this function, the change
   * will be used for this MetaMapper unless you override the
   * doHtmlLine method.
   */
  var displayFieldAsLineElement: NodeSeq => NodeSeq =
  MapperRules.displayFieldAsLineElement


  def doHtmlLine(toLine: A): NodeSeq =
  mappedFieldList.filter(_.field.dbDisplay_?).
  flatMap(mft => displayFieldAsLineElement(??(mft.method, toLine).asHtml))

  def asJs(actual: A): JsExp = {
    JE.JsObj(("$lift_class", JE.Str(dbTableName)) :: mappedFieldList.
             map(f => ??(f.method, actual)).filter(_.renderJs_?).flatMap(_.asJs).toList :::
             actual.suplementalJs(Empty) :_*)
  }

  /**
   *
   */
  def asJSON(actual: A, sb: StringBuilder): StringBuilder = {
    sb.append('{')
    mappedFieldList.foreach{
      f =>
      sb.append(f.name)
      sb.append(':')
      ??(f.method, actual).is
      // FIXME finish JSON
    }
    sb.append('}')
    sb
  }

  def asHtml(toLine: A): NodeSeq =
  Text(internalTableName_$_$) :: Text("={ ") ::
  (for (mft <- mappedFieldList if mft.field.dbDisplay_? ;
        val field = ??(mft.method, toLine)) yield
   <span>{field.displayName}={field.asHtml}&nbsp;</span>
  ) :::List(Text(" }"))


  /**
   * This function converts a name and form for a given field in the
   * model to XHTML for presentation in the browser.  By
   * default, a table row ( &lt;tr&gt; ) is presented, but
   * you can change the function to display something else.
   */
  var formatFormElement: (NodeSeq, NodeSeq) => NodeSeq =
  MapperRules.formatFormElement

  def formatFormLine(displayName: NodeSeq, form: NodeSeq): NodeSeq =
  formatFormElement(displayName, form)

  def toForm(toMap: A): NodeSeq =
  mappedFieldList.map(e => ??(e.method, toMap)).
  filter(_.dbDisplay_?).flatMap (
    field =>
    field.toForm.toList.
    flatMap(form => formatFormLine(Text(field.displayName), form))
  )

  /**
   * Get the fields (in order) for displaying a form
   */
  def formFields(toMap: A): List[MappedField[_, A]] =
  mappedFieldList.map(e => ??(e.method, toMap)).filter(_.dbDisplay_?)


  /**
   * map the fields titles and forms to generate a list
   * @param func called with displayHtml, fieldId, form
   */
  def mapFieldTitleForm[T](toMap: A,
                           func: (NodeSeq, Can[NodeSeq], NodeSeq) => T): List[T] =
  formFields(toMap).flatMap(field => field.toForm.
                            map(fo => func(field.displayHtml, field.fieldId, fo)))


  /**
   * flat map the fields titles and forms to generate a list
   * @param func called with displayHtml, fieldId, form
   */
  def flatMapFieldTitleForm[T](toMap: A,
                               func: (NodeSeq, Can[NodeSeq], NodeSeq) => Seq[T]): List[T] =
  formFields(toMap).flatMap(field => field.toForm.toList.
                            flatMap(fo => func(field.displayHtml,
                                               field.fieldId, fo)))


  /**
   * Given the prototype field (the field on the Singleton), get the field from the instance
   * @param actual -- the Mapper instance
   * @param protoField -- the field from the MetaMapper (Singleton)
   *
   * @return the field from the actual object
   */
  def getActualField[T](actual: A, protoField: MappedField[T, A]): MappedField[T, A] =
  ??(_mappedFields(protoField.name), actual).asInstanceOf[MappedField[T,A]]


  /**
   * Given the prototype field (the field on the Singleton), get the field from the instance
   * @param actual -- the Mapper instance
   * @param protoField -- the field from the MetaMapper (Singleton)
   *
   * @return the field from the actual object
   */
  def getActualBaseField(actual: A, protoField: BaseOwnedMappedField[A]): BaseOwnedMappedField[A] =
  ??(_mappedFields(protoField.name), actual) // .asInstanceOf[MappedField[T,A]]

  /**
   * The name of the database table.  Override this method if you
   * want to change the table to something other than the name of the Mapper class
   */
  def dbTableName = _dbTableName

  private[mapper] lazy val _dbTableName = fixTableName(internalTableName_$_$)

  /*
   private val _dbTableName: String = {
   fixTableName(internalTableName_$_$)
   }
   */


  private def setupInstanceForPostCommit(inst: A) {
    if (!inst.addedPostCommit) {
      DB.appendPostFunc(inst.connectionIdentifier, () => afterCommit.foreach(_(inst)))
      inst.addedPostCommit = true
    }
  }

  private def eachField(what: A, toRun: List[(A) => Any])(f: (LifecycleCallbacks) => Any) {
    mappedCallbacks.foreach (e =>
      e._2.invoke(what) match {
        case lccb: LifecycleCallbacks => f(lccb)
        case _ =>
      })
    toRun.foreach{tf => tf(what)}
  }
  private def _beforeValidation(what: A) {setupInstanceForPostCommit(what); eachField(what, beforeValidation) {field => field.beforeValidation}  }
  private def _beforeValidationOnCreate(what: A) {eachField(what, beforeValidationOnCreate) {field => field.beforeValidationOnCreate}  }
  private def _beforeValidationOnUpdate(what: A) {eachField(what, beforeValidationOnUpdate) {field => field.beforeValidationOnUpdate}  }
  private def _afterValidation(what: A) { eachField(what, afterValidation) {field => field.afterValidation}  }
  private def _afterValidationOnCreate(what: A) {eachField(what, afterValidationOnCreate) {field => field.afterValidationOnCreate}  }
  private def _afterValidationOnUpdate(what: A) {eachField(what, afterValidationOnUpdate) {field => field.afterValidationOnUpdate}  }

  private def _beforeSave(what: A) {setupInstanceForPostCommit(what); eachField(what, beforeSave) {field => field.beforeSave}  }
  private def _beforeCreate(what: A) { eachField(what, beforeCreate) {field => field.beforeCreate}  }
  private def _beforeUpdate(what: A) { eachField(what, beforeUpdate) {field => field.beforeUpdate}  }

  private def _afterSave(what: A) {eachField(what, afterSave) {field => field.afterSave}  }
  private def _afterCreate(what: A) {eachField(what, afterCreate) {field => field.afterCreate}  }
  private def _afterUpdate(what: A) {eachField(what, afterUpdate) {field => field.afterUpdate}  }

  private def _beforeDelete(what: A) {setupInstanceForPostCommit(what); eachField(what, beforeDelete) {field => field.beforeDelete}  }
  private def _afterDelete(what: A) {eachField(what, afterDelete) {field => field.afterDelete}  }

  def beforeSchemifier {}
  def afterSchemifier {}

  def dbIndexes: List[Index[A]] = Nil

  implicit def fieldToItem[T](in: MappedField[T, A]): IndexItem[A] = IndexField(in)
  implicit def boundedFieldToItem(in: (MappedField[String, A], Int)): BoundedIndexField[A] = BoundedIndexField(in._1, in._2)

  // protected def getField(inst : Mapper[A], meth : Method) = meth.invoke(inst, null).asInstanceOf[MappedField[AnyRef,A]]
}

object OprEnum extends Enumeration {
  val Eql = Value(1, "=")
  val <> = Value(2, "<>")
  val >= = Value(3, ">=")
  val != = <>
  val <= = Value(4, "<=")
  val > = Value(5, ">")
  val < = Value(6, "<")
  val IsNull = Value(7, "IS NULL")
  val IsNotNull = Value(8, "IS NOT NULL")
  val Like = Value(9, "LIKE")
}


case class Index[A <: Mapper[A]](columns: IndexItem[A]*)

abstract class IndexItem[A <: Mapper[A]] {
  def field: BaseMappedField
  def indexDesc: String
}

case class IndexField[A <: Mapper[A], T](field: MappedField[T, A]) extends IndexItem[A] {
  def indexDesc: String = field.dbColumnName
}
case class BoundedIndexField[A <: Mapper[A]](field: MappedField[String, A], len: Int) extends IndexItem[A] {
  def indexDesc: String = field.dbColumnName+"("+len+")"
}

trait QueryParam[O<:Mapper[O]]
case class Cmp[O<:Mapper[O], T](field: MappedField[T,O], opr: OprEnum.Value, value: Can[T], otherField: Can[MappedField[T, O]]) extends QueryParam[O]
case class OrderBy[O<:Mapper[O], T](field: MappedField[T,O],
                                    order: AscOrDesc) extends QueryParam[O]

trait AscOrDesc {
  def sql: String
}

case object Ascending extends AscOrDesc {
  def sql: String = " ASC "
}

case object Descending extends AscOrDesc {
  def sql: String = " DESC "
}

case class OrderBySql[O <: Mapper[O]](sql: String) extends QueryParam[O]

case class ByList[O<:Mapper[O], T](field: MappedField[T,O], vals: List[T]) extends QueryParam[O]
case class BySql[O<:Mapper[O]](query: String, params: Any*) extends QueryParam[O]
case class MaxRows[O<:Mapper[O]](max: Long) extends QueryParam[O]
case class StartAt[O<:Mapper[O]](start: Long) extends QueryParam[O]
case class Ignore[O <: Mapper[O]]() extends QueryParam[O]

abstract class InThing[OuterType <: Mapper[OuterType]] extends QueryParam[OuterType] {
  type JoinType
  type InnerType <: Mapper[InnerType]

  def outerField: MappedField[JoinType, OuterType]
  def innerField: MappedField[JoinType, InnerType]
  def innerMeta: MetaMapper[InnerType]
  def queryParams: List[QueryParam[InnerType]]
}

case class Join[TheType <: Mapper[TheType]](field: MappedForeignKey[_, TheType, _])
extends QueryParam[TheType]

case class InRaw[TheType <:
                 Mapper[TheType], T](field: MappedField[T, TheType],
                                     rawSql: String,
                                     checkedBy: IHaveValidatedThisSQL)
extends QueryParam[TheType]

object In {
  def fk[InnerMapper <: Mapper[InnerMapper], JoinTypeA,
         Zoom <% QueryParam[InnerMapper],
         OuterMapper <:KeyedMapper[JoinTypeA, OuterMapper]]
  (fielda: MappedForeignKey[JoinTypeA, InnerMapper, OuterMapper],
   qp: Zoom*): InThing[OuterMapper]
  = {
    new InThing[OuterMapper] {
      type JoinType = JoinTypeA
      type InnerType = InnerMapper

      val outerField: MappedField[JoinType, OuterMapper] = fielda.dbKeyToTable.primaryKeyField
      val innerField: MappedField[JoinType, InnerMapper] = fielda
      val innerMeta: MetaMapper[InnerMapper] = fielda.fieldOwner.getSingleton

      val queryParams: List[QueryParam[InnerMapper]] =
      qp.map{v => val r: QueryParam[InnerMapper] = v; r}.toList
    }
  }

  def apply[InnerMapper <: Mapper[InnerMapper], JoinTypeA,
            Zoom <% QueryParam[InnerMapper],
            OuterMapper <: Mapper[OuterMapper]]
  (outerField: MappedField[JoinTypeA, OuterMapper],
   innerField: MappedField[JoinTypeA, InnerMapper],
   qp: Zoom*): InThing[OuterMapper]
  = {
    new InThing[OuterMapper] {
      type JoinType = JoinTypeA
      type InnerType = InnerMapper

      val outerField: MappedField[JoinType, OuterMapper] = outerField
      val innerField: MappedField[JoinType, InnerMapper] = innerField
      val innerMeta: MetaMapper[InnerMapper] = innerField.fieldOwner.getSingleton

      val queryParams: List[QueryParam[InnerMapper]] = {

        qp.map{v => val r: QueryParam[InnerMapper] = v; r}.toList
      }
    }
  }


}

object Like {
  def apply[O <: Mapper[O]](field: MappedField[String, O], value: String) =
  Cmp[O, String](field, OprEnum.Like, Full(value), Empty)
}

object By {
  import OprEnum._

  def apply[O <: Mapper[O], T, U <% T](field: MappedField[T, O], value: U) = Cmp[O,T](field, Eql, Full(value), Empty)
  def apply[O <: Mapper[O],T,  Q <: KeyedMapper[T, Q]](field: MappedForeignKey[T, O, Q], value: Q) =
  Cmp[O,T](field, Eql, Full(value.primaryKeyField.is), Empty)

  def apply[O <: Mapper[O],T, Q <: KeyedMapper[T, Q]](field: MappedForeignKey[T, O, Q], value: Can[Q]) =
  value match {
    case Full(v) => Cmp[O,T](field, Eql, Full(v.primaryKeyField.is), Empty)
    case _ => Cmp(field, IsNull, Empty, Empty)
  }
}

object NotBy {
  import OprEnum._

  def apply[O <: Mapper[O], T, U <% T](field: MappedField[T, O], value: U) = Cmp[O,T](field, <>, Full(value), Empty)
  def apply[O <: Mapper[O],T,  Q <: KeyedMapper[T, Q]](field: MappedForeignKey[T, O, Q], value: Q) =
  Cmp[O,T](field, <>, Full(value.primaryKeyField.is), Empty)
  def apply[O <: Mapper[O],T, Q <: KeyedMapper[T, Q]](field: MappedForeignKey[T, O, Q], value: Can[Q]) =
  value match {
    case Full(v) => Cmp[O,T](field, <>, Full(v.primaryKeyField.is), Empty)
    case _ => Cmp(field, IsNotNull, Empty, Empty)
  }
}

object ByRef {
  import OprEnum._

  def apply[O <: Mapper[O], T](field: MappedField[T, O], otherField: MappedField[T,O]) = Cmp[O,T](field, Eql, Empty, Full(otherField))
}

object NotByRef {
  import OprEnum._

  def apply[O <: Mapper[O], T](field: MappedField[T, O], otherField: MappedField[T,O]) = Cmp[O,T](field, <>, Empty, Full(otherField))
}

object By_> {
  import OprEnum._

  def apply[O <: Mapper[O], T, U <% T](field: MappedField[T, O], value: U) = Cmp[O,T](field, >, Full(value), Empty)
  def apply[O <: Mapper[O], T](field: MappedField[T, O], otherField: MappedField[T,O]) = Cmp[O,T](field, >, Empty, Full(otherField))
}

object By_< {
  import OprEnum._

  def apply[O <: Mapper[O], T, U <% T](field: MappedField[T, O], value: U) = Cmp[O,T](field, <, Full(value), Empty)
  def apply[O <: Mapper[O], T](field: MappedField[T, O], otherField: MappedField[T,O]) = Cmp[O,T](field, <, Empty, Full(otherField))
}

object NullRef {
  import OprEnum._
  def apply[O <: Mapper[O], T](field: MappedField[T, O]) = Cmp(field, IsNull, Empty, Empty)
}

object NotNullRef {
  import OprEnum._
  def apply[O <: Mapper[O], T](field: MappedField[T, O]) = Cmp(field, IsNotNull, Empty, Empty)
}

trait LongKeyedMetaMapper[A <: LongKeyedMapper[A]] extends KeyedMetaMapper[Long, A] { self: A => }


trait KeyedMetaMapper[Type, A<:KeyedMapper[Type, A]] extends MetaMapper[A] with KeyedMapper[Type, A] {
  self: A  with MetaMapper[A] with KeyedMapper[Type, A] =>

  private def testProdArity(prod: Product): Boolean = {
    var pos = 0
    while (pos < prod.productArity) {
      if (!prod.productElement(pos).isInstanceOf[QueryParam[A]]) return false
      pos = pos + 1
    }
    true
  }

  type Q = MappedForeignKey[AnyBound, A, OO] with MappedField[AnyBound, A] forSome
  {type OO <: KeyedMapper[AnyBound, OO]}

  def asSafeJs(actual: A, f: KeyObfuscator): JsExp = {
    val pk = actual.primaryKeyField
    val first = (pk.name, JE.Str(f.obscure(self, pk.is)))
    JE.JsObj(first :: ("$lift_class", JE.Str(dbTableName)) :: mappedFieldList.
             map(f => this.??(f.method, actual)).
             filter(f => !f.dbPrimaryKey_? && f.renderJs_?).flatMap{
        case fk:  Q =>
          val key = f.obscure(fk.dbKeyToTable, fk.is)
          List((fk.name, JE.Str(key)),
               (fk.name+"_obj",
                JE.AnonFunc("index", JE.JsRaw("return index["+key.encJs+"];").cmd)))

        case x => x.asJs}.toList :::
             actual.suplementalJs(Full(f)) :_*)
  }

  private def convertToQPList(prod: Product): Array[QueryParam[A]] = {
    var pos = 0
    val ret = new Array[QueryParam[A]](prod.productArity)
    while (pos < prod.productArity) {
      ret(pos) = prod.productElement(pos).asInstanceOf[QueryParam[A]]
      pos = pos + 1
    }
    ret
  }

  private def anyToFindString(in: Any): Can[String] =
  in match {
    case Empty | None | null | Failure(_, _, _) => Empty
    case Full(n) => anyToFindString(n)
    case Some(n) => anyToFindString(n)
    case v => Full(v.toString)
  }

  def find(key: Any): Can[A] =
  key match {
    case qp: QueryParam[A] => find(List(qp.asInstanceOf[QueryParam[A]]) :_*)
    case prod: Product if (testProdArity(prod)) => find(convertToQPList(prod) :_*)
    case key => anyToFindString(key) flatMap (find(_))
  }

  def findDb(dbId: ConnectionIdentifier, key: Any): Can[A] =
  key match {
    case qp: QueryParam[A] => findDb(dbId, List(qp.asInstanceOf[QueryParam[A]]) :_*)
    case prod: Product if (testProdArity(prod)) => findDb(dbId, convertToQPList(prod) :_*)
    case key => anyToFindString(key) flatMap (find(dbId, _))
  }

  def find(key: String): Can[A] = dbStringToKey(key) flatMap (realKey => findDbByKey(selectDbForKey(realKey), realKey))

  def find(dbId: ConnectionIdentifier, key: String): Can[A] =  dbStringToKey(key) flatMap (realKey =>  findDbByKey(dbId, realKey))

  def findByKey(key: Type) : Can[A] = findDbByKey(dbDefaultConnectionIdentifier, key)

  def dbStringToKey(in: String): Can[Type] = primaryKeyField.convertKey(in)

  private def selectDbForKey(key: Type): ConnectionIdentifier =
  if (dbSelectDBConnectionForFind.isDefinedAt(key)) dbSelectDBConnectionForFind(key)
  else dbDefaultConnectionIdentifier

  def dbSelectDBConnectionForFind: PartialFunction[Type, ConnectionIdentifier] = Map.empty

  def findDbByKey(dbId: ConnectionIdentifier, key: Type) : Can[A] =
  findDbByKey(dbId, mappedFields, key)

  def findDbByKey(dbId: ConnectionIdentifier, fields: Seq[SelectableField],
                  key: Type) : Can[A] =
  DB.use(dbId) { conn =>
    val field = primaryKeyField

    DB.prepareStatement("SELECT "+
                        fields.map(_.dbSelectString).
                        mkString(", ")+
                        " FROM "+dbTableName+" WHERE "+field.dbColumnName+" = ?", conn) {
      st =>
      st.setObject(1, field.makeKeyJDBCFriendly(key), field.targetSQLType(field.dbColumnName))
      DB.exec(st) {
        rs =>
        val mi = buildMapper(rs)
        if (rs.next) Full(createInstance(dbId, rs, mi._1, mi._2))
        else Empty
      }
    }
  }

  def find(by: QueryParam[A]*): Can[A] = findDb(dbDefaultConnectionIdentifier, by :_*)

  def findDb(dbId: ConnectionIdentifier, by: QueryParam[A]*): Can[A] =
  findDb(dbId, mappedFields, by :_*)

  def findDb(dbId: ConnectionIdentifier, fields: Seq[SelectableField],
             by: QueryParam[A]*): Can[A] = {
    DB.use(dbId) {
      conn =>
      val bl = by.toList
      val (query, start, max) = addEndStuffs(addFields("SELECT "+
                                                       fields.map(_.dbSelectString).
                                                       mkString(", ")+
                                                       " FROM "+dbTableName+" ",false,  bl), bl, conn)
      DB.prepareStatement(query, conn) {
        st =>
        setStatementFields(st, bl, 1)
        DB.exec(st) {
          rs =>
          val mi = buildMapper(rs)
          if (rs.next) Full(createInstance(dbId, rs, mi._1, mi._2))
          else Empty
        }

      }
    }
  }

  override def afterSchemifier {
    if (crudSnippets_?) {
      LiftRules.appendSnippet(crudSnippets)
    }
  }

  /**
   * Override this definition in your model to enable CRUD snippets
   * for that model. Set to false by default.
   *
   * Remember to override editSnippetSetup and viewSnippetSetup as well,
   * as the defaults are broken.
   *
   * @return false
   */
  def crudSnippets_? = false

  /**
   * Defines the default CRUD snippets. Override if you want to change
   * the names of the snippets. Defaults are "add", "edit", and "view".
   *
   * (No, there's no D in CRUD.)
   */
  def crudSnippets: LiftRules.SnippetPF = {
    val Name = _dbTableName

    NamedPF("crud "+Name) {
      case Name :: "add"  :: _ => addSnippet
      case Name :: "edit" :: _ => editSnippet
      case Name :: "view" :: _ => viewSnippet
    }
  }

  /**
   * Default snippet for modification. Used by the default add and edit snippets.
   */
  def modSnippet(xhtml: NodeSeq, obj: A, cleanup: (A => Unit)): NodeSeq = {
    val name = _dbTableName

    def callback() {
      cleanup(obj)
    }

    xbind(name, xhtml)(obj.fieldPF orElse obj.fieldMapperPF(_.toForm.openOr(Text(""))) orElse {
        case "submit" => label => SHtml.submit(label.text, callback _)
      })
  }

  /**
   * Default add snippet. Override to change behavior of the add snippet.
   */
  def addSnippet(xhtml: NodeSeq): NodeSeq = {
    modSnippet(xhtml, addSnippetSetup, addSnippetCallback _)
  }

  /**
   * Default edit snippet. Override to change behavior of the edit snippet.
   */
  def editSnippet(xhtml: NodeSeq): NodeSeq = {
    modSnippet(xhtml, editSnippetSetup, editSnippetCallback _)
  }

  /**
   * Default view snippet. Override to change behavior of the view snippet.
   */
  def viewSnippet(xhtml: NodeSeq): NodeSeq = {
    val Name = _dbTableName
    val obj: A = viewSnippetSetup

    xbind(Name, xhtml)(obj.fieldPF orElse obj.fieldMapperPF(_.asHtml))
  }

  /**
   * Lame attempt at automatically getting an object from the HTTP parameters.
   * BROKEN! DO NOT USE! Only here so that existing sub-classes KeyedMetaMapper
   * don't have to implement new methods when I commit the CRUD snippets code.
   */
  def objFromIndexedParam: Can[A] = {
    val found = for (
      req <- S.request.toList;
      (param, value :: _) <- req.params;
      fh <- mappedFieldList if fh.field.dbIndexed_? == true && fh.name.equals(param)
    ) yield find(value)

    found.filter(obj => obj match {
        case Full(obj) => true
        case _         => false
      }) match {
      case obj :: _ => obj
      case _        => Empty
    }
  }

  /**
   * Default setup behavior for the add snippet. Creates a new mapped object.
   *
   * @return new mapped object
   */
  def addSnippetSetup: A = {
    this.create
  }

  /**
   * Default setup behavior for the edit snippet. BROKEN! MUST OVERRIDE IF
   * USING CRUD SNIPPETS!
   *
   * @return a mapped object of this metamapper's type
   */
  def editSnippetSetup: A = {
    objFromIndexedParam.open_!
  }
  /**
   * Default setup behavior for the view snippet. BROKEN! MUST OVERRIDE IF
   * USING CRUD SNIPPETS!
   *
   * @return a mapped object of this metamapper's type
   */
  def viewSnippetSetup: A = {
    objFromIndexedParam.open_!
  }
  /**
   * Default callback behavior of the edit snippet. Called when the user
   * presses submit. Saves the passed in object.
   *
   * @param obj mapped object of this metamapper's type
   */
  def editSnippetCallback(obj: A) { obj.save }
  /**
   * Default callback behavior of the add snippet. Called when the user
   * presses submit. Saves the passed in object.
   *
   * @param obj mapped object of this metamapper's type
   */
  def addSnippetCallback(obj: A) { obj.save }
}

case class FieldHolder[T](name: String, method: Method, field: MappedField[_, T])

class KeyObfuscator {
  var to: Map[String, Map[Any, String]] = Map.empty
  var from: Map[String, Map[String, Any]] = Map.empty

  def obscure[KeyType, MetaType <: KeyedMapper[KeyType, MetaType]](theType:
                                                                   KeyedMetaMapper[KeyType, MetaType], key: KeyType): String = synchronized {
    val local: Map[Any, String] = to.getOrElse(theType.dbTableName, Map.empty)
    local.get(key) match {
      case Some(s) => s
      case _ => val ret = "r"+randomString(15)

        val l2: Map[Any, String] = local + ( (key -> ret) )
        to = to + ( (theType.dbTableName -> l2) )

        val lf: Map[String, Any] = from.getOrElse(theType.dbTableName, Map.empty) + ( (ret -> key))
        // lf(ret) = key
        from = from + ( (theType.dbTableName -> lf) )

        ret
    }
  }

  def obscure[KeyType, MetaType <: KeyedMapper[KeyType, MetaType]](what: KeyedMapper[KeyType, MetaType]): String =
  {
    obscure(what.getSingleton, what.primaryKeyField.is)
  }

  def apply[KeyType, MetaType <: KeyedMapper[KeyType, MetaType], Q <% KeyType](theType:
                                                                               KeyedMetaMapper[KeyType, MetaType], key: Q): String = {
    val k: KeyType = key
    obscure(theType, k)
  }

  def apply[KeyType, MetaType <: KeyedMapper[KeyType, MetaType]](what: KeyedMapper[KeyType, MetaType]): String =
  {
    obscure(what)
  }

  def recover[KeyType, MetaType <: KeyedMapper[KeyType, MetaType]](theType:
                                                                   KeyedMetaMapper[KeyType, MetaType], id: String): Can[KeyType] = synchronized {
    Can(from.get(theType.dbTableName)).flatMap(h => Can(h.get(id)).map(_.asInstanceOf[KeyType]))
  }
}

case class IHaveValidatedThisSQL(who: String, date: String)

trait SelectableField {
  def dbSelectString: String
}
