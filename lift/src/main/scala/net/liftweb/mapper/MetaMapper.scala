package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.collection.mutable._
import java.lang.reflect.Method
import java.sql.{ResultSet, Types, PreparedStatement, Statement}
import scala.xml.{Elem, Node, Text, NodeSeq, Null, TopScope, UnprefixedAttribute, MetaData}
import net.liftweb.util.Helpers._
import java.util.Date

trait BaseMetaMapper {
  type RealType <: Mapper[RealType]
  
  def beforeSchemifier: Unit
  def afterSchemifier: Unit
  
  def dbTableName: String
  def mappedFields: Seq[BaseMappedField];
  def dbAddTable: Option[() => Unit]
  
  def dbIndexes: List[Index[RealType]]  
}

trait MetaMapper[A<:Mapper[A]] extends BaseMetaMapper with Mapper[A] {
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
  
  def dbDefaultConnectionIdentifier: ConnectionIdentifier = DefaultConnectionIdentifier
  
  def findAll: List[A] = findMapDb(dbDefaultConnectionIdentifier, Nil :_*)(v => Some(v))

  def findAllDb(dbId:ConnectionIdentifier): List[A] =  findMapDb(dbId, Nil :_*)(v => Some(v))

  def countByInsecureSql(query: String, IDidASecurityAuditOnThisQuery: boolean): long = countByInsecureSqlDb(dbDefaultConnectionIdentifier, query, IDidASecurityAuditOnThisQuery)

  def countByInsecureSqlDb(dbId: ConnectionIdentifier, query: String, IDidASecurityAuditOnThisQuery: boolean): long =
    if (!IDidASecurityAuditOnThisQuery) -1L
    else DB.use(dbId)(DB.prepareStatement(query, _)(DB.exec(_)(rs => if (rs.next) rs.getLong(1) else 0L))) 
  
  def findAllByInsecureSql(query: String, IDidASecurityAuditOnThisQuery: boolean): List[A] = findAllByInsecureSqlDb(dbDefaultConnectionIdentifier, query, IDidASecurityAuditOnThisQuery)

  def findAllByInsecureSqlDb(dbId: ConnectionIdentifier, query: String, IDidASecurityAuditOnThisQuery: boolean): List[A] = 
    findMapByInsecureSqlDb(dbId, query, IDidASecurityAuditOnThisQuery)(a => Some(a))
  

  def findMapByInsecureSql[T](query: String, IDidASecurityAuditOnThisQuery: boolean)(f: A => Option[T]): List[T] = findMapByInsecureSqlDb(dbDefaultConnectionIdentifier, query, IDidASecurityAuditOnThisQuery)(f)

  def findMapByInsecureSqlDb[T](dbId: ConnectionIdentifier, query: String, IDidASecurityAuditOnThisQuery: boolean)(f: A => Option[T]): List[T] = {
    DB.use(dbId) {
      conn =>
        if (!IDidASecurityAuditOnThisQuery) Nil
        else DB.prepareStatement(query, conn) {
          st =>
            DB.exec(st) {
              rs =>
                createInstances(dbId, rs, None, None, f)
            }
        }
    }
  }
  
  def dbAddTable: Option[() => unit] = None
  
  def count: long = countDb(dbDefaultConnectionIdentifier, Nil :_*)

  def count(by: QueryParam[A]*): long = countDb(dbDefaultConnectionIdentifier, by:_*)
  
  def countDb(dbId: ConnectionIdentifier, by: QueryParam[A]*): long = {
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
  
  def findAll(by: QueryParam[A]*): List[A] = findMapDb(dbDefaultConnectionIdentifier, by :_*)(v => Some(v))
  def findAllDb(dbId: ConnectionIdentifier,by: QueryParam[A]*): List[A] = findMapDb(dbId, by :_*)(v => Some(v))

  def findMap[T](by: QueryParam[A]*)(f: A => Option[T]) = findMapDb(dbDefaultConnectionIdentifier, by :_*)(f)

  def findMapDb[T](dbId: ConnectionIdentifier, by: QueryParam[A]*)(f: A => Option[T]) : List[T] = {
    DB.use(dbId) {
      conn =>
	val bl = by.toList
      val (query, start, max) = addEndStuffs(addFields("SELECT * FROM "+dbTableName+"  ", false, bl), bl, conn)
      DB.prepareStatement(query, conn) {
	st =>
          setStatementFields(st, bl, 1)
	DB.exec(st)(createInstances(dbId, _, start, max, f))
      }
    }
  }

  def create: A = createInstance

  private[mapper] def addFields(what: String,whereAdded: boolean, by: List[QueryParam[A]]): String = {

    var wav = whereAdded

    def whereOrAnd = if (wav) " AND " else {wav = true; " WHERE "}    
    
    by match {
      case Nil => what
      case x :: xs => {
        var updatedWhat = what        
        x match {
          case Cmp(field, opr, Some(_), _) =>
            (1 to field.dbColumnCount).foreach {
              cn =>
                updatedWhat = updatedWhat + whereOrAnd +field.dbColumnNames(field.name)(cn - 1)+" "+opr+" ? "
            }
          
          case Cmp(field, opr, _, Some(otherField)) =>
            (1 to field.dbColumnCount).foreach {
              cn =>
                updatedWhat = updatedWhat + whereOrAnd +field.dbColumnNames(field.name)(cn - 1)+" "+opr+" "+
              otherField.dbColumnNames(otherField.name)(cn - 1)
            }
            
          case Cmp(field, opr, None, None) =>
          (1 to field.dbColumnCount).foreach (cn => updatedWhat = updatedWhat + whereOrAnd +field.dbColumnNames(field.name)(cn - 1)+" "+opr+" ")

          case BySql(query, _*) => 
            updatedWhat = updatedWhat + whereOrAnd + " ( "+ query +" ) "
          case _ => 
        }
        addFields(updatedWhat,wav, xs)
      }
    }
  }
  
  private[mapper] def setStatementFields(st: PreparedStatement, by: List[QueryParam[A]], curPos: int) {
    by match {
      case Nil => {}
      case Cmp(field, _, Some(value), _) :: xs =>
        st.setObject(curPos, field.convertToJDBCFriendly(value), field.getTargetSQLType)
      setStatementFields(st, xs, curPos + 1)

      case BySql(query, params @ _*) :: xs => {
        params.toList match {
          case Nil => setStatementFields(st, xs, curPos)
          case List(i: int) => 
            st.setInt(curPos, i)
          setStatementFields(st, xs, curPos + 1)
          case List(lo: long) => 
            st.setLong(curPos, lo)
          setStatementFields(st, xs, curPos + 1)
          case List(s: String) => 
            st.setString(curPos, s)
          setStatementFields(st, xs, curPos + 1)
          case List(d: Date) => 
            st.setDate(curPos, new java.sql.Date(d.getTime))
          setStatementFields(st, xs, curPos + 1)
          case List(field: BaseMappedField) => st.setObject(curPos, field.getJDBCFriendly, field.getTargetSQLType)
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
  
  // def find(by: QueryParam): Option[A] = find(List(by))
  
  private def _addOrdering(in: String, params: List[QueryParam[A]]): String = {
    val lst = params.flatMap{p => p match {case OrderBy(field, ascending) => List(field.dbColumnName+" "+(if (ascending) "ASC" else "DESC")); case _ => Nil}} 
    if (lst.length == 0) in
    else in+" ORDER BY "+lst.mkString(" , ")
  }
  
  def addEndStuffs(in: String, params: List[QueryParam[A]], conn: SuperConnection): (String, Option[long], Option[long]) = {
    val tmp = _addOrdering(in, params)
    val max = params.foldRight(None.asInstanceOf[Option[long]]){(a,b) => a match {case MaxRows(n) => Some(n); case _ => b}}
    val start = params.foldRight(None.asInstanceOf[Option[long]]){(a,b) => a match {case StartAt(n) => Some(n); case _ => b}}

    if (conn.brokenLimit_?.get) (tmp, start, max) else {
      ((if (max.isDefined && start.isDefined) {
	tmp + " LIMIT "+start.get+","+max.get
      } else if (max.isDefined) {
	tmp + " LIMIT "+max.get
      } else if (start.isDefined) {
	tmp + " LIMIT "+start.get+","+java.lang.Long.MAX_VALUE
      } else tmp), None, None)}
  }
  
  def delete_!(toDelete : A) : boolean = {
    DB.use(toDelete.connectionIdentifier) {
      conn =>
	_beforeDelete(toDelete)
      val ret = DB.prepareStatement("DELETE FROM "+dbTableName +" WHERE "+indexMap+" = ?", conn) {
	st =>
	  val indVal = indexedField(toDelete)
	indVal.map{indVal =>
	  st.setObject(1, indVal.getJDBCFriendly(indexMap), indVal.getTargetSQLType(indexMap))

		   st.executeUpdate == 1
		 } getOrElse false
      }
      _afterDelete(toDelete)
      ret
    }
  }
  

  
  private def ??(meth : Method, inst :A) = meth.invoke(inst, null).asInstanceOf[MappedField[Any, A]]
  
  def dirty_?(toTest : A) : boolean = {
    mappedFieldArray.foreach {
      mft =>      
	if (??(mft._2, toTest).dirty_?) return true
    }
    false
  }
  
  def indexedField(toSave : A) : Option[MappedField[Any, A]] = {
    if (indexMap eq null) None else 
      Some(??(mappedColumns(indexMap), toSave))
  }
  
  
  def saved_?(toSave : A) : boolean = {
    if (indexMap eq null) true else {
      indexedField(toSave).get.dbIndexFieldIndicatesSaved_?
    }
  }
  
  def whatToSet(toSave : A) : String = {
    mappedColumns.elements.filter{c => ??(c._2, toSave).dirty_?}.map{c => c._1 + " = ?"}.toList.mkString("", ",", "")
  }
  
  def validate(toValidate : A) : List[ValidationIssue] = {
    val saved_? = this.saved_?(toValidate)
    _beforeValidation(toValidate)
    if (saved_?) _beforeValidationOnUpdate(toValidate) else _beforeValidationOnCreate(toValidate)
    
    var ret : List[ValidationIssue] = Nil
    
    mappedFieldArray.foreach{f => ret = ret ::: ??(f._2, toValidate).validate}

    _afterValidation(toValidate)
    if (saved_?) _afterValidationOnUpdate(toValidate) else _afterValidationOnCreate(toValidate)

    ret
  }
  
  val elemName = getClass.getSuperclass.getName.split("\\.").toList.last
  
  def toXml(what: A): NodeSeq = {
    
    Elem(null,elemName,
         mappedFieldArray.elements.foldRight(Null.asInstanceOf[MetaData]) {(p, md) => val fld = ??(p._2, what)
									   new UnprefixedAttribute(p._1, fld.toString, md)}
         ,TopScope)
    //    Elem("", 
    //    (mappedFieldArray.elements.map{p => ??(p._2, in).asString}).toList.mkString("", ",", "")
  }
  
  def save(toSave : A) : boolean = {
    DB.use(toSave.connectionIdentifier) { 
      conn =>
	_beforeSave(toSave)
      val ret = if (saved_?(toSave)) {
	if (!dirty_?(toSave)) true else {
	  _beforeUpdate(toSave)
	  val ret = DB.prepareStatement("UPDATE "+dbTableName+" SET "+whatToSet(toSave)+" WHERE "+indexMap+" = ?", conn) {
	    st =>
	      var colNum = 1
	    
	    for (col <- mappedColumns.elements) {
	      val colVal = ??(col._2, toSave)
	      if (!columnPrimaryKey_?(col._1) && colVal.dirty_?) {
                colVal.getTargetSQLType(col._1) match {
                  case Types.VARCHAR => st.setString(colNum, colVal.getJDBCFriendly(col._1).asInstanceOf[String])
                  
                  case _ => st.setObject(colNum, colVal.getJDBCFriendly(col._1), colVal.getTargetSQLType(col._1))
                }
		colNum = colNum + 1
	      }
	    }
	    
	    val indVal = indexedField(toSave)
	    st.setObject(colNum, indVal.get.getJDBCFriendly(indexMap), indVal.get.getTargetSQLType(indexMap))
	    st.executeUpdate
	    true
	  }
	  _afterUpdate(toSave)
	  ret
	}
      } else {
	_beforeCreate(toSave)
	val ret = DB.prepareStatement("INSERT INTO "+dbTableName+" ("+columnNamesForInsert+") VALUES ("+columnQueriesForInsert+")", Statement.RETURN_GENERATED_KEYS, conn) {
	  st =>
	    var colNum = 1
	  for (col <- mappedColumns.elements) {
	    if (!columnPrimaryKey_?(col._1)) {
	      val colVal = col._2.invoke(toSave, null).asInstanceOf[MappedField[AnyRef, A]]
                colVal.getTargetSQLType(col._1) match {
                  case Types.VARCHAR => st.setString(colNum, colVal.getJDBCFriendly(col._1).asInstanceOf[String])
                  
                  case _ => st.setObject(colNum, colVal.getJDBCFriendly(col._1), colVal.getTargetSQLType(col._1))
                }
              
	      // st.setObject(colNum, colVal.getJDBCFriendly(col._1), colVal.getTargetSQLType(col._1))
	      colNum = colNum + 1
	    }
	  }
	  
	  val updateCnt = st.executeUpdate
	  if (indexMap ne null) {
	    val rs = st.getGeneratedKeys
	    try {
	      if (rs.next) {
		val meta = rs.getMetaData
		toSave.runSafe {
		  findApplier(indexMap, rs.getObject(1)) match {
		    case null => 
		    case ap @ _ => ap.get.apply(toSave, rs.getObject(1))
		    
		  }
		}
	      }
	    } finally {
	      rs.close
	    }
	  }
	  updateCnt == 1
	}
	_afterCreate(toSave)
	ret
      }
      _afterSave(toSave)
      ret
    }
  }
  
  def columnPrimaryKey_?(name : String) = {
    
    mappedColumnInfo.get(name) match {
      case None => false
      case Some(v) => v.dbPrimaryKey_?
    }
  }

  def createInstances(dbId: ConnectionIdentifier, rs: ResultSet, start: Option[long], omax: Option[long]) : List[A] = createInstances(dbId, rs, start, omax, v => Some(v))

  
  def createInstances[T](dbId: ConnectionIdentifier, rs: ResultSet, start: Option[long], omax: Option[long], f: A => Option[T]) : List[T] = {
    var ret = new ListBuffer[T]
    val bm = buildMapper(rs)
    var pos = (start getOrElse 0L) * -1L
    val max = omax getOrElse java.lang.Long.MAX_VALUE

    while (pos < max && rs.next()) {
      if (pos >= 0L) {
        f(createInstance(dbId, rs, bm._1, bm._2)).foreach(v => ret += v)
      }
      pos = pos + 1L
    }
    
    ret.toList
  }
  
  def appendFieldToStrings(in : A) : String = {
    (mappedFieldArray.elements.map{p => ??(p._2, in).asString}).toList.mkString("", ",", "")
  }
  
  private val columnNameToMappee = new HashMap[String, Option[(ResultSet,int,A) => unit]]
  
  def buildMapper(rs: ResultSet): (int, Array[(ResultSet,int,A) => unit]) = synchronized {
    val meta = rs.getMetaData
    val colCnt = meta.getColumnCount
    val ar = new Array[(ResultSet,int,A) => unit](colCnt + 1)
    for (pos <- 1 to colCnt) {
      val colName = meta.getColumnName(pos).toLowerCase
      val optFunc = columnNameToMappee.get(colName) match {
        case None => {
	  val colType = meta.getColumnType(pos)      
	  val fieldInfo = mappedColumns.get(colName)
	  val setTo = 
	    if (fieldInfo != None) {
              val tField = fieldInfo.get.invoke(this, null).asInstanceOf[MappedField[AnyRef, A]]
              Some(colType match {
		case Types.INTEGER | Types.BIGINT => {
		  val bsl = tField.buildSetLongValue(fieldInfo.get, colName)
		  (rs: ResultSet, pos: int, objInst: A) => bsl(objInst, rs.getLong(pos), rs.wasNull)}
		case Types.VARCHAR => {
		  val bsl = tField.buildSetStringValue(fieldInfo.get, colName)
		  (rs: ResultSet, pos: int, objInst: A) => bsl(objInst, rs.getString(pos))}
		case Types.DATE | Types.TIME | Types.TIMESTAMP =>
		  val bsl = tField.buildSetDateValue(fieldInfo.get, colName)                  
		(rs: ResultSet, pos: int, objInst: A) => bsl(objInst, rs.getTimestamp(pos))
		case Types.BOOLEAN | Types.BIT =>{
		  val bsl = tField.buildSetBooleanValue(fieldInfo.get, colName)
		  (rs: ResultSet, pos: int, objInst: A) => bsl(objInst, rs.getBoolean(pos), rs.wasNull)}
		case _ => {
		  (rs: ResultSet, pos: int, objInst: A) => {
		    val res = rs.getObject(pos)
		    findApplier(colName, res) match {
		      case None =>
			case Some(f) => f(objInst, res)
		    }
		  }
		}
              })
	    } else None
	  
	  columnNameToMappee(colName) = setTo
	  setTo
        }
        case Some(of) => of
      }
      ar(pos) = optFunc match {
        case Some(f) => f
        case _ => null
      }
    }
    (colCnt, ar)
  }

  def createInstance(dbId: ConnectionIdentifier, rs : ResultSet, colCnt:int, mapFuncs: Array[(ResultSet,Int,A) => Unit]) : A = {
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
  
  protected def  findApplier(name : String, inst : AnyRef) : Option[((A, AnyRef) => unit)] = synchronized {
    val clz = inst match {
      case null => null
      case _ => inst.getClass
    }
    val look = (name.toLowerCase, if (clz ne null) Some(clz) else None)
      mappedAppliers.get(look) match {
	case s @ Some(_) => s
	case None => {
          val newFunc = createApplier(name, inst, clz)
          mappedAppliers(look) = newFunc
          Some(newFunc)
	}
      }
  }
  

  private def createApplier(name : String, inst : AnyRef, clz : Class) : (A, AnyRef) => unit = {
    val accessor = mappedColumns.get(name)
    if ((accessor eq null) || accessor == None) null else {
      (accessor.get.invoke(this, null).asInstanceOf[MappedField[AnyRef, A]]).buildSetActualValue(accessor.get, inst, name)
    }
  }
  
  
  def checkFieldNames(in: A) {
    var pos = 0
    var len = mappedFieldArray.length
    while (pos < len) {
      val f = mappedFieldArray(pos)
      ??(f._2, in) match {
        case field if (field.i_name_! eq null) => field.setName_!(f._1)
        case _ => 
      }
      pos = pos + 1
    }
  }
  
  
  def createInstance: A = rootClass.newInstance.asInstanceOf[A]
  
  def fieldOrder : List[AnyRef] = Nil
  
  protected val rootClass = this.getClass.getSuperclass
  
  private val mappedAppliers = new HashMap[(String, Option[Class]), (A, AnyRef) => unit];
  
  private val _mappedFields  = new HashMap[String, Method];
  private var mappedFieldArray : Array[(String, Method, MappedField[AnyRef,A])] = null; // new Array[Triple[String, Method, MappedField[Any,Any]]]();
  
  private var mappedCallbacks: List[(String, Method)] = Nil
  
  private val mappedColumns = new HashMap[String, Method];
  
  // private val mappedFieldInfo = new HashMap[String, MappedField[AnyRef, A]]
  private val mappedColumnInfo = new HashMap[String, MappedField[AnyRef, A]]  
  
  
  
  private var indexMap : String = null
  
  this.runSafe {
    val tArray = new ListBuffer[(String, Method, MappedField[AnyRef,A])]
    
    def isMagicObject(m: Method) = m.getReturnType.getName.endsWith("$"+m.getName+"$") && m.getParameterTypes.length == 0
    def isMappedField(m: Method) = classOf[MappedField[Nothing, A]].isAssignableFrom(m.getReturnType)
    def isLifecycle(m: Method) = classOf[LifecycleCallbacks].isAssignableFrom(m.getReturnType)
    
    mappedCallbacks = for (v <- this.getClass.getSuperclass.getMethods.toList if isMagicObject(v) && isLifecycle(v)) yield (v.getName, v)
    
    for (v <- this.getClass.getSuperclass.getMethods if isMagicObject(v) && isMappedField(v)) {
      v.invoke(this, null) match {
        case mf: MappedField[AnyRef, A] if !mf.ignoreField =>
          mf.setName_!(v.getName)
        tArray += ((mf.name, v, mf))
        for (colName <- mf.dbColumnNames(v.getName)) {
          mappedColumnInfo(colName) = mf
          mappedColumns(colName) = v
        }
        if (mf.dbPrimaryKey_?) {
          indexMap = v.getName
        }
        
        case _ =>
      }
    }
    
    def findPos(in : AnyRef) : Option[int] = {
      tArray.toList.zipWithIndex.filter(mft => in eq mft._1._3) match {
        case Nil => None
        case x :: xs => Some(x._2)
      }
    }
    
    val resArray = new ListBuffer[(String, Method, MappedField[AnyRef,A])];
    
    fieldOrder.foreach(f => findPos(f).foreach(pos => resArray += tArray.remove(pos)))
    
    tArray.foreach(mft => resArray += mft)      
    
    mappedFieldArray = resArray.toArray
    mappedFieldArray.foreach(ae => _mappedFields(ae._1) = ae._2)
  }

  val columnNamesForInsert = (mappedColumnInfo.elements.filter(!_._2.dbPrimaryKey_?).map(_._1)).toList.mkString(",")
  
  val columnQueriesForInsert = {
    (mappedColumnInfo.elements.filter(!_._2.dbPrimaryKey_?).map(p => "?")).toList.mkString(",")
  }
  
  private def fixTableName(name : String) = clean(name.toLowerCase)

  private def internalTableName_$_$ = getClass.getSuperclass.getName.split("\\.").toList.last
  
  def htmlHeaders : NodeSeq = {
    mappedFieldArray.filter{mft => mft._3.dbDisplay_?}.map {mft => <th>{mft._3.displayName}</th>}.toList
    // mappedFieldInfo.elements.filter{e => e._2.db_display_?}. map {e => <th>{e._1}</th>}.toList
  }
  
  def mappedFields: Seq[BaseMappedField] = mappedFieldArray.map(f => f._3)
  
  def doHtmlLine(toLine : A) : NodeSeq = {
    mappedFieldArray.filter{mft => mft._3.dbDisplay_?}.map {mft => <td>{??(mft._2, toLine).asHtml}</td>}.toList
  }
  
  def asHtml(toLine : A) : NodeSeq = {
    Text(internalTableName_$_$) :: Text("={ ") :: 
    mappedFieldArray.filter{mft => mft._3.dbDisplay_?}.map {
      mft => 
	val field = ??(mft._2, toLine)
      <span>{field.displayName}={field.asHtml}&nbsp;</span>}.toList :::
    List(Text(" }"))
  }
  
  def toForm(toMap : A) : NodeSeq = {
    mappedFieldArray.filter{e => e._3.dbDisplay_?}.map {
      e =>
	val field = ??(e._2, toMap)
      <tr>
      <td>{field.displayName}</td>
      <td>{field.toForm}</td>
      </tr>}.toList
  }
  
  def getActualField[T <: Any](actual: A, protoField: MappedField[T, A]): MappedField[T, A] = {
    ??(_mappedFields(protoField.name), actual).asInstanceOf[MappedField[T,A]]
  }

  def dbTableName = _dbTableName
  
  private val _dbTableName: String = {
    fixTableName(internalTableName_$_$)
  }
  
  private def eachField(what: A, toRun: List[(A) => Any])(f: (LifecycleCallbacks) => Any) {
    mappedCallbacks.foreach (e =>
      e._2.invoke(what, null) match {
        case lccb: LifecycleCallbacks => f(lccb)
        case _ =>
      }
			   )
    toRun.foreach{tf => tf(what)}
  }
  private def _beforeValidation(what: A) {eachField(what, beforeValidation) {field => field.beforeValidation}  }
  private def _beforeValidationOnCreate(what: A) {eachField(what, beforeValidationOnCreate) {field => field.beforeValidationOnCreate}  }
  private def _beforeValidationOnUpdate(what: A) {eachField(what, beforeValidationOnUpdate) {field => field.beforeValidationOnUpdate}  }
  private def _afterValidation(what: A) {eachField(what, afterValidation) {field => field.afterValidation}  }
  private def _afterValidationOnCreate(what: A) {eachField(what, afterValidationOnCreate) {field => field.afterValidationOnCreate}  }
  private def _afterValidationOnUpdate(what: A) {eachField(what, afterValidationOnUpdate) {field => field.afterValidationOnUpdate}  }

  private def _beforeSave(what: A) {eachField(what, beforeSave) {field => field.beforeSave}  }
  private def _beforeCreate(what: A) {eachField(what, beforeCreate) {field => field.beforeCreate}  }
  private def _beforeUpdate(what: A) {eachField(what, beforeUpdate) {field => field.beforeUpdate}  }

  private def _afterSave(what: A) {eachField(what, afterSave) {field => field.afterSave}  }
  private def _afterCreate(what: A) {eachField(what, afterCreate) {field => field.afterCreate}  }
  private def _afterUpdate(what: A) {eachField(what, afterUpdate) {field => field.afterUpdate}  }

  private def _beforeDelete(what: A) {eachField(what, beforeDelete) {field => field.beforeDelete}  }
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

abstract class QueryParam[O<:Mapper[O]]
//case class By[O<:Mapper[O], T](field: MappedField[T,O], value: T) extends QueryParam[O]
case class Cmp[O<:Mapper[O], T](field: MappedField[T,O], opr: OprEnum.Value, value: Option[T], otherField: Option[MappedField[T, O]]) extends QueryParam[O]
case class OrderBy[O<:Mapper[O], T](field: MappedField[T,O],ascending: boolean) extends QueryParam[O]
case class BySql[O<:Mapper[O]](query: String, params: Any*) extends QueryParam[O]
case class MaxRows[O<:Mapper[O]](max: long) extends QueryParam[O]
case class StartAt[O<:Mapper[O]](start: long) extends QueryParam[O]
//case class NotBy[O<:Mapper[O], T](field: MappedField[T, O], value: T) extends QueryParam[O]
object By {
  import OprEnum._
  
  def apply[O <: Mapper[O], T](field: MappedField[T, O], value: T) = Cmp[O,T](field, Eql, Some(value), None)
  def apply[O <: Mapper[O], T](field: MappedField[T, O], otherField: MappedField[T,O]) = Cmp[O,T](field, Eql, None, Some(otherField))
}

object NotBy {
  import OprEnum._

  def apply[O <: Mapper[O], T](field: MappedField[T, O], value: T) = Cmp[O,T](field, <>, Some(value), None)
  def apply[O <: Mapper[O], T](field: MappedField[T, O], otherField: MappedField[T,O]) = Cmp[O,T](field, <>, None, Some(otherField))
}

object ByGt {
  import OprEnum._

  def apply[O <: Mapper[O], T](field: MappedField[T, O], value: T) = Cmp[O,T](field, >, Some(value), None)
  def apply[O <: Mapper[O], T](field: MappedField[T, O], otherField: MappedField[T,O]) = Cmp[O,T](field, >, None, Some(otherField))  
}

object ByLt {
  import OprEnum._

  def apply[O <: Mapper[O], T](field: MappedField[T, O], value: T) = Cmp[O,T](field, <, Some(value), None)
  def apply[O <: Mapper[O], T](field: MappedField[T, O], otherField: MappedField[T,O]) = Cmp[O,T](field, <, None, Some(otherField))    
}

object NullRef {
  import OprEnum._
  def apply[O <: Mapper[O], T](field: MappedField[T, O]) = Cmp(field, IsNull, None, None)
}

object NotNullRef {
  import OprEnum._
  def apply[O <: Mapper[O], T](field: MappedField[T, O]) = Cmp(field, IsNotNull, None, None)
}

trait KeyedMetaMapper[Type, A<:KeyedMapper[Type, A]] extends MetaMapper[A] with KeyedMapper[Type, A] {
 
  private def testProdArity(prod: Product): boolean = {
    var pos = 0
    while (pos < prod.productArity) {
      if (!prod.productElement(pos).isInstanceOf[QueryParam[A]]) return false
      pos = pos + 1
    }
    true
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
  
  private def anyToFindString(in: Any): Option[String] =
    in match {
      case null => None
      case None => None
      case Some(n) => anyToFindString(n)
      case v => Some(v.toString)
  }
  
  def find(key: Any): Option[A] =
    key match {
  case qp: QueryParam[A] => find(List(qp.asInstanceOf[QueryParam[A]]) :_*)
  case prod: Product if (testProdArity(prod)) => find(convertToQPList(prod) :_*)
  case key => anyToFindString(key) flatMap (find(_))
  }
  
  def findDb(dbId: ConnectionIdentifier, key: Any): Option[A] = 
    key match {
    case qp: QueryParam[A] => findDb(dbId, List(qp.asInstanceOf[QueryParam[A]]) :_*)
    case prod: Product if (testProdArity(prod)) => findDb(dbId, convertToQPList(prod) :_*)
    case key => anyToFindString(key) flatMap (find(dbId, _))
    }   
  
  def find(key: String): Option[A] = dbStringToKey(key) flatMap (realKey => findDbByKey(selectDbForKey(realKey), realKey))
  
  def find(dbId: ConnectionIdentifier, key: String): Option[A] =  dbStringToKey(key) flatMap (realKey =>  findDbByKey(dbId, realKey))

  def findByKey(key: Type) : Option[A] = findDbByKey(dbDefaultConnectionIdentifier, key)
  
  def dbStringToKey(in: String): Option[Type] = primaryKeyField.convertKey(in) 
  
  private def selectDbForKey(key: Type): ConnectionIdentifier = 
    if (dbSelectDBConnectionForFind.isDefinedAt(key)) dbSelectDBConnectionForFind(key)
    else dbDefaultConnectionIdentifier
  
  def dbSelectDBConnectionForFind: PartialFunction[Type, ConnectionIdentifier] = Map.empty

  def findDbByKey(dbId: ConnectionIdentifier,key: Type) : Option[A] = 
    DB.use(dbId) { conn => 
        val field = primaryKeyField

            DB.prepareStatement("SELECT * FROM "+dbTableName+" WHERE "+field.dbColumnName+" = ?", conn) {
              st =>
                st.setObject(1, field.makeKeyJDBCFriendly(key), field.getTargetSQLType(field.dbColumnName))
              DB.exec(st) {
                rs =>
                  val mi = buildMapper(rs)
                if (rs.next) Some(createInstance(dbId, rs, mi._1, mi._2))
                else None
              }
            }
      }
    
  def find(by: QueryParam[A]*): Option[A] = findDb(dbDefaultConnectionIdentifier, by :_*)

  def findDb(dbId: ConnectionIdentifier, by: QueryParam[A]*): Option[A] = {
    DB.use(dbId) {
      conn =>
        val bl = by.toList
      val (query, start, max) = addEndStuffs(addFields("SELECT * FROM "+dbTableName+" ",false,  bl), bl, conn)
      DB.prepareStatement(query, conn) {
        st =>
          setStatementFields(st, bl, 1)
        DB.exec(st) {
          rs =>
            val mi = buildMapper(rs)
          if (rs.next) Some(createInstance(dbId, rs, mi._1, mi._2))
          else None
        }
        
      }
    }
  }
   
}
