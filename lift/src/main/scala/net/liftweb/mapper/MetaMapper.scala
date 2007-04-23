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
  def beforeSchemifier: unit
  def afterSchemifier: unit
  
  def dbTableName: String
  def mappedFields: Seq[BaseMappedField];
  def dbAddTable: Option[() => unit]
                        
}

trait MetaMapper[A<:Mapper[A]] extends BaseMetaMapper with Mapper[A] {

  def beforeValidation: List[(A) => unit] = Nil
  def beforeValidationOnCreate: List[(A) => unit] = Nil
  def beforeValidationOnUpdate: List[(A) => unit] = Nil
  def afterValidation: List[(A) => unit] = Nil
  def afterValidationOnCreate: List[(A) => unit] = Nil
  def afterValidationOnUpdate: List[(A) => unit] = Nil

  def beforeSave: List[(A) => unit] = Nil
  def beforeCreate: List[(A) => unit] = Nil
  def beforeUpdate: List[(A) => unit] = Nil

  def afterSave: List[(A) => unit] = Nil
  def afterCreate: List[(A) => unit] = Nil
  def afterUpdate: List[(A) => unit] = Nil

  def beforeDelete: List[(A) => unit] = Nil
  def afterDelete: List[(A) => unit] = Nil
  
  
  def findAll : List[A] = {
    findAll(Nil :_*)
  }
  
  // def findAll(by: QueryParam*): List[A] = findAll(List(by))
  
  def findAllByInsecureSql(query: String, IDidASecurityAuditOnThisQuery: boolean): List[A] = {
    DB.use {
      conn =>
    if (!IDidASecurityAuditOnThisQuery) Nil
    else DB.prepareStatement(query, conn) {
      st =>
	DB.exec(st) {
          rs =>
            createInstances(rs, None, None)
	}
    }
  }
  }
  
  def dbAddTable: Option[() => unit] = None
  
  def count: long = count(Nil :_*)
  
  def count(by: QueryParam[A]*): long = {
    DB.use {
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
  
  def findAll(by: QueryParam[A]*): List[A] = {
    DB.use {
      conn =>
    val bl = by.toList
    val (query, start, max) = addEndStuffs(addFields("SELECT * FROM "+dbTableName+"  ", false, bl), bl, conn)
    DB.prepareStatement(query, conn) {
      st =>
        setStatementFields(st, bl, 1)
      DB.exec(st) {
        rs =>
          createInstances(rs, start, max)
      }
    }
    }
  }

  def create: A = createInstance

  private def addFields(what: String,whereAdded: boolean, by: List[QueryParam[A]]): String = {

    var wav = whereAdded

    def whereOrAnd = if (wav) " AND " else {wav = true; " WHERE "}    
    
    by match {
      case Nil => what
      case x :: xs => {
        var updatedWhat = what        
        x match {
          case By(field, _) => 
            (1 to field.dbColumnCount).foreach {
              cn =>
		updatedWhat = updatedWhat + whereOrAnd +field.dbColumnNames(field.name)(cn - 1)+" = ? "
            }
          case BySql(query, _*) => 
            updatedWhat = updatedWhat + whereOrAnd + query
          case _ => 
        }
        addFields(updatedWhat,wav, xs)
      }
    }
  }
  
  private def setStatementFields(st: PreparedStatement, by: List[QueryParam[A]], curPos: int) {
    by match {
      case Nil => {}
      case By(field, value) :: xs => {
        st.setObject(curPos, field.convertToJDBCFriendly(value), field.getTargetSQLType)
        setStatementFields(st, xs, curPos + 1)
      }
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
    else in+" ORDER BY "+lst.mkString("", " , ", "")
  }
  
  def addEndStuffs(in: String, params: List[QueryParam[A]], conn: SuperConnection): (String, Option[long], Option[long]) = {
    val tmp = _addOrdering(in, params)
    val max = params.foldRight(None.asInstanceOf[Option[long]]){(a,b) => a match {case MaxRows(n) => Some(n); case _ => b}}
    val start = params.foldRight(None.asInstanceOf[Option[long]]){(a,b) => a match {case StartAt(n) => Some(n); case _ => b}}

    if (conn.brokenLimit_?) (tmp, start, max) else {
    ((if (max.isDefined && start.isDefined) {
      tmp + " LIMIT "+start.get+","+max.get
    } else if (max.isDefined) {
      tmp + " LIMIT "+max.get
    } else if (start.isDefined) {
      tmp + " LIMIT "+start.get+","+java.lang.Long.MAX_VALUE
    } else tmp), None, None)}
  }
  
  def find(by: QueryParam[A]*): Option[A] = {
    DB.use {
      conn =>
    val bl = by.toList
    val (query, start, max) = addEndStuffs(addFields("SELECT * FROM "+dbTableName+" ",false,  bl), bl, conn)
    DB.prepareStatement(query, conn) {
      st =>
        setStatementFields(st, bl, 1)
      DB.exec(st) {
        rs =>
          val mi = buildMapper(rs)
        if (rs.next) Some(createInstance(rs, mi._1, mi._2))
        else None
      }
      
    }
  }
  }
  
  def delete_!(toDelete : A) : boolean = {
    DB.use {
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
  
  def find(key: Any) : Option[A] = {
    def testProdArity(prod: Product): boolean = {
      var pos = 0
      while (pos < prod.arity) {
        if (!prod.element(pos).isInstanceOf[QueryParam[A]]) return false
        pos = pos + 1
      }
      true
    }
    
    def convertToQPList(prod: Product): Array[QueryParam[A]] = {
      var pos = 0
      val ret = new Array[QueryParam[A]](prod.arity)
      while (pos < prod.arity) {
        ret(pos) = prod.element(pos).asInstanceOf[QueryParam[A]]
        pos = pos + 1
      }
      ret
    }
    
    key match {
      case null => None
      case None => None
      case Some(n) => find(n)
      case qp: QueryParam[A] => find(List(qp.asInstanceOf[QueryParam[A]]) :_*)
      case prod: Product if (testProdArity(prod)) => find(convertToQPList(prod) :_*)
      // case s: Seq[Any] if (s.length > 0 && s(0).isInstanceOf[QueryParam[Any]]) => find(s.asInstanceOf[Seq[QueryParam[A]]])
      case v => find(v.toString)
    }
  }
  
  def find(key : String) : Option[A] = {
    
    DB.use { conn => 
    if (indexMap eq null) None
    else {
      val field = mappedColumnInfo(indexMap).asInstanceOf[MappedField[AnyRef,A] with IndexedField[AnyRef]]
      val convertedKey = field.convertKey(key)
      if (convertedKey eq None) None else
	{
          DB.prepareStatement("SELECT * FROM "+dbTableName+" WHERE "+indexMap+" = ?", conn) {
            st =>
              st.setObject(1, field.makeKeyJDBCFriendly(convertedKey.get), field.getTargetSQLType(indexMap))
            DB.exec(st) {
              rs =>
                val mi = buildMapper(rs)
	      if (rs.next) Some(createInstance(rs, mi._1, mi._2))
	      else None
            }
          }
	}
    }
  }
  }
  
  private def ??(meth : Method, inst :A) = {
    meth.invoke(inst, null).asInstanceOf[MappedField[Any, A]]
  }
  
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
    DB.use { conn =>
    _beforeSave(toSave)
    val ret = if (saved_?(toSave)) {
      if (!dirty_?(toSave)) true else {
        _beforeUpdate(toSave)
        val ret = DB.prepareStatement("UPDATE "+dbTableName+" SET "+whatToSet(toSave)+" WHERE "+indexMap+" = ?", conn) {
          st =>
            var colNum = 1
          
          for (val col <- mappedColumns.elements) {
            val colVal = ??(col._2, toSave)
            if (!columnPrimaryKey_?(col._1) && colVal.dirty_?) {
              st.setObject(colNum, colVal.getJDBCFriendly(col._1), colVal.getTargetSQLType(col._1))
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
        for (val col <- mappedColumns.elements) {
          if (!columnPrimaryKey_?(col._1)) {
            val colVal = col._2.invoke(toSave, null).asInstanceOf[MappedField[AnyRef, A]]
            st.setObject(colNum, colVal.getJDBCFriendly(col._1), colVal.getTargetSQLType(col._1))
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
                  case null => {}
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
  
  def createInstances(rs: ResultSet, start: Option[long], omax: Option[long]) : List[A] = {
    var ret = new ArrayBuffer[A]
    val bm = buildMapper(rs)
    var pos = (start getOrElse 0L) * -1L
    val max = omax getOrElse java.lang.Long.MAX_VALUE

    while (pos < max && rs.next()) {
      if (pos >= 0L) {
        ret += createInstance(rs, bm._1, bm._2)
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
    for (val pos <- 1 to colCnt) {
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

  def createInstance(rs : ResultSet, colCnt:int, mapFuncs: Array[(ResultSet,int,A) => unit]) : A = {
    val ret = createInstance
    val ra = ret// .asInstanceOf[Mapper[A]]
    var pos = 1
    while (pos <= colCnt) {
      mapFuncs(pos) match {
        case null => {}
        /*
         case f => try {
         f(rs, pos, ra)
         } catch {
         case e : java.lang.NullPointerException => Console.println("Failed with pos "+pos+" Retrying")
         f(rs, pos, ra)
         }
         */
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
    val look = (name.toLowerCase, if (clz != null) Some(clz) else None)
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
    if (accessor == null || accessor == None) {null} else {
      (accessor.get.invoke(this, null).asInstanceOf[MappedField[AnyRef, A]]).buildSetActualValue(accessor.get, inst, name)
    }
  }
  
  
  def checkFieldNames(in : A) : unit = {
    var pos = 0
    var len = mappedFieldArray.length
    while (pos < len) {
      val f = mappedFieldArray(pos)
      val field = ??(f._2, in);
      field match {
        case null => {}
        case _ if (field.i_name_! == null) => field.setName_!(f._1)
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
  
  {
    def canUse(meth: Method) = {
      meth.getName != "primaryKeyField"
    }
    this.runSafe {
      val tArray = new ArrayBuffer[(String, Method, MappedField[AnyRef,A])]
      for (val v <- this.getClass.getSuperclass.getMethods) {
        if (classOf[LifecycleCallbacks].isAssignableFrom(v.getReturnType) && v.getParameterTypes.length == 0 &&
            canUse(v)) {
              mappedCallbacks = (v.getName, v) :: mappedCallbacks
            }
	if (classOf[MappedField[AnyRef, A]].isAssignableFrom(v.getReturnType) && v.getParameterTypes.length == 0 &&
            canUse(v)) {
	      val mf = v.invoke(this, null).asInstanceOf[MappedField[AnyRef, A]];
	      if (mf != null && !mf.ignoreField) {
		mf.setName_!(v.getName)
		val trp = (mf.name, v, mf)
		  tArray += trp
		for (val colName <- mf.dbColumnNames(v.getName)) {
		  mappedColumnInfo(colName) = mf
		  mappedColumns(colName) = v
		}
		if (mf.dbPrimaryKey_?) {
		  indexMap = v.getName
		}
	      }
	    }
      }
      def findPos(in : AnyRef) : Option[int] = {
	tArray.elements.zipWithIndex.foreach {mft => if (in eq mft._1._3) return Some(mft._2)}
	None
      }
      
      val resArray = new ArrayBuffer[(String, Method, MappedField[AnyRef,A])];
      
      fieldOrder.foreach {
	f => 
	  findPos(f).foreach{pos => resArray += tArray.remove(pos)}
      }
      
      tArray.foreach {mft => resArray += mft}      
      
      mappedFieldArray = resArray.toArray
      mappedFieldArray.foreach {
        ae =>
          _mappedFields(ae._1) = ae._2
      }
    }
  }

  val columnNamesForInsert = {
    (mappedColumnInfo.elements.filter{c => !c._2.dbPrimaryKey_?}.map{p => p._1}).toList.mkString("", ",", "")
  }
  
  val columnQueriesForInsert = {
    (mappedColumnInfo.elements.filter{c => !c._2.dbPrimaryKey_?}.map{p => "?"}).toList.mkString("", ",", "")
  }
  
  private def fixTableName(name : String) = clean(name.toLowerCase)

  protected def internalTableName_$ = getClass.getSuperclass.getName.split("\\.").toList.last
  
  def htmlHeaders : NodeSeq = {
    mappedFieldArray.filter{mft => mft._3.dbDisplay_?}.map {mft => <th>{mft._3.displayName}</th>}.toList
    // mappedFieldInfo.elements.filter{e => e._2.db_display_?}. map {e => <th>{e._1}</th>}.toList
  }
  
  def mappedFields: Seq[BaseMappedField] = mappedFieldArray.map(f => f._3)
  
  def doHtmlLine(toLine : A) : NodeSeq = {
    mappedFieldArray.filter{mft => mft._3.dbDisplay_?}.map {mft => <td>{??(mft._2, toLine).asHtml}</td>}.toList
  }
  
  def asHtml(toLine : A) : NodeSeq = {
    Text(internalTableName_$) :: Text("={ ") :: 
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
    fixTableName(internalTableName_$)
  }
  
  private def eachField(what: A, toRun: List[(A) => unit])(f: (LifecycleCallbacks) => unit) {
    mappedCallbacks.foreach {
      e =>
        e._2.invoke(what, null) match {
          case lccb: LifecycleCallbacks => f(lccb)
          case _ =>
        }
	// f(e._2.invoke(what, null).asInstanceOf[LifecycleCallbacks])
    }
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
  
  // protected def getField(inst : Mapper[A], meth : Method) = meth.invoke(inst, null).asInstanceOf[MappedField[AnyRef,A]]
}

abstract class QueryParam[O<:Mapper[O]]
case class By[O<:Mapper[O], T](field: MappedField[T,O], value: T) extends QueryParam[O]
case class OrderBy[O<:Mapper[O], T](field: MappedField[T,O],ascending: boolean) extends QueryParam[O]
case class BySql[O<:Mapper[O]](query: String, params: Any*) extends QueryParam[O]
case class MaxRows[O<:Mapper[O]](max: long) extends QueryParam[O]
case class StartAt[O<:Mapper[O]](start: long) extends QueryParam[O]

trait KeyedMetaMapper[Type, A<:KeyedMapper[Type, A]] extends MetaMapper[A] with KeyedMapper[Type, A] {
  
}
