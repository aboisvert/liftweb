package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.collection.mutable._
import java.lang.reflect.Method
import java.sql.{ResultSet, Types, PreparedStatement}
import scala.xml.{Elem, Node, Text}
import net.liftweb.util.Helpers._

trait MetaMapper[A] extends Mapper[A] {
  def findAll() : List[A] = {
    DB.use {db => 

      DB.exec(db, "SELECT * FROM "+tableName_$) { rs =>
	createInstances(rs)
					       }
	  }
  }
  
  def findAll(by: QueryParam): List[A] = findAll(List(by))
  
  def findBySql(query: String): List[A] = {
    DB.prepareStatement(query) {
      st =>
      DB.exec(st) {
        rs =>
        createInstances(rs)
      }
    }
  }
  
  def findAll(by: List[QueryParam]): List[A] = {
    
    val query = addFields("SELECT * FROM "+tableName_$+" WHERE TRUE ", by)
              DB.prepareStatement(query) {
            st =>
              setStatementFields(st, by, 1)
            DB.exec(st) {
              rs =>
                createInstances(rs)
            }
 
  }
  }
  
  def count : long = {
    DB.use {db =>
      DB.exec(db, "SELECT COUNT(*) FROM "+tableName_$) {
        rs =>
        if (rs.next) rs.getLong(1)
        else 0
      }
    }
  }
  
  private def addFields(what: String, by: List[QueryParam]): String = {
    by match {
      case Nil => what
      case x :: xs => {
        var updatedWhat = what        
        x match {
          case ByField(field, _) => 
        (1 to field.db_column_count).foreach {
          cn =>
          updatedWhat = updatedWhat + " AND "+field.db_column_names(field.name)(cn - 1)+" = ? "
        }
            case _ => {}
        }
        addFields(updatedWhat, xs)
        }
      }
    }
  
  private def setStatementFields(st: PreparedStatement, by: List[QueryParam], curPos: int) {
    by match {
      case Nil => {}
      case ByField(field, value) :: xs => {
        st.setObject(curPos, field.convertToJDBCFriendly(value), field.getTargetSQLType(field.db_column_names(field.name)(0)))
        setStatementFields(st, xs, curPos + 1)
      }
      case _ :: xs => {
        setStatementFields(st, xs, curPos)
      }
    }
  }
 
  def find(by: QueryParam): Option[A] = find(List(by))
  
  def find(by: List[QueryParam]): Option[A] = {
    
    val query = addFields("SELECT * FROM "+tableName_$+" WHERE TRUE ", by)
              DB.prepareStatement(query) {
            st =>
              setStatementFields(st, by, 1)
            DB.exec(st) {
              rs =>
                if (rs.next) createInstance(rs)
                else None
            }
 
  }
  }
  
  def delete_!(toDelete : Mapper[A]) : boolean = {

    DB.prepareStatement("DELETE FROM "+tableName_$ +" WHERE "+indexMap+" = ?") {
      st =>
	val indVal = indexedField(toDelete)
      st.setObject(1, indVal.getJDBCFriendly(indexMap), indVal.get.getTargetSQLType(indexMap))

      st.executeUpdate == 1
    }
  }
  
  def find(key: Any) : Option[A] = {
    key match {
      case null => None
      case Some(n) => find(n)
      case v => find(v.toString)
    }
  }
  
  def find(key : String) : Option[A] = {
    if (indexMap eq null) None
    else {
      val field = mappedColumnInfo(indexMap).asInstanceOf[MappedField[AnyRef,A] with IndexedField[AnyRef]]
      val convertedKey = field.convertKey(key)
      if (convertedKey eq None) None else
	{
          DB.prepareStatement("SELECT * FROM "+tableName_$+" WHERE "+indexMap+" = ?") {
            st =>
              st.setObject(1, field.makeKeyJDBCFriendly(convertedKey.get), field.getTargetSQLType(indexMap))
            DB.exec(st) {
              rs =>
		if (rs.next) createInstance(rs)
		else None
            }
          }
	}
    }
  }
  
  private def ??(meth : Method, inst : Mapper[A]) = {
    meth.invoke(inst, null).asInstanceOf[MappedField[AnyRef, A]]
  }
  
  def dirty_?(toTest : Mapper[A]) : boolean = {
    mappedFieldArray.foreach {
      mft =>      
      if (??(mft._2, toTest).dirty_?) return true
    }
    false
  }
  
  def indexedField(toSave : Mapper[A]) : Option[MappedField[AnyRef, A]] = {
    if (indexMap eq null) None else 
      Some(??(mappedColumns(indexMap), toSave))
  }
  
  
  def saved_?(toSave : Mapper[A]) : boolean = {
    if (indexMap eq null) true else {
      indexedField(toSave).db_index_field_indicates_saved_?
    }
  }
  
  def whatToSet(toSave : Mapper[A]) : String = {
    mappedColumns.elements.filter{c => ??(c._2, toSave).dirty_?}.map{c => c._1 + " = ?"}.toList.mkString("", ",", "")
  }
  
  def sws_validate(toValidate : Mapper[A]) : List[ValidationIssues[AnyRef, A]] = {
    var ret : List[ValidationIssues[AnyRef, A]] = Nil
    
    mappedFieldArray.foreach{f => ret = ret ::: ??(f._2, toValidate).sws_validate}
    
    ret
  }
  
  
  def save(toSave : Mapper[A]) : boolean = {
    if (saved_?(toSave)) {
      if (!dirty_?(toSave)) true else {
        DB.prepareStatement("UPDATE "+tableName_$+" SET "+whatToSet(toSave)+" WHERE "+indexMap+" = ?") {
          st =>
            var colNum = 1
          
          for (val col <- mappedColumns.elements) {
            val colVal = ??(col._2, toSave)
            if (!columnIndex_?(col._1) && colVal.dirty_?) {
              st.setObject(colNum, colVal.getJDBCFriendly(col._1), colVal.getTargetSQLType(col._1))
              colNum = colNum + 1
            }
          }
          
          val indVal = indexedField(toSave)
          st.setObject(colNum, indVal.get.getJDBCFriendly(indexMap), indVal.get.getTargetSQLType(indexMap))
          1 == st.executeUpdate
        }
      }
    } else {
      DB.prepareStatement("INSERT INTO "+tableName_$+" ("+columnNamesForInsert+") VALUES ("+columnQueriesForInsert+")") {
        st =>
          var colNum = 1
        for (val col <- mappedColumns.elements) {
          if (!columnIndex_?(col._1)) {
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
    }
  }
  
  def columnIndex_?(name : String) = {
    
    mappedColumnInfo.get(name) match {
      case None => false
      case v @ _ => v.db_index_?
    }
  }
  
  
  def createInstances(rs: ResultSet) : List[A] = {
    var ret  = new ArrayBuffer[A]
    while (rs.next()) {
      createInstance(rs) match {
        case None => {}
        case s @ Some(_) => {ret += s.get}
      }
    }
    ret.toList
  }
  
  def appendFieldToStrings(in : Mapper[A]) : String = {
    (mappedFieldArray.elements.map{p => ??(p._2, in).asString}).toList.mkString("", ",", "")
  }

  def createInstance(rs : ResultSet) : Option[A] = {
    val ret = createInstance
    val meta = rs.getMetaData
    ret.asInstanceOf[Mapper[A]].runSafe {
      var cnt = meta.getColumnCount
      while (cnt > 0) {
        val obj = rs.getObject(cnt)
        findApplier(meta.getColumnName(cnt), obj) match
        {
          case Some(ap) if (ap != null) => ap(ret.asInstanceOf[Mapper[A]], obj)
          case _ => 
        }
        cnt = cnt - 1
      }
    }
    Some(ret)
  }
  
  protected def  findApplier(name : String, inst : AnyRef) : Option[((Mapper[A], AnyRef) => unit)] = synchronized {
    val clz = inst match {
      case null => null
      case _ => inst.getClass
    }
    val look = Pair(name.toLowerCase, if (clz != null) Some(clz) else None)
    mappedAppliers.get(look) match {
      case s @ Some(_) => s
      case None => {
        val newFunc = createApplier(name, inst, clz)
        mappedAppliers(look) = newFunc
        Some(newFunc)
      }
    }
  }
  

  private def createApplier(name : String, inst : AnyRef, clz : Class) : (Mapper[A], AnyRef) => unit = {
    val accessor = mappedColumns.get(name)
    if (accessor == null || accessor == None) {null} else {
      (accessor.get.invoke(this, null).asInstanceOf[MappedField[AnyRef, A]]).buildSetActualValue(accessor.get, inst, name)
    }
  }
  
  
  
  def checkFieldNames(in : Mapper[A]) : unit = {
    mappedFieldArray.foreach {
      f =>
      val field = ??(f._2, in);
      field match {
        case null => {}
        case _ if (field.i_name_! == null) => field.setName_!(f._1)
      }
    }
  }
  
  
  def createInstance : A = {
    val ret = rootClass.newInstance.asInstanceOf[A];
    val mr = ret.asInstanceOf[Mapper[A]]
    mr.runSafe {
      checkFieldNames(mr)
    }
    
    ret
  }
  
  
  def sws_fieldOrder : List[AnyRef] = Nil
  
  protected val rootClass = this.getClass.getSuperclass
  
  private val mappedAppliers = new HashMap[Pair[String, Option[Class]], (Mapper[A], AnyRef) => unit];
  
  // private val mappedFields  = new HashMap[String, Method];
  private var mappedFieldArray : Array[Triple[String, Method, MappedField[AnyRef,A]]] = null; // new Array[Triple[String, Method, MappedField[Any,Any]]]();
  
  private val mappedColumns = new HashMap[String, Method];
  
  // private val mappedFieldInfo = new HashMap[String, MappedField[AnyRef, A]]
  private val mappedColumnInfo = new HashMap[String, MappedField[AnyRef, A]]  
  
                                             
                                             
  private var indexMap : String = null
  
  {
    
    this.runSafe {
  val tArray = new ArrayBuffer[Triple[String, Method, MappedField[AnyRef,A]]]
  for (val v <- this.getClass.getSuperclass.getMethods) {
    if (classOf[MappedField[AnyRef, A]].isAssignableFrom(v.getReturnType) && v.getParameterTypes.length == 0) {
      val mf = v.invoke(this, null).asInstanceOf[MappedField[AnyRef, A]];
      if (mf != null && !mf.ignoreField) {
        mf.setName_!(v.getName)
        val trp = Triple(mf.i_name_!, v, mf)
        tArray += trp
        for (val colName <- mf.db_column_names(v.getName)) {
          mappedColumnInfo(colName) = mf
          mappedColumns(colName) = v
        }
        if (mf.db_index_?) {
          indexMap = v.getName
        }
      }
    }
  }
  def findPos(in : AnyRef) : Option[int] = {
    tArray.elements.zipWithIndex.foreach {mft => if (in eq mft._1._3) return Some(mft._2)}
    None
  }
  
  val resArray = new ArrayBuffer[Triple[String, Method, MappedField[AnyRef,A]]];
  
  sws_fieldOrder.foreach {
    f => 
    findPos(f).foreach{pos => resArray += tArray.remove(pos)}
  }
                                 
  tArray.foreach {mft => resArray += mft}      
  
  mappedFieldArray = resArray.toArray
    }
  }

  val columnNamesForInsert = {
    (mappedColumnInfo.elements.filter{c => !c._2.db_index_?}.map{p => p._1}).toList.mkString("", ",", "")
  }
  
  val columnQueriesForInsert = {
    (mappedColumnInfo.elements.filter{c => !c._2.db_index_?}.map{p => "?"}).toList.mkString("", ",", "")
  }
  
  private def fixTableName(name : String) = clean(name.toLowerCase)

  protected def internalTableName_$ = getClass.getSuperclass.getName.split("\\.").toList.last
  
  def htmlHeaders : Seq[Node] = {
    mappedFieldArray.filter{mft => mft._3.db_display_?}.map {mft => <th>{mft._3.displayName}</th>}.toList
    // mappedFieldInfo.elements.filter{e => e._2.db_display_?}. map {e => <th>{e._1}</th>}.toList
  }
  
  def doHtmlLine(toLine : Mapper[A]) : Seq[Node] = {
    mappedFieldArray.filter{mft => mft._3.db_display_?}.map {mft => <td>{??(mft._2, toLine).asHtml}</td>}.toList
  }
  
  def asHtml(toLine : Mapper[A]) : Seq[Node] = {
    Text(internalTableName_$) :: Text("={ ") :: 
    mappedFieldArray.filter{mft => mft._3.db_display_?}.map {mft => 
      val field = ??(mft._2, toLine)
      <span>{field.displayName}={field.asHtml}&nbsp;</span>}.toList :::
    List(Text(" }"))
  }
  
  def generateInputTable(toMap : Mapper[A]) : Seq[Node] = {
    
    mappedFieldArray.filter{e => e._3.db_display_?}.map {
      e =>
      val field = ??(e._2, toMap)
    <tr>
    <td>{field.displayName}</td>
    <td>{field.i}</td>
    </tr>}.toList
  }

  val tableName_$ : String = {
    fixTableName(internalTableName_$)
  }
  
  
  
  // protected def getField(inst : Mapper[A], meth : Method) = meth.invoke(inst, null).asInstanceOf[MappedField[AnyRef,A]]
}

abstract class QueryParam
case class ByField[T <: Any, O](field: MappedField[T,O], value: T) extends QueryParam

