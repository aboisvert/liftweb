package net.liftweb.proto

import scala.xml.{Node, NodeSeq, Text}
import net.liftweb.http._
import S._
import net.liftweb.mapper.ValidationIssue
import net.liftweb.util.{Can}

/**
   * The base (not Typed) trait that defines a field that is mapped to a column or more than 1 column
   * (e.g., MappedPassword) in the database
   */
 trait SimpleProtoBase {
   /**
     * Validate this field and return a list of Validation Issues
     */
   def validate: List[ValidationIssue]
   
   /**
     * The human name of this field
     */
   def name: String
   
   /**
     * Convert the field to its name/value pair (e.g., name=David)
     */
   def asString: String
   
   /**
     * The number of database columns that this field represents
     */  
   def dbColumnCount: Int
   
   
   def dbColumnNames(in: String): List[String]
   
   def dbColumnName: String
   
   /**
     * Create an input field for the item
     */
   def toForm: NodeSeq   
   
   def asHtml: Node  
   
   /**
      * The display name of this field (e.g., "First Name")
      */
    def displayName: String  
 }

trait ProtoOwner[MyType <: ProtoOwner[MyType]] { 
  def safe_? : Boolean;
  
  def getSingleton : MetaProtoOwner[MyType, _]
      
}

trait MetaProtoOwner[BaseType <: ProtoOwner[BaseType],MyType <: MetaProtoOwner[BaseType, MyType] ] extends ProtoOwner[BaseType] { self: MyType => 
  def getActualField[T, N, FT <: OwnedProtoBase[T, BaseType, N]](actual: ProtoOwner[BaseType],field: FT): FT
   
}

trait OwnedProtoBase[FieldType <: Any, OwnerType <: ProtoOwner[OwnerType], MyType <: OwnedProtoBase[FieldType, OwnerType, MyType]] extends ReadOnlyProtoBase[FieldType] {self: MyType => 

  /**
    * Get the field that this prototypical field represents
    *
    * @param actual the object to find the field on
    */
  def actualField(actual: OwnerType): MyType = actual.getSingleton.getActualField[FieldType, MyType, MyType](actual, this)

  /**
    * Return the owner of this field
    */
  def fieldOwner: OwnerType  
  
  /**
     * Are we in "safe" mode (i.e., the value of the field can be read or written without any security checks.)
     */
   final def safe_? : Boolean = fieldOwner.safe_?
   

}


trait ReadWriteProtoBase[FieldType <: Any, MyType <: ReadOnlyProtoBase[FieldType] with ReadWriteProtoBase[FieldType, MyType]] { self: MyType =>

type SetReturnType
/**
   * Is the field dirty
   */
 private var _dirty_? = false
     
     /**
        * Is the field dirty (has it been changed since the record was loaded from the database
        */
      def dirty_? = _dirty_?
          

          /**
            * Make the field dirty
            */
          protected def dirty_?(b: Boolean) = _dirty_? = b
              
              /**
                 * Given the current execution state, can the field be written?
                 */
               def writePermission_? = false       
                   
                   def apply(v: FieldType): SetReturnType
                             
                   
                   def resetDirty {
                     if (safe_?) dirty_?(false)
                   }

        
        /**
          * Set the field to the value
          */
        def set(value: FieldType): FieldType = {
          if (safe_? || writePermission_?) i_set_!(value)
          else throw new Exception("Do not have permissions to set this field")
        }
        
        /**
           * Set the field to the Can value if the Can is Full
           */
         def set_?(value: Can[FieldType]): Can[FieldType] = {
           value.foreach(v => this.set(v))
           value
         }
        
        
        /**
         * Assignment from the underlying type.  It's ugly, but:<br />
         * field() = new_value <br />
         * field := new_value <br />
         * field set new_value <br />
         * field.set(new_value) <br />
         * are all the same
         */
        def update(v: FieldType) {
          this.set(v)
        }
}

/**
   * The strongly typed field that's mapped to a column (or many columns) in the database.
   * FieldType is the type of the field and OwnerType is the Owner of the field 
   */
 trait ReadOnlyProtoBase[FieldType <: Any] extends SimpleProtoBase {
   def initialValue: FieldType
   
   /**
     * The default value for the field
     */
   def defaultValue: FieldType
   

       
   /**
     * Given the current execution state, can the field be read?
     */
   def readPermission_? = false
  


    def safe_? : Boolean  
    
   private var _name : String = null
   
   /**
     * The internal name of this field.  Use name
     */
   private[proto] final def i_name_! = _name
   
   /**
     * The name of this field
     */
       /*
   final def name = synchronized {
     if (_name eq null) {
       fieldOwner.checkNames
     }
     _name
   }*/
     
     def name: String
   
   /**
     * Set the name of this field
     */
   private[proto] final def setName_!(newName : String) : String = {
     if(safe_?) _name = newName.toLowerCase
     _name
   }
   
   /**
     * The display name of this field (e.g., "First Name")
     */
   override def displayName: String = name

   
   def dbDisplay_? = true
   
   /**
    * pascal-style assignment for syntactic sugar
    */
   /*
   def ::=(v : Any) : T
   */
     
   /**
     *  Attempt to figure out what the incoming value is and set the field to that value.  Return true if
     * the value could be assigned
     */
   def setFromAny(value: Any): FieldType
      
     
   /**
    * Create an input field for the item
    */
   override def toForm: NodeSeq = <input type='text' name={S.mapFunc({s: List[String] => this.setFromAny(s)})} value={is match {case null => "" case s => s.toString}}/>



   
   /**
     * A list of functions that transform the value before it is set.  The transformations
     * are also applied before the value is used in a query.  Typical applications
     * of this are trimming and/or toLowerCase-ing strings
     */
   protected def setFilter: List[FieldType => FieldType] = Nil
   
   protected final def i_set_!(value: FieldType): FieldType = {
     real_i_set_!(runFilters(value, setFilter))
   }
   
   def runFilters(in: FieldType, filter: List[FieldType => FieldType]): FieldType =
     filter match {
       case Nil => in
       case x :: xs => runFilters(x(in), xs)
     }
   
   /**
     * Must be implemented to store the value of the field
     */
   protected def real_i_set_!(value: FieldType): FieldType

   /*
   protected def getField(inst: OwnerType, meth: Method) = meth.invoke(inst, null).asInstanceOf[MappedField[FieldType,OwnerType]];
   protected def doField(inst: OwnerType, meth: Method, func: PartialFunction[MappedField[FieldType, OwnerType], Unit]) {
     val f = getField(inst, meth)
     if (func.isDefinedAt(f)) func(f)
   }
   */
     
   /**
     * Convert the field to its "context free" type (e.g., String, Int, Long, etc.)
     * If there are no read permissions, the value will be obscured
     */
   def is: FieldType = {
     if (safe_? || readPermission_?) i_is_!
     else i_obscure_!(i_is_!)
   }
   
   /**
     * What value was the field's value when it was pulled from the DB?
     */
   def was: FieldType = {
     if (safe_? || readPermission_?) i_was_!
     else i_obscure_!(i_was_!)
   }
   
   /**
     * The actual value of the field
     */
   protected def i_is_! : FieldType
   
   /**
     * The value of the field when it was pulled from the DB
     */
   protected def i_was_! : FieldType
   
   /**
     * Obscure the incoming value to a "safe" value (e.g., if there are
     * not enough rights to view the entire social security number 123-45-5678, this
     * method might return ***-**-*678
     */
   protected def i_obscure_!(in : FieldType): FieldType
   
   /**
     * Return the field name and field value, delimited by an '='
     */
   def asString = displayName + "=" + toString
   
   def dbColumnCount = 1
   
   def dbColumnNames(in : String) = if (dbColumnCount == 1) List(dbColumnName) else List(in.toLowerCase)
   
   def dbColumnName = name.toLowerCase match {
     case name if net.liftweb.mapper.DB.reservedWords.contains(name) => name+"_c"
     case name => name
   }
     

   def dbIndexed_? : Boolean = false

   def dbPrimaryKey_? : Boolean = false
       
       /**
          * Is the field a foreign key reference
          */
        def dbForeignKey_? : Boolean = false

   def jdbcFriendly(field : String) : Object
   
   def jdbcFriendly: Object = jdbcFriendly(dbColumnName)

   /**
    * Get the JDBC SQL Type for this field
    */
   def targetSQLType(field : String): Int = targetSQLType

   /**
    * Get the JDBC SQL Type for this field
    */
   def targetSQLType: Int
   
   override def toString : String = 
     is match {
       case null => ""
       case v => v.toString
     }
   
   def validations: List[FieldType => List[ValidationIssue]] = Nil
   
   def validate : List[ValidationIssue] = {
     val cv = is
     validations.flatMap(_(cv))
   }
   
   def realClass: Class

   final def convertToJDBCFriendly(value: FieldType): Object = real_convertToJDBCFriendly(runFilters(value, setFilter))

   protected def real_convertToJDBCFriendly(value: FieldType): Object
   
   /**
     * Does the "right thing" comparing mapped fields 
     */
   override def equals(other: Any): Boolean = {
     other match {
       case mapped: ReadOnlyProtoBase[Any] => this.is == mapped.is
       case ov: AnyRef if (ov ne null) && realClass.isAssignableFrom(ov.getClass) => this.is == runFilters(ov.asInstanceOf[FieldType], setFilter)
       case ov => this.is == ov
     }
   }

   override def asHtml : Node = Text(toString)
 }

class ProtoBase {

}
