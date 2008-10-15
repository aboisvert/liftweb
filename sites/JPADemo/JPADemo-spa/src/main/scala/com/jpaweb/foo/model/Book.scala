package com.foo.jpaweb.model

import java.util.Date

import javax.persistence._
import org.hibernate.annotations.Type


/**
 This class represents a book that we might want to read.
*/
@Entity
class Book {
  @Id
  @GeneratedValue(){val strategy = GenerationType.AUTO}
  var id : Long = _

  @Column{val unique = true, val nullable = false}
  var title : String = ""

  @Temporal(TemporalType.DATE)
  @Column{val nullable = true}
  var published : Date = new Date()

  @Type{val `type` = "com.foo.jpaweb.model.GenreType"}
  var genre : Genre.Value = Genre.unknown

  @ManyToOne
  var author : Author = _
}
