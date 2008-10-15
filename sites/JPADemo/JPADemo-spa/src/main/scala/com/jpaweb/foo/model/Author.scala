package com.foo.jpaweb.model

import javax.persistence._

/**
  An author is someone who writes books.
*/
@Entity
class Author {
  @Id
  @GeneratedValue(){val strategy = GenerationType.AUTO}
  var id : Long = _

  @Column{val unique = true, val nullable = false}
  var name : String = ""

  @OneToMany(){val mappedBy = "author", val targetEntity = classOf[Book]}
  var books : java.util.Set[Book] = new java.util.HashSet[Book]()
}
