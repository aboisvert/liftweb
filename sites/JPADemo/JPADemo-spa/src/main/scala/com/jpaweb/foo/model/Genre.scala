package com.foo.jpaweb.model

object Genre extends Enumeration with Enumv {
  val Mystery = Value("Mystery", "Mystery")
  val SciFi = Value("SciFi", "SciFi")
  val Classic = Value("Classic", "Classic")
  val Childrens = Value("Childrens", "Childrens")
  val Horror = Value("Horror", "Horror")
  val Poetry = Value("Poetry", "Poetry")
  val unknown = Value("Unknown", "Unknown genre")
}

class GenreType extends EnumvType(Genre) {}
