package net.liftweb.util

/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

trait NamedPartialFunction[-A, +B] extends PartialFunction[A, B] {
  def functionName: String
}

class NamedPF[-A, +B](name: String, f: PartialFunction[A, B]) extends
NamedPartialFunction[A, B] {
  override def isDefinedAt(x: A): Boolean = f.isDefinedAt(x)
  override def apply(x: A): B = f(x)
  def functionName = name
}

object NamedPF {
  def apply[A, B](name: String)(f: PartialFunction[A,B]):
  NamedPartialFunction[A,B] = new NamedPF(name, f)

  def isDefinedAt[A](in: A, lst: Seq[PartialFunction[A, _]]): Boolean =
  lst.find(pf => pf.isDefinedAt(in)).isDefined

  def apply[A, B](param: A, lst: Seq[PartialFunction[A, B]]): B =
  lst.find(pf => pf.isDefinedAt(param)) match {
    case Some(pf) => pf.apply(param)
    case None => throw new MatchError(param)
  }

  def applyCan[A, B](param: A, lst: Seq[PartialFunction[A, B]]): Can[B] =
  lst.find(pf => pf.isDefinedAt(param)) match {
    case Some(pf) => Full(pf.apply(param))
    case None => Empty
  }
}
