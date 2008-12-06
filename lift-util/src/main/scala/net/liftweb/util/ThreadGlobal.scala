package net.liftweb.util

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
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

class ThreadGlobal[T]
{
  private val threadLocal = new ThreadLocal[T]

  def value: T = threadLocal.get

  def set(v: T): ThreadGlobal[T] = {
    threadLocal.set(v)
    this
  }

  def doWith[R](x: T)(f : => R) : R = {
    val original = value
    try {
      threadLocal.set(x)
      f
    } finally {
      threadLocal.set(original)
    }
  }
}
