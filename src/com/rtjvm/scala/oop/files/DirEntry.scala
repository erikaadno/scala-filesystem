package com.rtjvm.scala.oop.files

abstract class DirEntry(val parentPath: String, val name: String) {
  def path: String = {
    val separetorIfNecessary =
      if(Directory.ROOT_PATH.equals(parentPath)) ""
      else Directory.SEPARATOR
        s"$parentPath$separetorIfNecessary$name"
  }
  def asDirectory: Directory
  def asFile: File
  def isDirectory: Boolean
  def isFile: Boolean
  def getType: String
}
