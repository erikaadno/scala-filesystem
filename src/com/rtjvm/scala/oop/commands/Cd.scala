package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec
import scala.reflect.io.Path

class Cd(dir: String) extends Command {
  override def apply(state: State): State = {
    // find root
    val root = state.root
    val wd = state.wd

    // find the absolute path of the directory I want to cd to
    val absolutePath =
      if(dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir

    // find the directory to cd to, given the path
    val destinationDirectory = doFindEntry(root, absolutePath)

    // chenge the state given the new directory && che
    if(destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(s"$dir: no such directory")
    else
      State(root, destinationDirectory.asDirectory)

  }


  def doFindEntry(root: Directory, path: String): Directory = {
    @tailrec
    def findEntryHelper(currentDirectory: Directory, path: List[String]): DirEntry = {
      if(path.isEmpty || path.head.isEmpty) currentDirectory
      else if(path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if(nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      if(path.isEmpty) result
      else if(".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if("..".equals(path.head)) {
        if(result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      } else collapseRelativeTokens(path.tail, result :+ path.head)
    }

    // token
    val tokens: List[String] = path.substring(1).split(Directory.SEPARATOR).toList
    // eliminate relative tokens
    val newTokens: List[String] = collapseRelativeTokens(tokens, Nil)
    if(newTokens == null) null
    else findEntryHelper(root, tokens).asDirectory
  }

}
