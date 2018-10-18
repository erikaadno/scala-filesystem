package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{DirEntry, Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {
  override def apply(state: State): State = {
    if(args.isEmpty) state
    else if(args.length == 1) state.setMessage(args.head)
    else {
      val operator = args(args.length - 2)
      val fileName: String  = args(args.length - 1)
      val contents = createContents(args, args.length - 2)
      if(">>".equals(operator))
        doEcho(state, contents, fileName, true)
      else if(">".equals(operator))
        doEcho(state, contents, fileName, false)
      else
        state.setMessage(createContents(args, args.length))
    }
  }

  def createContents(args: Array[String], topIndex: Int): String = {
    @tailrec
    def createContentsHelper(currentIndex: Int, accumulator: String): String = {
      if(currentIndex >= topIndex) accumulator
      else createContentsHelper(currentIndex + 1, s"$accumulator ${args(currentIndex)}")
    }
    createContentsHelper(0, "")
  }

  def getRootAfterEcho(currentDirectory: Directory, path: List[String], contents: String, append: Boolean): Directory = {
    /*
    if path is empty, then fail currentdDirectory
    else if no more things to explore =  path.tail.isEmpty
    findthe file to create /addcontent to
    if ifle not found, create file
    else if  the entry is actally a directory, the fail
    else
      replace or append content to the file
      replase the entry with the filenme ith the New file
    else


     */
    if(path.isEmpty) currentDirectory
    else if(path.tail.isEmpty) {
      val dirEntry: DirEntry = currentDirectory.findEntry(path.head)
      if(dirEntry == null)
        currentDirectory.addEntry(new File(currentDirectory.path, path.head, contents))
      else if(dirEntry.isDirectory) currentDirectory
      else
        if(append) currentDirectory.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
        else currentDirectory.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
    } else {
      val nextDirectory = currentDirectory.findEntry(path.head).asDirectory
      val newNextDirectory: Directory = getRootAfterEcho(nextDirectory, path.tail, contents, append)
      if(newNextDirectory == nextDirectory) currentDirectory
      else currentDirectory.replaceEntry(path.head, newNextDirectory)
    }
  }

  def doEcho(state: State, contents: String, fileName: String, append: Boolean): State = {
    if(fileName.contains(Directory.SEPARATOR))
      state.setMessage("Echo: filename must not contain separators")
    else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ fileName, contents, append)
      if(newRoot == state.root)
        state.setMessage(s"$fileName: no such file")
      else
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }
  }
}
