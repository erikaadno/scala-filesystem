package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(name: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.wd
    if(wd.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists!")
    } else if(name.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$name must not contain separators!")
    } else if(checkIllegal(name)) {
      state.setMessage(s"$name: illegal entry name!")
    } else {
      doCreateEntry(state, name)
    }
  }

  def checkIllegal(name: String): Boolean = name.contains(".")

  def updateStructure(currentDir: Directory, path: List[String], newEntry: DirEntry): Directory = {
    if(path.isEmpty) currentDir.addEntry(newEntry)
    else {
      println(path)
      println(currentDir.findEntry(path.head))
      val oldEntry  = currentDir.findEntry(path.head).asDirectory
      currentDir.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
    }
  }

  def doCreateEntry(state: State, name: String): State = {
    val wd = state.wd

    val allDirectoryPath = wd.getAllFoldersInPath
    val newEntry = createSpecificEntry(state) //Directory.empty(wd.path, name)
    val newRoot = updateStructure(state.root, allDirectoryPath, newEntry)
    val newWD = newRoot.findDescendant(allDirectoryPath)
    State(newRoot, newWD)
  }

  def createSpecificEntry(state: State): DirEntry

}
