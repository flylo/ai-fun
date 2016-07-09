package com.ai.search.datastructures

import scala.collection.mutable.ListBuffer

/**
  * Created by ZacP on 6/26/16.
  */
class SearchState[T](obj: T) {
  val data = obj
  var start: Boolean = false
  var goal: Boolean = false
  var children = ListBuffer[T]()

  def addChild(child: T): Unit = {
    children += child
  }
}

class StateSpaceGraph[T] {
  var nodes = collection.mutable.Map[T, SearchState[T]]()

  def addNode(obj: T, newNode: SearchState[T]): Unit = {
    nodes += (obj -> newNode)
  }
  def getNode(obj: T): SearchState[T] = {
    val gottenNode = nodes.getOrElse(obj, new SearchState[T](obj))
    gottenNode
  }
}