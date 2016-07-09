package com.ai.search.datastructures

import scala.collection.mutable.ListBuffer

/**
  * Created by ZacP on 6/26/16.
  */
class SearchTreeNode[T](obj: T) {
  val data = obj
  var start: Boolean = false
  var goal: Boolean = false
  var children = ListBuffer[T]()

  def addChild(child: T): Unit = {
    children += child
  }
}

class SearchTree[T] {
  var nodes = collection.mutable.Map[T, SearchTreeNode[T]]()

  def addNode(obj: T, parent: Option[T]): Unit = {
    val newNode = new SearchTreeNode(obj)
    nodes += (obj -> newNode)
  }
}