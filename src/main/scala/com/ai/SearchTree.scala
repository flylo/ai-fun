package com.ai

import com.ai.SearchTreeNode

/**
  * Created by ZacP on 6/26/16.
  */
class SearchTree[T] {
  var nodes = collection.mutable.Map[T, SearchTreeNode[T]]()

  def addNode(obj: T, parent: Option[T]): Unit = {
    val newNode = new SearchTreeNode(obj)
    nodes += (obj -> newNode)
  }
}
