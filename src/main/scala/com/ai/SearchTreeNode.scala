package com.ai

import scala.collection.mutable.ListBuffer

/**
  * Created by ZacP on 6/26/16.
  */
class SearchTreeNode[T](obj: T) {
  val data = obj
  var children = ListBuffer[T]()

  def addChild(child: T): Unit = {
    children += child
  }
}
