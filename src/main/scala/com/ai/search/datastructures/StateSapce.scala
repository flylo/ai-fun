package com.ai.search.datastructures

import scala.collection.mutable.ListBuffer

class SearchState[T](obj: T) {
  val data = obj
  var start: Boolean = false
  var goal: Boolean = false
  var heuristic: Int = 0
  var children = ListBuffer[T]()

  def addChild(child: T): Unit = {
    children += child
  }
  def setHeuristic(value: Int): Unit = {
    heuristic = value
  }
}

class StateSpaceGraph[T] {
  var nodes = collection.mutable.Map[T, SearchState[T]]()
  private var start = new ListBuffer[T]
  private var goal = new ListBuffer[T]

  def addNode(obj: T, newNode: SearchState[T]): Unit = {
    nodes += (obj -> newNode)
    if (newNode.start.equals(true)) {
      start += newNode.data
    }
    else if (newNode.goal.equals(true)) {
      goal += newNode.data
    }
  }
  def getNode(obj: T): SearchState[T] = {
    val gottenNode = nodes.getOrElse(obj, new SearchState[T](obj))
    gottenNode
  }
  def getStartNode: SearchState[T] = {
    val gottenNode = nodes.getOrElse(start.head, new SearchState[T](start.head))
    gottenNode
  }
  def getGoalNode: SearchState[T] = {
    val gottenNode = nodes.getOrElse(goal.head, new SearchState[T](goal.head))
    gottenNode
  }
}