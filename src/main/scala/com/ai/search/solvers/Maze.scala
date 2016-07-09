package com.ai.search.solvers

import com.ai.search.datastructures.{StateSpaceGraph, SearchState}
import scala.collection.mutable.ListBuffer

class Maze(mazeFile: String) {
  private val maze2dArray = stringTo2dArray(mazeFile)
  private val mazeArray = arrayToIndices(maze2dArray)
  private val arrayLength = maze2dArray.head.length
  private val maxArrayVal = maze2dArray.length * arrayLength - 1
  val stateSpaceGraph = new StateSpaceGraph[Int]

  makeMazeGraph()

  /**
    * Method to read a ".maze" file and convert it to an array of arrays
    */
  private def stringTo2dArray(mazeFile: String): Array[Array[Char]] = {
    val mazeString = scala.io.Source.fromFile(mazeFile)
      .getLines
      .toArray

    println("Converting the following maze into a state-space graph:")
    mazeString.foreach(println)

    mazeString
      .map(line => line.toArray)
  }

  /**
    * Method to convert a 2d array of maze entries into a 1d array of key value pairs
    */
  private def arrayToIndices(array2d: Array[Array[Char]]): Map[Int, Char] = {
    array2d
      .zipWithIndex
      .flatMap {
        case (nodeRow, id) =>
          val rowLength = nodeRow.length
          val nodeIds = (0 to rowLength)
            .map(r => (rowLength - 1) * id + r).toArray
            .zip(nodeRow)
            .map {
              case (nodeId, nodeValue) =>
                nodeId -> nodeValue
            }
          nodeIds
      }
      .toMap
  }

  /**
    * Method to find the valid neighbors of a given node
    */
  private def getNeighbors(nodeId: Int): ListBuffer[Int] = {
    var outList = new ListBuffer[Int]()
    val right = nodeId + 1
    val left = nodeId - 1
    val above = nodeId - arrayLength
    val below = nodeId + arrayLength

    if (right % arrayLength != 0) {
      // has right
      outList += right
    }
    if (nodeId % arrayLength != 0) {
      // has left
      outList += left
    }
    if (above >= 0) {
      // has above
      outList += above
    }
    if (below <= maxArrayVal) {
      // has below
      outList += below
    }
    outList.flatMap {
      neighbor =>
        val mazeEntry = mazeArray.getOrElse(neighbor, "+")
        if (mazeEntry.toString == "+") None
        else Some(neighbor)
    }
  }

  /**
    * Method to create a new state object
    */
  private def createState(nodeId: Int, nodeValue: Char): Option[SearchState[Int]] = {
    if (nodeValue.toString == "S") {
      val node = new SearchState[Int](nodeId)
      node.start = true
      Some(node)
    }
    else if (nodeValue.toString == "G") {
      val node = new SearchState[Int](nodeId)
      node.goal = true
      Some(node)
    }
    else if (nodeValue.toString != "+") {
      val node = new SearchState[Int](nodeId)
      Some(node)
    } else {
      None
    }
  }

  /**
    * Method to build the state-space graph
    */
  private def makeMazeGraph(): Unit = {
    mazeArray
      .flatMap {
        case (id, nodeValue) =>
          createState(id, nodeValue)
      }
      .foreach {
        node =>
          node.children = getNeighbors(node.data)
          stateSpaceGraph.addNode(node.data, node)
      }
  }

}

object Maze {

  def main(args: Array[String]): Unit = {

    val maze = new Maze(args(0))

  }
}