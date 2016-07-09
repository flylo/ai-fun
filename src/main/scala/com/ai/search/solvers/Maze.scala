package com.ai.search.solvers

import com.ai.search.datastructures.SearchTreeNode

import scala.collection.mutable.ListBuffer

/**
  * Created by ZacP on 6/26/16.
  */

// TODO: finish makeMazeGraph

class mazeThing(mazeTwoDimensionalArray: Array[Array[Char]]) {
  val mazeArray = arrayToIndices(mazeTwoDimensionalArray)


  /**
    * Function to convert a 2d array of maze entries into a 1d array of <id, <"+","">>
    *
    * @param array2d
    * @return
    */
  def arrayToIndices(array2d: Array[Array[Char]]): Map[Int, Char] = {
    array2d
      .zipWithIndex
      .flatMap {
        case (nodeRow, id) =>

          val rowLength = nodeRow.length

          val nodeIds = (0 to rowLength)
            .map(r => (rowLength - 1) * id + r)
            .toArray

          nodeIds.zip(nodeRow)
            .map {
              case (nodeId, nodeValue) =>
                nodeId -> nodeValue
            }
      }
      .toMap
  }

  def getNeighbors(nodeId: Int, arrayLength: Int, maxArrayVal: Int): ListBuffer[Int] = {
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
    outList.flatMap{
      neighbor =>
        val mazeEntry = mazeArray.get(neighbor)
        if (mazeEntry.toString == "+") None
        else Some(neighbor)
    }
  }


  def makeMazeGraph(): Map[Int, Int] = {
    mazeArray
      .map {
        case (id, nodeValue) =>
          getNeighbors()
      }

    Map(1 -> 1)
  }

}

object Maze {



  def createState(nodeId: Int, nodeValue: Char): Option[SearchTreeNode[Int]] = {
    if (nodeValue.toString == "S") {
      val node = new SearchTreeNode[Int](nodeId)
      node.start = true
      Some(node)
    }
    else if (nodeValue.toString == "G") {
      val node = new SearchTreeNode[Int](nodeId)
      node.goal = true
      Some(node)
    }
    else if (nodeValue.toString != "+") {
      val node = new SearchTreeNode[Int](nodeId)
      Some(node)
    } else {
      None
    }
  }


  def main(args: Array[String]): Unit = {

    // move this logic out of main as well
    val mazeString = scala.io.Source.fromFile(args(0))
      .getLines
      .toArray
      .map(line => line.toArray)

    // store the arrayToIndices as a val so that we can re-create and print solutions

    mazeArray
      .foreach(println)

  }
}