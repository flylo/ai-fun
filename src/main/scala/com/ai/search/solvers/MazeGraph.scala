package com.ai.search.solvers

import com.ai.search.datastructures.{SearchState, StateSpaceGraph}

import scala.collection.mutable.ListBuffer

/**
  * Created by ZacP on 7/15/16.
  */
class MazeGraph(val mazeFile: String) {
  val maze2dArray = stringTo2dArray(mazeFile)
  val mazeArray = arrayToIndices(maze2dArray)
  var solutionMazeArray = collection.mutable.Map[Int, String]()
  val arrayLength = maze2dArray.head.length
  val numCols = arrayLength
  val numRows = maze2dArray.length
  val maxArrayVal = numRows * numCols - 1
  val stateSpaceGraph = new StateSpaceGraph[Int]

  makeMazeGraph()
  setManhattanDistanceHeuristic()

  /**
    * Method to read a ".maze" file and convert it to an array of arrays
    */
  def stringTo2dArray(mazeFile: String): Array[Array[String]] = {
    val mazeString = scala.io.Source.fromFile(mazeFile)
      .getLines
      .toArray

    println("Converting the following maze into a state-space graph:")
    mazeString.foreach(println)

    mazeString
      .map(line => line.toArray
        .map(character => character.toString))
  }

  case class coordinate(row: Int, col: Int)
  case class NodeData(id: Int, stringData: String, coordinates: coordinate)
  /**
    * Method to convert a 2d array of maze entries into a 1d array of key value pairs
    */
  def arrayToIndices(array2d: Array[Array[String]]): Map[Int, NodeData] = {
    array2d
      .zipWithIndex
      .flatMap {
        case (nodeRow, id) =>
          val rowLength = nodeRow.length
          val nodeIds = (0 until rowLength)
            .map(r => NodeData(rowLength * id + r, "", coordinate(id, r)))
            .toArray
            .zip(nodeRow)
            .map {
              case (nodeData, nodeValue) =>
                val newNodeData = NodeData(nodeData.id, nodeValue, nodeData.coordinates)
                nodeData.id -> newNodeData
            }
          nodeIds
      }
      .toMap
  }

  /**
    * Method to find the valid neighbors of a given node
    */
  def getNeighbors(nodeId: Int): ListBuffer[Int] = {
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
        val mazeEntry = mazeArray.getOrElse(neighbor, new NodeData(-1, "+", coordinate(-1,-1)))
        if (mazeEntry.stringData == "+") None
        else Some(neighbor)
    }
  }

  /**
    * Method to create a new state object
    */
  def createState(nodeId: Int, nodeValue: String): Option[SearchState[Int]] = {
    if (nodeValue == "S") {
      val node = new SearchState[Int](nodeId)
      node.start = true
      Some(node)
    }
    else if (nodeValue == "G") {
      val node = new SearchState[Int](nodeId)
      node.goal = true
      Some(node)
    }
    else if (nodeValue != "+") {
      val node = new SearchState[Int](nodeId)
      Some(node)
    } else {
      None
    }
  }

  /**
    * Method to build the state-space graph
    */
  def makeMazeGraph(): Unit = {
    mazeArray
      .flatMap {
        case (id, nodeValue) =>
          createState(id, nodeValue.stringData)
      }
      .foreach {
        node =>
          node.children = getNeighbors(node.data)
          stateSpaceGraph.addNode(node.data, node)
      }
  }

  /**
    * Compute manhattan distance to goal node for each node
    */
  def setManhattanDistanceHeuristic(): Unit = {
    val goalCoordinates = mazeArray.get(stateSpaceGraph.getGoalNode.data).get.coordinates
    stateSpaceGraph.nodes.keySet
      .foreach {
        k =>
          val nodeCoordinates = mazeArray.get(k).get.coordinates
          val manhattanDistance = (goalCoordinates.row - nodeCoordinates.row).abs +
            (goalCoordinates.col - nodeCoordinates.col).abs
          stateSpaceGraph
            .getNode(k)
            .setHeuristic(manhattanDistance)
      }
  }

}
