package com.ai.search.solvers

import com.ai.search.datastructures.{StateSpaceGraph, SearchState}
import scala.collection.mutable.ListBuffer

// TODO:
//  implement all search algorithms
//  write tests

class Maze(override val mazeFile: String) extends MazeGraph(mazeFile) {

  def depthFirstSearchWithBacktracking(): Unit = {
    // due the usage of foldLeft, this will return a StackOverflowError on deep trees
    val startValue = stateSpaceGraph.getStartNode
    def depthFirstRecursiveSearch(node: SearchState[Int],
                                  visited: List[SearchState[Int]]): List[SearchState[Int]] = {
      if (node.goal equals true) {
        node :: visited
      }
      else {
        // filtering to remove nodes we've been to from the stack constitutes backtracking here
        val neighbors = node.children.map(stateSpaceGraph.getNode) filterNot visited.contains
        neighbors.foldLeft(node :: visited)((b, a) => depthFirstRecursiveSearch(a, b))
      }
    }
    val searchResults = depthFirstRecursiveSearch(startValue, List())
      .map(_.data)
    indicesToPrintedSolution(searchResults)
  }


  def hillClimbingSearch(): Unit = {
    val startVisited = List(stateSpaceGraph.getStartNode)
    val startMaxHeuristic = startVisited.head.heuristic
    def hillClimbingRecursiveSearch(visited: List[SearchState[Int]],
                                    maxHeuristicValue: Int): List[SearchState[Int]] = {
      val nextNode = visited.head.children
        .find(stateSpaceGraph.getHeuristic(_) <= maxHeuristicValue)
      if (nextNode.isDefined) {
        val newVisited = stateSpaceGraph.getNode(nextNode.get) :: visited
        val newHeuristic = stateSpaceGraph.getHeuristic(nextNode.get)
        hillClimbingRecursiveSearch(newVisited, newHeuristic)
      }
      else {
        visited
      }
    }
    val searchResults = hillClimbingRecursiveSearch(startVisited, startMaxHeuristic)
      .map(_.data)
    indicesToPrintedSolution(searchResults)
  }


  private def indicesToPrintedSolution(solution: List[Int]): Unit = {
    solutionMazeArray = collection.mutable.Map(mazeArray.mapValues(v => v.stringData).toSeq: _*)
    solution foreach (solutionMazeArray.update(_, "*"))
    val outArray = Array.ofDim[String](numRows, numCols)
    val rowColIds = for (row <- 0 until numRows; col <- 0 until numCols) yield (row, col)
    (0 to maxArrayVal zip rowColIds)
      .foreach {
        case (id, (row, col)) =>
          outArray(row)(col) = solutionMazeArray.getOrElse(id, "+")
      }
    outArray
      .map(_.foldLeft("")((a, b) => a + b))
      .foreach(println)
  }

}

object Maze {

  def main(args: Array[String]): Unit = {

    val maze = new Maze(args(0))
    maze.depthFirstSearchWithBacktracking()

  }
}