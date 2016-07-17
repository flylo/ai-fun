package com.ai.search.solvers

import com.ai.search.datastructures.{StateSpaceGraph, SearchState}
import scala.collection.mutable.ListBuffer

// TODO:
//  implement all search algorithms
//  write tests
//  fix names to be "minheuristicvalue" in hillclimbing
//  clean up the data structures we use so that the "child" case class isn't necessary
//  update the "getChildren" method to return the nodes and not just the node IDs

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
    def hillClimbingSearchRecursive(visited: List[SearchState[Int]],
                                    maxHeuristicValue: Int): List[SearchState[Int]] = {
      val nextNode = visited.head.children
        .find(stateSpaceGraph.getHeuristic(_) <= maxHeuristicValue)
      if (nextNode.isDefined) {
        val newVisited = stateSpaceGraph.getNode(nextNode.get) :: visited
        val newHeuristic = stateSpaceGraph.getHeuristic(nextNode.get)
        hillClimbingSearchRecursive(newVisited, newHeuristic)
      }
      else {
        visited
      }
    }
    val searchResults = hillClimbingSearchRecursive(startVisited, startMaxHeuristic)
      .map(_.data)
    indicesToPrintedSolution(searchResults)
  }

  case class child(nodeId: Int, heuristic: Int)

  def steepestAscentHillClimbingSearch(): Unit = {
    val startVisited = List(stateSpaceGraph.getStartNode)
    val startMaxHeuristic = startVisited.head.heuristic
    def hillClimbingSearchRecursive(visited: List[SearchState[Int]],
                                    maxHeuristicValue: Int): List[SearchState[Int]] = {
      val nextNode = visited.head.children
        .map { r =>
          child(r, stateSpaceGraph.getHeuristic(r))
        }
        .minBy(_.heuristic)
      if (nextNode.heuristic <= maxHeuristicValue) {
        val newVisited = stateSpaceGraph.getNode(nextNode.nodeId) :: visited
        val newHeuristic = stateSpaceGraph.getHeuristic(nextNode.nodeId)
        hillClimbingSearchRecursive(newVisited, newHeuristic)
      }
      else {
        visited
      }
    }
    val searchResults = hillClimbingSearchRecursive(startVisited, startMaxHeuristic)
      .map(_.data)
    indicesToPrintedSolution(searchResults)
  }

  def beamSearch(beamWidth: Int = 2): Unit = {
    // currently gets stuck in cycles and returns nothing
    // note that this measure can't backtrack to nodes on the open list
    val startVisited = List[SearchState[Int]]()
    val startOpen = List(stateSpaceGraph.getStartNode)

    def beamSearchRecursive(visited: List[SearchState[Int]],
                            open: List[SearchState[Int]]): List[SearchState[Int]] = {
      if (open.exists(_.goal equals true)) {
        open ++ visited
      }
      else {
        // get the children of each node in the open list
        val children = open.flatMap {
          _.children.map(stateSpaceGraph.getNode(_))
        }
        // create a new open list
        val newOpen = children
          .sortBy(_.heuristic)
          .take(beamWidth)
        // create a new visited list
        val newVisited = open ++ visited
        beamSearchRecursive(newVisited, newOpen)
      }
    }
    val searchResults = beamSearchRecursive(startVisited, startOpen)
      .map(_.data)
    indicesToPrintedSolution(searchResults)
  }

  def bestFirstSearch(beamWidth: Int = 2): Unit = {
    // currently gets stuck in cycles and returns nothing
    val startVisited = List[SearchState[Int]]()
    val startOpen = List(stateSpaceGraph.getStartNode)

    def bestFirstSearchRecursive(visited: List[SearchState[Int]],
                                 open: List[SearchState[Int]]): List[SearchState[Int]] = {
      if (open.exists(_.goal equals true)) {
        open ++ visited
      }
      else {
        val currentNode = open.head
        val newChildren = currentNode.children.map(stateSpaceGraph.getNode(_))
        // create a new open list
        val newOpen = (open.filter(_ != currentNode) ++ newChildren).sortBy(_.heuristic)
        // create a new visited list
        val newVisited = currentNode :: visited
        bestFirstSearchRecursive(newVisited, newOpen)
      }
    }
    val searchResults = bestFirstSearchRecursive(startVisited, startOpen)
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