package com.ai.search.solvers

import com.ai.search.datastructures.{StateSpaceGraph, SearchState}
import scala.collection.mutable.ListBuffer

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
    // nor does it filter nodes out of the closed list, it solely maintains
    // the separate lists so that it can remember the paths taken in
    // a memory-efficient manner
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
    // implemented to avoid nodes that are on the closed list
    val startVisited = List[SearchState[Int]]()
    val startOpen = List(stateSpaceGraph.getStartNode)

    def bestFirstSearchRecursive(visited: List[SearchState[Int]],
                                 open: List[SearchState[Int]]): List[SearchState[Int]] = {
      if (open.exists(_.goal equals true)) {
        open ++ visited
      }
      else {
        // the filterNot function removes any nodes that we've already visited and allows search to converge
        val currentNode = open.filterNot(visited.contains(_)).head
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

  case class queueNode(node: SearchState[Int], pathLength: Int)

  def branchAndBoundSearch(): Unit = {
    // note that at each iteration, this re-opens children that it has visited and
    // takes a very long time even for simple search spaces
    val startQueue = List(queueNode(stateSpaceGraph.getStartNode, 0))
    val startVisited = List[SearchState[Int]]()
    var optimalPathLengthToGoal = 10000 // set very large

    def branchAndBoundSearchRecursive(queue: List[queueNode],
                                      visited: List[SearchState[Int]]): List[SearchState[Int]] = {
      val currentNode = queue.head
      // check if that node is the goal
      if (currentNode.node.goal equals true) {
        currentNode.node :: visited
      }
      else if (queue.minBy(_.pathLength).pathLength > optimalPathLengthToGoal) {
        val goalNode = queue.filter(_.node.goal equals true).head.node
        goalNode :: visited
      }
      else {
        // for each child, add them to the queue with an increasing path length
        val currentChildren = currentNode.node.children
          .map {
            child =>
              queueNode(stateSpaceGraph.getNode(child), currentNode.pathLength + 1)
          }.toList
        // check the pathlength to the goal node
        if (queue.map(_.node.goal).contains(true)) {
          optimalPathLengthToGoal = queue.filter(_.node.goal equals true).head.pathLength
        }
        // add the open node to the list of visited nodes
        val newVisited = currentNode.node :: visited
        // remove the open node from the queue and sort by pathlength
        val newQueue = (currentChildren ++ queue)
          .diff(List(currentNode))
          .sortBy(_.pathLength)
        branchAndBoundSearchRecursive(newQueue, newVisited)
      }
    }
    val searchResults = branchAndBoundSearchRecursive(startQueue, startVisited)
      .map(_.data)
    indicesToPrintedSolution(searchResults)
  }

  def branchAndBoundWithDPSearch(): Unit = {
    // note that at each iteration, this re-opens children that it has visited and
    // takes a very long time even for simple search spaces
    val startQueue = List(queueNode(stateSpaceGraph.getStartNode, 0))
    val startVisited = List[SearchState[Int]]()
    var optimalPathLengthToGoal = 10000 // set very large

    def branchAndBoundWithDPSearchRecursive(queue: List[queueNode],
                                            visited: List[SearchState[Int]]): List[SearchState[Int]] = {
      val currentNode = queue.head
      // check if that node is the goal
      if (currentNode.node.goal equals true) {
        currentNode.node :: visited
      }
      else if (queue.minBy(_.pathLength).pathLength > optimalPathLengthToGoal) {
        val goalNode = queue.filter(_.node.goal equals true).head.node
        goalNode :: visited
      }
      else {
        // for each child, add them to the queue with an increasing path length
        val currentChildren = currentNode.node.children
          .map {
            child =>
              queueNode(stateSpaceGraph.getNode(child), currentNode.pathLength + 1)
          }.toList
          // this is the key difference: we remove nodes we've seen
          .filterNot(kid => visited.contains(kid.node))
        // check the pathlength to the goal node
        if (queue.map(_.node.goal).contains(true)) {
          optimalPathLengthToGoal = queue.filter(_.node.goal equals true).head.pathLength
        }
        // add the open node to the list of visited nodes
        val newVisited = currentNode.node :: visited
        // remove the open node from the queue and sort by pathlength
        val newQueue = (currentChildren ++ queue)
          .diff(List(currentNode))
          .sortBy(_.pathLength)
        branchAndBoundWithDPSearchRecursive(newQueue, newVisited)
      }
    }
    val searchResults = branchAndBoundWithDPSearchRecursive(startQueue, startVisited)
      .map(_.data)
    indicesToPrintedSolution(searchResults)
  }


  def branchAndBoundWithHeuristicsSearch(): Unit = {
    // note that at each iteration, this re-opens children that it has visited and
    // takes a very long time even for simple search spaces
    val startQueue = List(queueNode(stateSpaceGraph.getStartNode, 0))
    val startVisited = List[SearchState[Int]]()
    var optimalPathLengthToGoal = 10000 // set very large

    def branchAndBoundWithDPSearchRecursive(queue: List[queueNode],
                                            visited: List[SearchState[Int]]): List[SearchState[Int]] = {
      val currentNode = queue.head
      // check if that node is the goal
      if (currentNode.node.goal equals true) {
        currentNode.node :: visited
      }
      else if (queue.minBy(_.pathLength).pathLength > optimalPathLengthToGoal) {
        val goalNode = queue.filter(_.node.goal equals true).head.node
        goalNode :: visited
      }
      else {
        // for each child, add them to the queue with an increasing path length
        val currentChildren = currentNode.node.children
          .map {
            child =>
              queueNode(stateSpaceGraph.getNode(child), currentNode.pathLength + 1)
          }.toList
        // check the pathlength to the goal node
        if (queue.map(_.node.goal).contains(true)) {
          optimalPathLengthToGoal = queue.filter(_.node.goal equals true).head.pathLength
        }
        // add the open node to the list of visited nodes
        val newVisited = currentNode.node :: visited
        // remove the open node from the queue and sort by pathlength
        val newQueue = (currentChildren ++ queue)
          .diff(List(currentNode))
          // ADDED HEURISTIC BELOW vvv
          .sortBy(node => node.pathLength + node.node.heuristic)
        branchAndBoundWithDPSearchRecursive(newQueue, newVisited)
      }
    }
    val searchResults = branchAndBoundWithDPSearchRecursive(startQueue, startVisited)
      .map(_.data)
    indicesToPrintedSolution(searchResults)
  }

  def aStarSearch(): Unit = {
    // note that at each iteration, this re-opens children that it has visited and
    // takes a very long time even for simple search spaces
    val startQueue = List(queueNode(stateSpaceGraph.getStartNode, 0))
    val startVisited = List[SearchState[Int]]()
    var optimalPathLengthToGoal = 10000 // set very large

    def branchAndBoundWithDPSearchRecursive(queue: List[queueNode],
                                            visited: List[SearchState[Int]]): List[SearchState[Int]] = {
      val currentNode = queue.head
      // check if that node is the goal
      if (currentNode.node.goal equals true) {
        currentNode.node :: visited
      }
      else if (queue.minBy(_.pathLength).pathLength > optimalPathLengthToGoal) {
        val goalNode = queue.filter(_.node.goal equals true).head.node
        goalNode :: visited
      }
      else {
        // for each child, add them to the queue with an increasing path length
        val currentChildren = currentNode.node.children
          .map {
            child =>
              queueNode(stateSpaceGraph.getNode(child), currentNode.pathLength + 1)
          }.toList
          // this is the key difference: we remove nodes we've seen
          .filterNot(kid => visited.contains(kid.node))
        // check the pathlength to the goal node
        if (queue.map(_.node.goal).contains(true)) {
          optimalPathLengthToGoal = queue.filter(_.node.goal equals true).head.pathLength
        }
        // add the open node to the list of visited nodes
        val newVisited = currentNode.node :: visited
        // remove the open node from the queue and sort by pathlength
        val newQueue = (currentChildren ++ queue)
          .diff(List(currentNode))
          // ADDED HEURISTIC BELOW vvv
          .sortBy(node => node.pathLength + node.node.heuristic)
        branchAndBoundWithDPSearchRecursive(newQueue, newVisited)
      }
    }
    val searchResults = branchAndBoundWithDPSearchRecursive(startQueue, startVisited)
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

    println("\n\n\nPlease Enter Desired Search Method:\n")
    val userInput = scala.io.StdIn.readLine()

    if (userInput == "dfs") {
      println("\n\nDepth-First Search with Backtracking:\n")
      maze.depthFirstSearchWithBacktracking()
    }

    if (userInput == "hill-climbing") {
      println("\n\nHill-Climbing Search:\n")
      maze.hillClimbingSearch()
    }

    if (userInput == "steepest-ascent") {
      println("\n\nSteepest-Ascent Hill-Climbing Search:\n")
      maze.steepestAscentHillClimbingSearch()
    }

    if (userInput == "beam") {
      println("\n\nBeam Search:\n")
      maze.beamSearch()
    }

    if (userInput == "best-first") {
      println("\n\nBest-First Search:\n")
      maze.bestFirstSearch()
    }

    if (userInput == "b&b") {
      println("\n\nBranch And Bound Search:\n")
      maze.branchAndBoundSearch()
    }

    if (userInput == "b&b-dp") {
      println("\n\nBranch And Bound With Dynamic Programming:\n")
      maze.branchAndBoundWithDPSearch()
    }

    if (userInput == "b&b-heuristics") {
      println("\n\nBranch And Bound With Heuristics:\n")
      maze.branchAndBoundWithHeuristicsSearch()
    }

    if (userInput == "a*") {
      println("\n\nA-Star Search:\n")
      maze.aStarSearch()
    }

    println("\n\nFinished.")

  }

}
