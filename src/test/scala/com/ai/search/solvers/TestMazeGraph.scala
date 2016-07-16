package com.ai.search.solvers

import org.scalatest._

import scala.collection.mutable.ListBuffer

class TestMazeGraph extends FlatSpec with Matchers {
  /*
      +++++++
      G  ++++
      ++   ++
      ++    +
      ++  + +
      +++++S+
   */
  val newString = "maze2.maze"
  val mazeGraph = new MazeGraph(newString)
  "String to Array Method" should "Provide valid 2d array" in {
    val result = mazeGraph.maze2dArray

    result(0)(0) should equal("+")
    result(1)(0) should equal("G")
    result(2)(3) should equal(" ")
    result.length should equal(6)
    result.head.length should equal(7)
  }
  "Node Indexing Method" should "Provide correct indices" in {
    val result = mazeGraph.mazeArray

    result.get(5).get.stringData should equal("+")
    result.get(30).get.stringData should equal(" ")
    result.get(25).get.stringData should equal(" ")
    result.get(7).get.stringData should equal("G")
  }
  "Node Neighbors Method" should "Provide correct neighbors/children" in {
    val result = mazeGraph

    result.getNeighbors(9).toSet should equal(Set(8, 16))
    result.getNeighbors(24).toSet should equal(Set(23, 25, 31, 17))
  }
  "Manhattan Distance Method" should "Provide correct heuristic estimates of distance" in {
    val result = mazeGraph.stateSpaceGraph

    result.getNode(8).heuristic should equal(1)
    result.getNode(30).heuristic should equal(5)
    result.getNode(33).heuristic should equal(8)
    result.getNode(40).heuristic should equal(9)
  }
  "Maze Object" should "Generate the true state space" in {
    val result = mazeGraph.stateSpaceGraph.nodes.keySet

    result.size should equal(14)  // only counts spaces that can be visited
    result.max should equal(40)  // the start node in this case
    result.min should equal(7)  //

  }
  it should "Know where the start and end of the maze are" in {
    val result = mazeGraph.stateSpaceGraph
    val startResult = result.getStartNode
    val goalResult = result.getGoalNode

    startResult.start should equal(true)
    startResult.goal should equal(false)
    startResult.heuristic should not equal(0)

    goalResult.goal should equal(true)
    goalResult.start should equal(false)
    goalResult.heuristic should equal(0)

  }
  it should "Provide the expected data in each node" in {
    val result = mazeGraph.stateSpaceGraph

    result.getNode(0).data should equal(0)
    result.getNode(6).data should equal(6)
    result.getNode(41).data should equal(41)
  }
  it should "Provide the correct children" in {
    val result = mazeGraph.stateSpaceGraph

    result.getStartNode.children should equal(ListBuffer(33))
    result.getGoalNode.children should equal(ListBuffer(8))
    result.getNode(16).children.toSet should equal(Set(17, 9, 23))
  }
}
