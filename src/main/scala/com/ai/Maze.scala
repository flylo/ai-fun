package com.ai

/**
  * Created by ZacP on 6/26/16.
  */


class Maze[T] {
  // TODO: fix type T -> is it Int?

//  def arrayColumnEdges(array2d: Array[Array[Char]]): collection.mutable.Map[Char, Char] = {
//    var outmap = collection.mutable.Map[Char, Char]
//
//    array2d
//      .flatMap(
//        entry =>
//          entry
//      )
////      .iterator
////      .zipWithIndex
////      .map{
////        case (array, rowId) =>
////          val idvec = array.map(entry => (rowId + (array.length-1)*rowId) + entry)
////          idvec.zip(array).map{
////
////          }
//////          idvec
////      }
//////      .map(_.iterator)
//
//
////    >>> for i, row in enumerate(a):
////    ...     print (i + (row.shape[0]-1)*i) + row
////      ...
////    [0 1 2]
////    [3 4 5]
////    [6 7 8]
//  }
//  def makeMazeGraph(maze: Array[Array[Char]]): Map[Int, Int] = {
    // give each element of the array a unique id that becomes the index of the map

    // iterate through the rows to find edges

    // iterate through the columns to find edges
//  }

  def main(): Unit = {
    val mazestring = scala.io.Source.fromFile("maze.txt")
      .getLines
      .toArray
      .foreach(println)
//      .map(line => line.toArray)

//    arrayColumnEdges(mazestring)

//    var maze = new MazeGraph[Int]
//    maze.graph = Map(1 -> List(2), 2-> List(1, 3), 3 -> List(2, 4, 5),
//      4 -> List(3, 6), 5 -> List(3, 7), 6 -> List(4), 7 -> List(3))
  }
}