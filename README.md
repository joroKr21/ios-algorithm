# groupBy Algorithm [![Build Status](https://travis-ci.org/joroKr21/IoS-Algorithm.svg?branch=master)](https://travis-ci.org/joroKr21/IoS-Algorithm)
Welcome to the groupBy algorithm project.

This is a [Tabu Search](https://en.wikipedia.org/wiki/Tabu_search)-based algorithm for assigning students to group projects based on flexible criteria. It has an embedded [`spray-can`](http://spray.io/documentation/1.2.2/spray-can/) stateless service endpoint for solving assignment problems.

### Building, Testing and Running
The project is managed via [`sbt`](http://www.scala-sbt.org/), so you first need to have it installed on your system. Then just clone this repository and `cd` to the root directory.

* To build the project use the command `sbt compile`
* To run all tests use the command `sbt test`
* To run the service endpoint use the command `sbt run re-start [port]`

For more details please visit the [wiki](../../wiki).
