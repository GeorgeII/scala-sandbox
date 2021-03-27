# The repo contains some of my programs that I've written to learn Scala and master my skills in it.

### Content of the repo:


## Exercises and Competitions

:heavy_check_mark: [Functional Programming Playground](https://github.com/GeorgeII/scala-sandbox/tree/master/functional-programming) - this is where I try to 
dive into the functional paradigm. Mostly consists of exercises for well-known books like "Scala with Cats", "Functional Programming in Scala" (aka the Red Book).

:heavy_check_mark: [Advent of Code](https://github.com/GeorgeII/scala-sandbox/tree/master/advent-of-code) - a yearly competition. 
Suits perfect if you want to get to know a new language.


## Concurrency and Multithreading

:heavy_check_mark: [Concurrent Matrix Multiplication](https://github.com/GeorgeII/scala-sandbox/tree/master/concurrent-matrix-multiplication) - the first time I 
tried to parallelize calculations with Futures. Despite the fact I used naïve blocking Await approach, it's still be significantly faster than one-threaded 
multiplication (for medium+ size matrices, ofc).

:heavy_check_mark: [Shakespeare Words Counter](https://github.com/GeorgeII/scala-sandbox/tree/master/shakespeare-words-counter) - another parallel program 
with Futures to count number of words in Hamlet. This time it was done much cleaner (only one blocking Await in the entire program) than the previous one.

:heavy_check_mark: [Dining Philosophers Problem](https://github.com/GeorgeII/scala-sandbox/tree/master/dining-philosophers-problem) - the first attempt to use 
Akka and actor model. Though, my solution of "Philosophers' problem" is kind of 'hacking'.

:heavy_check_mark: [Most Common Color Finder](https://github.com/GeorgeII/scala-sandbox/tree/master/most-common-color-finder) - the program reads pictures in the 
directory, gets the color that occurs the most frequently in every picture, and prints all colors as stripes to the final picture. The program is backed by Cats 
(my first touch with Cats) using multiple IOs and struggling with their nestings.


## Streams

:heavy_check_mark: [Grep Utility](https://github.com/GeorgeII/scala-sandbox/tree/master/grep-utility) - a simple couterpart of Linux 'grep' using Akka-Streams. 
It is able to process files bigger than your RAM.

:heavy_check_mark: [Sentiment Analysis Spark Streaming](https://github.com/GeorgeII/scala-sandbox/tree/master/sentiment-analysis-spark-streaming) - a 
Spark-Streaming application that reads text files and tries to predict their sentiment.

:heavy_check_mark: [Flink-Kafka-Zookeeper cluster deployment](https://github.com/GeorgeII/scala-sandbox/tree/master/flink-kafka-tweets-flow) - an attempt to 
deploy Kafka, Zookeeper, Flink, and a usual Scala app in containers via docker-compose.


## HTTP

:heavy_check_mark: [Rest Service for Todo List](https://github.com/GeorgeII/scala-sandbox/tree/master/REST-service-TODO-list) - Akka-http RESTful service with 
Get, Post, Delete methods. More details in the link.


## Other

:heavy_check_mark: [Short Sroty Parser](https://github.com/GeorgeII/scala-sandbox/tree/master/short-story-parser) - my first program written in Scala. 
It traverses and parses novels on the [Reedsy.com](https://blog.reedsy.com/creative-writing-prompts/contests/) website and saves them into MongoDB.

:heavy_check_mark: [Scala Collections](https://github.com/GeorgeII/scala-sandbox/tree/master/collections-implementation) - my implemetation of Scala basic 
collections and their main methods like 'append', 'prepand', 'insert', etc. Turns out, immutable collections are super non-trivial, thus I wrote only mutable ones.

<br />
<br />
<br />

Maybe the list will grow further, who knows...
