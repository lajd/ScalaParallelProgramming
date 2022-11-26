# ScalaParallelProgramming

## Overview

The ScalaParallelProgramming course is taught by Prof. Viktor Kuncak and Dr. Aleksandar Prokopec, with learning outcomes:

- Reason about task and data parallel programs
- Express common algorithms in a functional style and solve them in parallel
- Competently microbenchmark parallel code
- Write programs that effectively use parallel collections to achieve performance

For details, see the course home page: https://www.coursera.org/learn/scala-parallel-programming/home/info.

This is course 3 of 5 in the Functional Programming in Scala Specialization (see here: https://www.coursera.org/specializations/scala).

## Requirements
SBT
Scala >= 3

## Course Contents

#### Week 1: Parallel Programming
- Parallelism on the JVM
- Parallel computations
- Monte Carlo estimates for Pi
- Assignment: Parallel Box Blur

#### Week 2: Task parallelism
- Parallel sorting
- Data operations and parallel mapping
- Parallel fold (reduce)
- Associativity in parallelism
- Parallel scan (prefix sum)
- Assignment: Reductions and prefix sums

#### Week 3: Data parallelism
- Data-parallel programming
- Data-parallel operations
- Scala parallel collections
- Splitters and combiners
- Assignment: Parallel KMeans

#### Week 4: Data structures for Parallel Computing
- Implementing combiners
- Parallel two-phase construction
- Conc-tree data structure
- Amortized, constant-time append operations
- Conc-tree combiners
- Assigment: Barnes-hut simulation
