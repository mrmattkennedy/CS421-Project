## CS421 Project Submission
This is Matthew Kennedy's report submission for the CS421 final project. This repository includes the code used to exemplify the learnings from the paper [Composable Memory Transactions by Harris. et al](https://dl.acm.org/doi/10.1145/1065944.1065952).

Related video: https://youtu.be/81NZ2-RTQHM

### Running code
To run the 3 code files, simply clone this repository and run the below lines of code:
```
cabal build
cabal run chapter3
cabal run chapter4
cabal run chapter6
```

### Final report outline
#### Overview
*Describe the motivation, goals, and broad accomplishments of your project in general terms.*

The motivation for this project was to be able to replicate referenced issues facing programmers dealing with concurrency from ~2005 by using the above paper from Harris et al. While this was nearly 20 years ago, the major problems referenced are still prevelant today. 
The goals of this project were to understand the solutions sought by Harris et al., and to write simple examples of their solutions using Haskell to understand the solutions better.
Both of these goals were accomplished - my findings can be found in both the attached pdf, as well as the referenced video (https://youtu.be/81NZ2-RTQHM). Additionally, I have a much better understanding of how transactions can be used when dealing with concurrency.

#### Implementation
*A brief description of the important aspects of your implementation, in terms of (a) the major tasks or capabilities of your code; (b) components of the code; (c) status of the project -- what works well, what works partially, and and what is not implemented at all. You MUST compare these with your original proposed goals in the project proposal.*


- (a) the major tasks or capabilities of your code: There are 3 parts of my code which break down the major implementation chapters from Harris et al. - the first part, `ch3.hs`, showcases the overall concepts that are foundational to the authors' solution. These concepts are transaction variable or `TVar`, `readTVar`, `writeTVar`, `retry`, `blocking`, and `orElse`. The second part, `ch4.hs`, dives more into using these `TVars` to build concurrent programming principles, such as mutable variables which are used to safely communicate between threads, as well as multicast channels where messages can be broadcasted to multiple readers. Lastly, in `ch6.hs`, I explore more of automatic conflict handling through STM, and nested transactions.

- (b) components of the code: This can be seen by going through each file. There are many comments to help identify the components. I will briefly list the major components below.
    - In `ch3.hs`, there is a `Counter` which is of type `TVar Int`, and there are functions `incrementCounter` to increment a `TVar Int`, a `decrementCounter` to try to decrement or block if that counter not available.
    - In `ch4.hs`, there is an `MVar` which is of type `TVar (Maybe a)`, since this MVar is either `Nothing` or "full" (has a value). There are functions `putMVar` to try to atomically put a value into a passed `MVar`, as well as `takeMVar` to try and consume from some passed `MVar`. In the second part of `ch4.hs`, there is an `MChan` which is of type `(TVar [TVar (Maybe a)])`, as it is a port as well as a list of TVars that listen and read. Data cam be passed to an `MChan` via `writeMChan`. We can define a `Port` via `newPort` that listens to an `MChan`, and ports can be read atomically via `readPort`.
    - In `ch6.hs`, we have a `Resource` of type `TVar Int`. We have functions `addResource`, which adds to the value of some `Resource`, and `consumeResource`, which does the opposite. We also have `nestedTransaction` to showcase a nested transaction.

- (c) status of the project: 
    From my project proposal: "My goal with this summary and/or presentation is to showcase how this new model of concurrency within functional programming attempts to solve several issues inherent to concurrency at the time the paper was written, and to try and understand more of why these problems were difficult, and how this new model of concurrency attempts to resolve this issue."

    - **what works well** - Most of the examples I sought to create work well to demonstrate the points made in this paper by Harris et al. I wanted to showcase how the authors' model can help solve problems with conurrency, and I believe I did that by showcasing how well `TVars` can be used to build up concurrent programming blocks. I also believe these examples showcase why some of these problems were difficult, in a few different ways. One was by explaining the problems, such as deadlock or blocking or resource starvation. The second was by showcasing how Transactions and STM can be used to solve these problems.
    - **what works partially** - The only thing I would say works "partially" would be there are limited examples. I could have spent a *significant* amount more time trying to truly replicate what Haskell1998 may have looked like as well as trying to following the authors' *exact samples*, but this would have been difficult, given it has been nearly 20 years since then, and the code provided in this paper were largely pseudocode or samples of what should have been a much larger piece.
    - **what is not implemented at all** - There were no goals I described that have not been implemented at all. I have demonstrated the new model of concurrency with examples (STM), described issues that were focused on by the authors, understood why these problems were difficult, and showcased how STM handles various issues.

#### Tests
*Coming up with appropriate tests is one of the most important part of good software development. Your tests should include unit tests, feature tests, and larger test codes. Give a short description of the tests used, performance results if appropriate (e.g., memory consumption for garbage collection) etc. Be sure to explain how these tests exercise the concept(s) you've implemented.*

#### Listing
*A listing of your code. The code should be documented thoroughly and clearly. You don't need to comment every single line or even every single function. Instead, focus on the central functions and data structures in your implementation, and document them well.*