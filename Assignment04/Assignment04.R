# Comp454 - Assigment 04
# Kemal Akkoyun
# 11076004
# =============================================================================

# install.packages("network")
# install.packages("sna")

library(network)
library(sna)

# Implement the configuration model algorithm in R.
# The algorithm generates a random network of a desired degree distribution.
# The detail of algorithm is described in 4.1.4. Check the text book.

# That is, given a network on n nodes, we end up with a list of the degrees of
# different nodes: (d1, d2, ..., dn), which is the degree sequence.


# makeDegreeSequnence : vector -> vector
# Purpose : Order degree distribution and produce degree sequence.
makeDegreeSequnence <- function(degDist){
  sort(degDist)
}

# initializeGraph : number -> network
# Purpose : To initialize a network.
initializeGraph <- function(n){
  net <- network.initialize(n, directed = FALSE)
  net
}

# randomNode : vector -> number
# Purpose : Select a random number from given vector which is not 0.
randomNode <- function(degSeq){
  x <- sample(degSeq, 1, replace=T)
  while(x <= 0){
    x <- sample(degSeq, 1, replace=T)
  }
  x
}

# Assuptions:
# -- Undirected and
# -- Weighted Graphs to be produced.

# randomNetwork : vector -> network
# Purpose : Generates a random network through given degree distribution by using,
#         - distribution model.
randomNetwork <- function(degDist){
  degSeq <- makeDegreeSequnence(degDist)
  net <- initializeGraph(length(degSeq))
  while(Reduce("+", degSeq) > 1){
    a <- randomNode(degSeq)
    b <- randomNode(degSeq)
    indexA <- match(c(a), degSeq)
    indexB <- match(c(b), degSeq)
    add.edges(net, indexA, indexB)
    degSeq[indexA] <- (a - 1)
    degSeq[indexB] <- (b - 1)
  }
  net
}

# Tests and Plots:
degSeq <- c(1,1,2,2,3,4,4,4,5,7)
net <- randomNetwork(degSeq)
gplot(net, gmode = 'graph', label = net %v% 'vertex.names')

