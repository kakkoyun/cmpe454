# install.packages("network")
# install.packages("sna")

library(network)
library(sna)

# Assignment - 02

# 1) Write an R function/module which computes diammeter of a given Network object.
# Use the method which finds number of walks of lenghth k in between nodes i and j.

# So diameter is the longest shortest path and  kth power of a certain adjencency matrix
# - gives the k length walks between given nodes in resulting matrix.

is.zero.matrix <- function(m){ Reduce("&", m == 0) }

# There is an ill case in this algorithm if you have cycles in graph, because
# - walks can include cycles so this fuction do not return correct result for graphs with cycles.
find.diameter.V1 <- function(g){
    matrix <- g[,]
    acc <- matrix
    i <- 0
    while(!is.zero.matrix(matrix)){
        acc <- acc %*% matrix
        i <- i + 1
    }
    i
}

# Straight forward but givews correct result but
# - it depends on a shortest path distribution funtion from sna.
find.diameter <- function(g){
    max(geodist(g)$counts)
}

# 2) Write R functions:

# Detects a complete graph.
# All nodes must have exactly V-1 edges.
is.complete <- function(g){
    Reduce("&", degree(g, cmode="outdegree") == (length(g) - 1))
}

# Detects a star graph.
# Center node has V - 1 edges and rest of them only one.
is.star <- function(g){
    degreeList <- degree(g, cmode="outdegree")
    Reduce("&", ((degreeList == (length(g) - 1)) || (degreeList == 1))) && length(Filter((function(x) x == (length(g) - 1)), degreeList)) == 1
}

# Detects a circle graph.
# All nodes must have exactly 2 edges.
is.circle <- function(g){
    Reduce("&", degree(g, cmode="outdegree") == 2)
}
