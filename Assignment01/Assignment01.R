# install.packages("network")
# install.packages("sna")

library(network)
library(sna)


# ============================================================================ #

# 1) Given a network (N, g)
# Show that if a network is not connected then its complement is.
# Provide an example of a four node network that is connected
# and such that its complement is also connected.

# NOTE THAT: MAYBE, I MISUNDERSTOOD BUT QUESTION 1 AND 2 ARE SAME.

# Four node unconnected graph.
g1 <- network.initialize(4, directed = F)
g1
add.edge(g1, 1, 2)
g1[,]
is.connected(g1) # FALSE, Obviously
gplot(g1, label = g1 %v% 'vertex.names')

# Just that easy to take complemet of a network/graph
# complementOfg1 <- !g1 # Somehow this doesn't give correct answer
complementOfg1 <- as.network(!g1[,]) # This gives correct one !
complementOfg1
complementOfg1[,]
is.connected(complementOfg1)
gplot(complementOfg1, label = complementOfg1 %v% 'vertex.names')

# Define this as a function, I don't know if there exist any function
# in libraries.
complement <- function(g){ as.network(!g[,]) }

# 2) Generate sample networks in R:
#     G is connected and G' is also connected
#     G is not connected G' is connected.

# G is connected and G' is also connected

net1 <- network.initialize(4, directed = F)
from <- c(1,2,3)
to <- c(2,3,4)
net1[,] <- 0
add.edges(net1,from,to)
net1
is.connected(net1)
gplot(net1, label = net1 %v% 'vertex.names')

net2 <- complement(net1)
net2
is.connected(net2)
gplot(net2, label = net2 %v% 'vertex.names')

# G is not connected G' is connected.

net3 <- network.initialize(5, directed = F)
from <- c(1,2,3,4)
to <- c(2,3,4,1)
net3[,] <- 0
add.edges(net3,from,to)
net3
is.connected(net3)
gplot(net3, label = net3 %v% 'vertex.names')

net4 <- complement(net3)
net4
is.connected(net4)
gplot(net4, label = net4 %v% 'vertex.names')


# 3) Write an R function which checks wheather a given graph G is a tree or not.

# So from book,
# A tree is a connected network that has no cycles.
# is.connected(g)

# is.tree <- function(g){ is.connected(g) && !has.cycles(g) }
# so we need a has.cycles function ??

# Also, can be deriven from definition above:

# A connected network is a tree if and only if it has n-1 links.
# |E| = |V| - 1
# So,
has.proper.size <- function(g){
    vertex.count <- network.size(g)
    edge.count <- network.edgecount(g)
    vertex.count > 3 && edge.count == vertex.count - 1
}
# A tree has at least two leaves, where leaves are nodes that have exactly one link.
# --- hasProperSize function check this property !!

# In a tree, there is a unique path between any two nodes.
# --- Again if it has cycles or not ??

has.cycles <- function(g){
    # kcycle.census calculating k-cycle census statistics,
    cc <- kcycle.census(g, tabulate.by.vertex = FALSE, mode = 'graph')
    # it returns a tabulate represented as a list,
    cc <- cc$cycle.count == 0
    # check if for all k-cycles is there a cycle or not.
    !Reduce("&", cc)
    # Fold boolean values that cc has if it is true is has cycles, otherwise no.
}

# Consequently,
is.tree <- function(g){ has.proper.size(g) && is.connected(g) && !has.cycles(g) }




