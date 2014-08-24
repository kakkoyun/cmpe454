# install.packages("network")
# install.packages("sna")
# install.packages("igraph")

library(network)
library(sna)
library(igraph)

# Assignment 03
# =====================================

# 1) Compare your results
# 2) Examine how they are implemented
# 3) Examine the differences at running them when on directed and when on undirected graphs.

# Generate some random networks/graphs.
m0 <- rgraph(100, tprob=0.3, mode="graph")
m1 <- rgraph(100, tprob=0.5, mode="graph")
m2 <- rgraph(100, tprob=0.8, mode="graph")

m3 <- rgraph(100, tprob=0.3, mode="digraph")
m4 <- rgraph(100, tprob=0.5, mode="digraph")
m5 <- rgraph(100, tprob=0.8, mode="digraph")

# find.triangles : igraph graph object -> number
find.triangles <- function(g){
    # a function from igraph library
    graph.motifs.no(g, size = 3)
}

# find.distinct.2.neigbours : matrix -> number
find.distinct.2.neigbours <- function(m){
    acc <- 0
    for(n in 1:dim(m)[1]){
      acc <- acc + dim(combn(length(
            Filter((function(x) x != 0), m[n,])), 2))[2]
    }
    acc
}

# Implement an R function which computes graph level clustering coefficient of of a given network.

# Note that you are not expected to use node level clustering coefficient and have an average value as the graph level cluster.

# Graph level clustering coefficients function,
# formula from book page 58.

# gLC : matrix string -> number
gLC <- function(m, mode){
    g <- graph.adjacency(m, mode=mode)
    nominator <- find.triangles(g)
    denominator <- find.distinct.2.neigbours(m)
    nominator / denominator
}

gLC(m0, "undirected")
gLC(m1, "undirected")
gLC(m2, "undirected")

gLC(m4, "directed")
gLC(m5, "directed")
gLC(m6, "directed")

# In R generate a network and run following centrality measures:
# degree (degree)
# closeness (closeness)
# betweenness (betweenness)
# bonacich power (bonpow)
# eigenvector (eigen)
# prestige (prestige)

# Undirected
sna::degree(m0, gmode="graph")
sna::closeness(m0, gmode="graph", cmode="undirected")
sna::betweenness(m0, gmode="graph", cmode="undirected")

sna::centralization(m0,sna::degree)

sna::bonpow(m0, gmode="graph")
sna::bonpow(m0, gmode="graph", exponent=2)
sna::bonpow(m0, gmode="graph", exponent=3)
sna::bonpow(m0, gmode="graph", exponent=4)
sna::evcent(m0, gmode="graph")
sna::prestige(m0, gmode="graph")

# Directed since default is gmode="digraph" no need to specify!
sna::degree(m3)
sna::degree(m3,cmode="indegree")
sna::degree(m3,cmode="outdegree")

sna::closeness(m3)
sna::betweenness(m3)
sna::bonpow(m3)
sna::bonpow(m3, exponent=2)
sna::bonpow(m3, exponent=3)
sna::bonpow(m3, exponent=4)

sna::evcent(m3)
sna::prestige(m3)


# How do they implement them?
sna::degree
sna::closeness
sna::betweenness
sna::bonpow
sna::evcent
sna::prestige
