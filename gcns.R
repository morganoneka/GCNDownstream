library(igraph)
library(tidyverse)
library(reshape)

# g = an igraph object
# percentage = a numeric value between 0 and 1
getTopXPercentHubGenes <- function(g, percentage){
  # calculate hub scores
  hub_scores <- hub.score(g)
  
  # calculate how many genes "top 10%" is
  num_genes = ceiling(length(hub_scores$vector)*percentage)
  
  # sort in decreasing order of hub score, then grab the top 10%
  return(head(names(sort(hub_scores$vector, decreasing=TRUE)), num_genes))
}

# adj = a symmetric adjacency matrix, such as a covariance matrix
# cutoff = value differentiating a significant relationship
convertAdjacencyToBinary <- function(adj, cutoff){
  # the multiply by 1 is weird but it converts from logical to numeric
    return(as.matrix(abs(adj) >= cutoff) * 1)
}

# graphlist = a list of binary matrices
calculateEdgeOccurrences <- function(matrixlist){
  # sum all the binary matrices together
  edge_sums = Reduce('+', matrixlist)
  
  # melt to convert to a long-style matrix
  edge_sums_melted = melt(edge_sums)
  edge_sums_melted$X1 = as.character(edge_sums_melted$X1)
  edge_sums_melted$X2 = as.character(edge_sums_melted$X2)
  colnames(edge_sums_melted) = c("Node1", "Node2", "Count")
  
  return(edge_sums_melted)
}

# identify which edges are unique based on binarization
# binarymxlist = list of binary matrices (output from convertAdjacencyToBinary)
# output: list containing graph of unique edges
identifyUniqueEdges <- function(binarygraphs){
  
  # iterate over all binary matrices in the list
  lapply(1:length(binarygraphs), function(i){
    
    # combine all graphs BUT index i
    combo_graph = do.call(union, binarygraphs[setdiff(1:length(binarygraphs), i)])
    
    # identify what edges ARE in the combo graph
    intersect_graph = intersection(combo_graph, binarygraphs[[i]])
    
    # return the edges NOT in that intersection
    return(difference(binarygraphs[[i]], intersect_graph))
  })
}

# originalgraphs: the whole genetic coexpression networks
# uniqueedgelist: output from identifyUniqueEdges
# these two lists MUST be in the same order (i.e. the UEN at index 1 corresponds to the whole GCN at index 1)
createWeightedUniqueGraph <- function(originalgraphs, uniqueedgelist){
  lapply(1:length(originalgraphs), function(i){
    
    # find the intersection between the unique edge graph (binarized) and the original graph
    intersect_graph = intersection(uniqueedgelist[[i]], originalgraphs[[i]])
    
    # so unweighted graphs are considered to have an edge weight of 1
    # therefore when we intersect above, it adds an edge weight of 1 to our original graph
    # so we have to subtract by 1 to get the true weights of the original graph
    intersect_graph = set_edge_attr(intersect_graph, "weight", value= edge.attributes(intersect_graph)$weight - 1)
  })
  
}

