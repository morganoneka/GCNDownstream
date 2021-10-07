# GCNDownstream
A collection of scripts for comparing & contrasting GCNs and functional output from DAVID, related to a forthcoming paper.

## Data format
You will need:
- A directory containing GCNs. Should be an adjacency matrix, covariance, something like this.
- A directory containing DAVID output. (TODO: include a tutorial for running DAVID)

(TODO: tutorial using dummy data)

## What these scripts do:
### Analysis of GCNs
- Convert to igraph
- Identify edge counts
- Calculate hub scores
- Identify unique edge networks
- Compare whole GCNs to UENs

### Analysis of DAVID output
- Split into output for each database
- Count occurrences 
- Identify differences and similarities between 2+ outputs
