### SOCIAL NETWORK ANALYSIS - PROJECT 1 - p2822003 - Vretteas Stylianos ###

#~~~~~~~~~~~~ Task1 - `A Song of Ice and Fire' network ~~~~~~~~~~~~~~~~~~~~#

# libraries 
library(igraph)

# load the dataset
got_data<-read.csv(file.choose(),sep = ",")
head(got_data)

View(got_data)
summary(got_data)

# find NA values in got_data
na_values <- which(colSums(is.na(got_data)) > 0)
sort(colSums(sapply(got_data[na_values], is.na)), decreasing = TRUE) # no NA values

# clean the dataset 
got_data$Type <- NULL
got_data$id <- NULL


# build the network 

# An edge list has 2 columns. 
# Each row represents a connection between an origin (Source) 
# and a destination (Target).

network1 <- graph_from_data_frame(d=got_data, directed=FALSE)
print(network1, e=TRUE, v=TRUE)

# plot 
plot1<-plot(network1) # not good 

#~~~~~~~~~~~~ Task2 - Network Properties ~~~~~~~~~~~~~~~~~~~~# 

#  Network and node descriptives 

# number of vertices - 796 
vcount(network1) 

# number of edges - 2823 
ecount(network1)

# diameter - 53
diameter(network1, directed = F)

# number of triangles
sum(count_triangles(network1, vids = V(network1)))

# The top-10 characters of the network as far as their degree is concerned

# The degree of a vertex is its most basic structural property, the number of its adjacent edges.
# degrees
degrees <- degree(network1, v = V(network1),loops = TRUE, normalized = FALSE)
degrees_sorted <- sort(degrees, decreasing = TRUE)
degrees_top10<- head(degrees_sorted, n=10)
degrees_top10

# The top-10 characters of the network as far as their weighted degree is concerned
# Summing up the edge weights of the adjacent edges for each vertex.
w_degrees <- strength(network1, vids = V(network1), loops = TRUE, weights = NULL)
w_degrees_sorted <- sort(w_degrees, decreasing = TRUE)
w_degrees_top10 <- head(w_degrees_sorted, n=10)
w_degrees_top10

# Simililarities
degrees_top10[(names(degrees_top10) %in% names(w_degrees_top10))]
w_degrees_top10[(names(w_degrees_top10) %in% names(degrees_top10))]

# differences
degrees_top10[!(names(degrees_top10) %in% names(w_degrees_top10))]
w_degrees_top10[!(names(w_degrees_top10) %in% names(degrees_top10))]

#~~~~~~~~~~~~ Task3 - Subgraph ~~~~~~~~~~~~~~~~~~~~# 

# best plot 
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
layouts

plot(network1, 
     layout=layout_with_fr, 
     vertex.color="steelblue", 
     vertex.label = NA, 
     edge.arrow.width=15, 
     vertex.size=3,
     vertex.cex = 3,)


# Subgraph

#Then, you will create a subgraph of the network, by discarding all vertices that
#have less than 10 connections in the network, and plot the subgraph.
 
vertices_10 <- degrees >= 10

plot(induced_subgraph(network1, vertices_10),
     layout=layout_with_fr,
     vertex.label = NA, 
     edge.arrow.width=3, 
     vertex.shape="circle",
     vertex.size= 8)


# In addition to the above plots, you are also asked to write code that calculates
# the edge density of the entire graph, as well as the aforementioned subgraph,
# and provide an explanation on the obtained results (a few sentences in your report).



# The density of a graph is the ratio of the number of edges and the number of possible edges
# Edge density
edge_density(network1, loops = FALSE)
edge_density(induced_subgraph(network1, vertices_10),
             loops = FALSE)



#~~~~~~~~~~~~ Task4 - Centrality ~~~~~~~~~~~~~~~~~~~~# 


# closeness centrality
# Closeness (centrality based on distance to others in the graph)
# Inverse of the node’s average geodesic distance to others in the network.

close <- closeness(network1, vids = V(network1), weights = NULL, normalized = FALSE)
close_sort <- sort(close, decreasing =  TRUE)
closeness_top15 <- head(close_sort, n=15)
closeness_top15

# betweeness centrality
# Betweenness (centrality based on a broker position connecting others)
# Number of geodesics that pass through the node or the edge.
between <- betweenness(network1, v = V(network1), directed = TRUE, weights = NULL, nobigint = TRUE, normalized = FALSE)
between_sort <- sort(between, decreasing = TRUE)
betweeness_top15 <- head(between_sort, n=15)
betweeness_top15


#~~~~~~~~~~~~ Task5 - Ranking and Visualization ~~~~~~~~~~~~~~~~~~~~# 

# rank the characters of the network with regard to their PageRank value

# PageRank implementation - algo parameter "prpack" by default 
p_rank <- page_rank(network1, 
                    vids = V(network1), 
                    directed = FALSE,
                    damping = 0.85)

typeof(p_rank) # list output

# convert into df to manipulate the data 
p_rank_df <- as.data.frame(do.call(cbind, p_rank))  
# rename Page_Rank value
names(p_rank_df)[names(p_rank_df)=="vector"] <- "Page_Rank"
# sort the Page_Rank column 
p_rank_df<- p_rank_df[order(-p_rank_df$Page_Rank),]
# set index as Character column
p_rank_df$Character <- rownames(p_rank_df)
# remake index
rownames(p_rank_df) <- 1:nrow(p_rank_df)
p_rank_df$value <- NULL
#reorder by column index
p_rank_df <- p_rank_df[c(2,1)]
# get the first 10 
p_rank_df[1:10,]


# revalue the Page_Rank value for bigger nodes in the graph
page_rank_revalue <- as.numeric(p_rank_df[,2] * 1000)

# plot the Pagerank
plot(network1, 
     layout=layout_with_fr, 
     vertex.color="steelblue", 
     vertex.label = NA, 
     edge.arrow.width=10, 
     vertex.size=page_rank_revalue)

