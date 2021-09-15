### SOCIAL NETWORK ANALYSIS - PROJECT 2 - p2822003 - Vretteas Stylianos ###

# load df_consol files for each year that were created in task1 
data_2016<-read.csv(file.choose(), sep = ",", header = TRUE)
data_2017<-read.csv(file.choose(), sep = ",", header = TRUE)
data_2018<-read.csv(file.choose(), sep = ",", header = TRUE)
data_2019<-read.csv(file.choose(), sep = ",", header = TRUE)
data_2020<-read.csv(file.choose(), sep = ",", header = TRUE)

# remove X column
data_2016$X <- NULL
data_2017$X <- NULL
data_2018$X <- NULL
data_2019$X <- NULL
data_2020$X <- NULL

# check data type
summary(data_2016)
summary(data_2017)
summary(data_2018)
summary(data_2019)
summary(data_2020)

library(igraph)

# build network-graph object for each year 
# set weights as edge attributes in each igraph object 
# check if the graph is weighted
# print the graph

network_2016 <- graph_from_data_frame(d=data_2016, directed=FALSE)
network_2016 <- set.edge.attribute(network_2016, "weight", index=E(network_2016), data_2016[1:nrow(data_2016),]$Weight)
is_weighted(network_2016)
print(network_2016, e=TRUE, v=TRUE)

network_2017 <- graph_from_data_frame(d=data_2017, directed=FALSE)
network_2017 <- set.edge.attribute(network_2017, "weight", index=E(network_2017), data_2017[1:nrow(data_2017),]$Weight)
is_weighted(network_2017)
print(network_2017, e=TRUE, v=TRUE)

network_2018 <- graph_from_data_frame(d=data_2018, directed=FALSE)
network_2018 <- set.edge.attribute(network_2018, "weight", index=E(network_2018), data_2018[1:nrow(data_2018),]$Weight)
is_weighted(network_2018)
print(network_2018, e=TRUE, v=TRUE)

network_2019 <- graph_from_data_frame(d=data_2019, directed=FALSE)
network_2019 <- set.edge.attribute(network_2019, "weight", index=E(network_2019), data_2019[1:nrow(data_2019),]$Weight)
is_weighted(network_2019)
print(network_2019, e=TRUE, v=TRUE)

network_2020 <- graph_from_data_frame(d=data_2020, directed= FALSE)
network_2020 <- set.edge.attribute(network_2020, "weight", index=E(network_2020), data_2020[1:nrow(data_2020),]$Weight)
is_weighted(network_2020)
print(network_2020, e=TRUE, v=TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Task2 - Average degree over time  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

#  Network and node descriptives 

# number of vertices  
# number of edges
# diameter
# average degree - (2 * TOTAL EDGES) / TOTAL NODES

# 2016
vertices_2016 <- vcount(network_2016) 
edges_2016 <- ecount(network_2016)
diameter_2016 <- diameter(network_2016, directed = F)
avg_degree_2016 <- (2 * edges_2016) / vertices_2016

# 2017  
vertices_2017 <- vcount(network_2017) 
edges_2017 <- ecount(network_2017)
diameter_2017 <- diameter(network_2017, directed = F)
avg_degree_2017 <- (2 * edges_2017) / vertices_2017

# 2018  
vertices_2018 <- vcount(network_2018) 
edges_2018 <- ecount(network_2018)
diameter_2018 <- diameter(network_2018, directed = F)
avg_degree_2018 <- (2 * edges_2018) / vertices_2018

# 2019  
vertices_2019 <- vcount(network_2019) 
edges_2019 <- ecount(network_2019)
diameter_2019 <- diameter(network_2019, directed = F)
avg_degree_2019 <- (2 * edges_2019)/ vertices_2019

# 2020  
vertices_2020 <- vcount(network_2020) 
edges_2020 <- ecount(network_2020)
diameter_2020 <- diameter(network_2020, directed = F)
avg_degree_2020 <- (2 * edges_2020) / vertices_2020


# timeline vector
x <- c(2016,2017,2018,2019,2020)

# vertices vector
vertices <- c(vertices_2016,vertices_2017,vertices_2018,vertices_2019,vertices_2020)
plot(x,vertices, type = "l", col = "red", main = "VERTICES_EVOLUTION")
vertices

# edges vector
edges <- c(edges_2016,edges_2017,edges_2018,edges_2019,edges_2020)
plot(x,edges, type = "l", col = "red", main = "EDGES_EVOLUTION")
edges

# diameter vector 
diameter <- c(diameter_2016,diameter_2017,diameter_2018,diameter_2019,diameter_2020)
plot(x,diameter, type = "l", col = "red", main = "DIAMETER_EVOLUTION")
diameter

# average degree
avg_degree <- c(avg_degree_2016,avg_degree_2017,avg_degree_2018,avg_degree_2019,avg_degree_2020)
plot(x,avg_degree, type = "l", col = "red", main = "AVG_DEGREE_EVOLUTION")
avg_degree

# comments 
# vertices 
# edges
# diameter
# average degree 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Task3 - Important nodes   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

# Degree - based 

# 2016
degrees_2016 <- degree(network_2016, v = V(network_2016),loops = TRUE, normalized = FALSE)
degrees_sorted_2016 <- sort(degrees_2016, decreasing = TRUE)
degrees_top10_2016<- head(degrees_sorted_2016, n=10)
print(degrees_top10_2016)
# 2017
degrees_2017 <- degree(network_2017, v = V(network_2017),loops = TRUE, normalized = FALSE)
degrees_sorted_2017 <- sort(degrees_2017, decreasing = TRUE)
degrees_top10_2017<- head(degrees_sorted_2017, n=10)
print(degrees_top10_2017)
# 2018
degrees_2018 <- degree(network_2018, v = V(network_2018),loops = TRUE, normalized = FALSE)
degrees_sorted_2018 <- sort(degrees_2018, decreasing = TRUE)
degrees_top10_2018<- head(degrees_sorted_2018, n=10)
print(degrees_top10_2018)
# 2019
degrees_2019 <- degree(network_2019, v = V(network_2019),loops = TRUE, normalized = FALSE)
degrees_sorted_2019 <- sort(degrees_2019, decreasing = TRUE)
degrees_top10_2019<- head(degrees_sorted_2019, n=10)
print(degrees_top10_2019)
# 2020
degrees_2020 <- degree(network_2020, v = V(network_2020),loops = TRUE, normalized = FALSE)
degrees_sorted_2020 <- sort(degrees_2020, decreasing = TRUE)
degrees_top10_2020<- head(degrees_sorted_2020, n=10)
print(degrees_top10_2020)

# all years 
print(degrees_top10_2016)
print(degrees_top10_2017)
print(degrees_top10_2018)
print(degrees_top10_2019)
print(degrees_top10_2020)

# Pagerank

# 2016 
# PageRank implementation - algo parameter "prpack" by default 
p_rank_2016 <- page_rank(network_2016, 
                         vids = V(network_2016), 
                         directed = FALSE,
                         damping = 0.85)
typeof(p_rank_2016) # list output
# convert into df to manipulate the data 
p_rank_df_2016 <- as.data.frame(do.call(cbind, p_rank_2016))  
# rename Page_Rank value
names(p_rank_df_2016)[names(p_rank_df_2016)=="vector"] <- "Page_Rank"
# sort the Page_Rank column 
p_rank_df_2016<- p_rank_df_2016[order(-p_rank_df_2016$Page_Rank),]
# set index as Character column
p_rank_df_2016$Character <- rownames(p_rank_df_2016)
# remake index
rownames(p_rank_df_2016) <- 1:nrow(p_rank_df_2016)
p_rank_df_2016$value <- NULL
#reorder by column index
p_rank_df_2016 <- p_rank_df_2016[c(2,1)]
# get the first 10 
p_rank_df_2016[1:10,]
# top 10 Pagerank
pagerank_top10_2016 <- head(p_rank_df_2016$Character, n=10)  
pagerank_top10_2016

# 2017 
# PageRank implementation - algo parameter "prpack" by default 
p_rank_2017 <- page_rank(network_2017, 
                         vids = V(network_2017), 
                         directed = FALSE,
                         damping = 0.85)
typeof(p_rank_2017) # list output
# convert into df to manipulate the data 
p_rank_df_2017 <- as.data.frame(do.call(cbind, p_rank_2017))  
# rename Page_Rank value
names(p_rank_df_2017)[names(p_rank_df_2017)=="vector"] <- "Page_Rank"
# sort the Page_Rank column 
p_rank_df_2017<- p_rank_df_2017[order(-p_rank_df_2017$Page_Rank),]
# set index as Character column
p_rank_df_2017$Character <- rownames(p_rank_df_2017)
# remake index
rownames(p_rank_df_2017) <- 1:nrow(p_rank_df_2017)
p_rank_df_2017$value <- NULL
#reorder by column index
p_rank_df_2017 <- p_rank_df_2017[c(2,1)]
# get the first 10 
p_rank_df_2017[1:10,]
# top 10 Pagerank
pagerank_top10_2017 <- head(p_rank_df_2017$Character, n=10)  
pagerank_top10_2017

# 2018 
# PageRank implementation - algo parameter "prpack" by default 
p_rank_2018 <- page_rank(network_2018, 
                         vids = V(network_2018), 
                         directed = FALSE,
                         damping = 0.85)
typeof(p_rank_2018) # list output
# convert into df to manipulate the data 
p_rank_df_2018 <- as.data.frame(do.call(cbind, p_rank_2018))  
# rename Page_Rank value
names(p_rank_df_2018)[names(p_rank_df_2018)=="vector"] <- "Page_Rank"
# sort the Page_Rank column 
p_rank_df_2018<- p_rank_df_2018[order(-p_rank_df_2018$Page_Rank),]
# set index as Character column
p_rank_df_2018$Character <- rownames(p_rank_df_2018)
# remake index
rownames(p_rank_df_2018) <- 1:nrow(p_rank_df_2018)
p_rank_df_2018$value <- NULL
#reorder by column index
p_rank_df_2018 <- p_rank_df_2018[c(2,1)]
# get the first 10 
p_rank_df_2018[1:10,]
# top 10 Pagerank
pagerank_top10_2018 <- head(p_rank_df_2018$Character, n=10)  
pagerank_top10_2018

# 2019 
# PageRank implementation - algo parameter "prpack" by default 
p_rank_2019 <- page_rank(network_2019, 
                         vids = V(network_2019), 
                         directed = FALSE,
                         damping = 0.85)
typeof(p_rank_2019) # list output

# convert into df to manipulate the data 
p_rank_df_2019 <- as.data.frame(do.call(cbind, p_rank_2019))  
# rename Page_Rank value
names(p_rank_df_2019)[names(p_rank_df_2019)=="vector"] <- "Page_Rank"
# sort the Page_Rank column 
p_rank_df_2019<- p_rank_df_2019[order(-p_rank_df_2019$Page_Rank),]
# set index as Character column
p_rank_df_2019$Character <- rownames(p_rank_df_2019)
# remake index
rownames(p_rank_df_2019) <- 1:nrow(p_rank_df_2019)
p_rank_df_2019$value <- NULL
#reorder by column index
p_rank_df_2019 <- p_rank_df_2019[c(2,1)]
# get the first 10 
p_rank_df_2019[1:10,]
# top 10 Pagerank
pagerank_top10_2019 <- head(p_rank_df_2019$Character, n=10)  
pagerank_top10_2019

# 2020 
# PageRank implementation - algo parameter "prpack" by default 
p_rank_2020 <- page_rank(network_2020, 
                         vids = V(network_2020), 
                         directed = FALSE,
                         damping = 0.85)
typeof(p_rank_2020) # list output
# convert into df to manipulate the data 
p_rank_df_2020 <- as.data.frame(do.call(cbind, p_rank_2020))  
# rename Page_Rank value
names(p_rank_df_2020)[names(p_rank_df_2020)=="vector"] <- "Page_Rank"
# sort the Page_Rank column 
p_rank_df_2020<- p_rank_df_2020[order(-p_rank_df_2020$Page_Rank),]
# set index as Character column
p_rank_df_2020$Character <- rownames(p_rank_df_2020)
# remake index
rownames(p_rank_df_2020) <- 1:nrow(p_rank_df_2020)
p_rank_df_2020$value <- NULL
#reorder by column index
p_rank_df_2020 <- p_rank_df_2020[c(2,1)]
# get the first 10 
p_rank_df_2020[1:10,]
# top 10 Pagerank
pagerank_top10_2020 <- head(p_rank_df_2020$Character, n=10)  
pagerank_top10_2020

# all years Page_Rank
pagerank_top10_2016
pagerank_top10_2017
pagerank_top10_2018
pagerank_top10_2019
pagerank_top10_2020

# 2016 comparison 
degrees_top10_2016
p_rank_df_2016[1:10,]

# 2017 comparison
degrees_top10_2017
p_rank_df_2017[1:10,]

# 2018 comparison
degrees_top10_2018
p_rank_df_2018[1:10,]

# 2019 comparison
degrees_top10_2019
p_rank_df_2019[1:10,]

# 2020 comparison
degrees_top10_2020
p_rank_df_2020[1:10,]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Task4 - Communities   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

# algorithms - clustering - fast_greedy, infomap, louvain

# 2016
fast_greedy_2016 <- cluster_fast_greedy(network_2016)
infomap_2016 <- cluster_infomap(network_2016)
louvain_2016 <- cluster_louvain(network_2016)
# 2017
fast_greedy_2017 <- cluster_fast_greedy(network_2017)
infomap_2017 <- cluster_infomap(network_2017)
louvain_2017 <- cluster_louvain(network_2017)
# 2018
fast_greedy_2018 <- cluster_fast_greedy(network_2018)
infomap_2018 <- cluster_infomap(network_2018)
louvain_2018 <- cluster_louvain(network_2018)
# 2019
fast_greedy_2019 <- cluster_fast_greedy(network_2019)
infomap_2019 <- cluster_infomap(network_2019)
louvain_2019 <- cluster_louvain(network_2019)
# 2020
fast_greedy_2020 <- cluster_fast_greedy(network_2020)
infomap_2020 <- cluster_infomap(network_2020)
louvain_2020 <- cluster_louvain(network_2020)

# system time 
# 2016
system.time(cluster_fast_greedy(network_2016))
system.time(cluster_infomap(network_2016))
system.time(cluster_louvain(network_2016))
# 2017
system.time(cluster_fast_greedy(network_2017))
system.time(cluster_infomap(network_2017))
system.time(cluster_louvain(network_2017))
# 2018
system.time(cluster_fast_greedy(network_2018))
system.time(cluster_infomap(network_2018))
system.time(cluster_louvain(network_2018))
# 2019
system.time(cluster_fast_greedy(network_2019))
system.time(cluster_infomap(network_2019))
system.time(cluster_louvain(network_2019))
# 2020
system.time(cluster_fast_greedy(network_2020))
system.time(cluster_infomap(network_2020))
system.time(cluster_louvain(network_2020))

# comparison 2016
compare(fast_greedy_2016, infomap_2016)
compare(fast_greedy_2016, louvain_2016)
compare(louvain_2016, infomap_2016)

# comparison 2017
compare(fast_greedy_2017, infomap_2017)
compare(fast_greedy_2017, louvain_2017)
compare(louvain_2017, infomap_2017)

# comparison 2018
compare(fast_greedy_2018, infomap_2018)
compare(fast_greedy_2018, louvain_2018)
compare(louvain_2018, infomap_2018)

# comparison 2019
compare(fast_greedy_2019, infomap_2019)
compare(fast_greedy_2019, louvain_2019)
compare(louvain_2019, infomap_2019)

# comparison 2020
compare(fast_greedy_2020, infomap_2020)
compare(fast_greedy_2020, louvain_2020)
compare(louvain_2020, infomap_2020)

# INTERSECTION - find authors that are present in each year  
intersect_author<-Reduce(intersect, list(louvain_2016$names, louvain_2017$names, 
                                         louvain_2018$names, louvain_2019$names,louvain_2020$names))

# list of common authors - 218 in total 
intersect_author
length(intersect_author)

# choose louvain algorithm for performance reasons 
# membership - louvain algorithm 
membership_lv_2016 <- as.matrix(membership(louvain_2016))
membership_lv_2017 <- as.matrix(membership(louvain_2017))
membership_lv_2018 <- as.matrix(membership(louvain_2018))
membership_lv_2019 <- as.matrix(membership(louvain_2019))
membership_lv_2020 <- as.matrix(membership(louvain_2020))

# membership dataframes for each year

# 2016
df_members_2016 = as.data.frame((membership_lv_2016))
df_members_2016$Author <- rownames(df_members_2016)
rownames(df_members_2016) <- 1:nrow(df_members_2016)
# 2017
df_members_2017 = as.data.frame((membership_lv_2017))
df_members_2017$Author <- rownames(df_members_2017)
rownames(df_members_2017) <- 1:nrow(df_members_2017)
# 2018
df_members_2018 = as.data.frame((membership_lv_2018))
df_members_2018$Author <- rownames(df_members_2018)
rownames(df_members_2018) <- 1:nrow(df_members_2018)
# 2019
df_members_2019 = as.data.frame((membership_lv_2019))
df_members_2019$Author <- rownames(df_members_2019)
rownames(df_members_2019) <- 1:nrow(df_members_2019)
# 2020
df_members_2020 = as.data.frame((membership_lv_2020))
df_members_2020$Author <- rownames(df_members_2020)
rownames(df_members_2020) <- 1:nrow(df_members_2020)



# example -  Steven C. H. Hoi

# find out the community - cluster that Steven C. H. Hoi was part of each year  

df_members_2016$V1[which(df_members_2016$Author == "Steven C. H. Hoi")]
df_members_2017$V1[which(df_members_2017$Author == "Steven C. H. Hoi")]
df_members_2018$V1[which(df_members_2018$Author == "Steven C. H. Hoi")]
df_members_2019$V1[which(df_members_2019$Author == "Steven C. H. Hoi")]
df_members_2020$V1[which(df_members_2020$Author == "Steven C. H. Hoi")]

# communities are 282, 541, 27, 665, 732

# find out how many vertices each of the above communities consists - ( how many authors the community consists)
# compare vertices 
length(louvain_2016$membership[which(louvain_2016$membership == 281)])
length(louvain_2017$membership[which(louvain_2017$membership == 541)])
length(louvain_2018$membership[which(louvain_2018$membership == 27)])
length(louvain_2019$membership[which(louvain_2019$membership == 665)])
length(louvain_2020$membership[which(louvain_2020$membership == 732)])

# compare author similaritities in communities

subset1 <- subset(df_members_2016, V1 == 281)
subset2 <- subset(df_members_2017, V1 == 541)
subset3 <- subset(df_members_2018, V1 == 27)
subset4 <- subset(df_members_2019, V1 == 665)
subset5 <- subset(df_members_2020, V1 == 732)

# intersection 
similarities <- Reduce(intersect, list(subset1$Author,subset2$Author,subset3$Author,subset4$Author,subset5$Author))
similarities


# communities vertices plot

communities_vertices <- c(length(louvain_2016$membership[which(louvain_2016$membership == 281)]),
                          length(louvain_2017$membership[which(louvain_2017$membership == 541)]),
                          length(louvain_2018$membership[which(louvain_2018$membership == 27)]),
                          length(louvain_2019$membership[which(louvain_2019$membership == 665)]),
                          length(louvain_2020$membership[which(louvain_2020$membership == 732)]))
communities_vertices

# evolution of vertices for each year 
plot(x,communities_vertices,type = "l", col = "red", main = "AUTHOR'S_COMMUNITIES'_VERTICES_EVOLUTION")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Input from user   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

#intersect_author[30] # eg. "Georgios Theodoropoulos 0001"

var1 = intersect_author[30]

# remove comments (lines 534,535,536) to insert Author Name from intersect_author list

# intersect_author
#var1 = readline(prompt = "Enter author name from intersect_author list : ");
#var1 = as.character(var1);

# find out the community - cluster that var1  was part of each year   
example_1 <- df_members_2016$V1[which(df_members_2016$Author == var1)]
example_2 <- df_members_2017$V1[which(df_members_2017$Author == var1)]
example_3 <- df_members_2018$V1[which(df_members_2018$Author == var1)]
example_4 <- df_members_2019$V1[which(df_members_2019$Author == var1)]
example_5 <- df_members_2020$V1[which(df_members_2020$Author == var1)]

example_vector <- c(example_1, example_2, example_3, example_4, example_5)

# find out how many vertices each of the above communities consists - ( how many authors the community consists)
# compare vertices 

example_vertices <- c(length(louvain_2016$membership[which(louvain_2016$membership == example_vector[1])]),
                      length(louvain_2017$membership[which(louvain_2017$membership == example_vector[2])]),
                      length(louvain_2018$membership[which(louvain_2018$membership == example_vector[3])]),
                      length(louvain_2019$membership[which(louvain_2019$membership == example_vector[4])]),
                      length(louvain_2020$membership[which(louvain_2020$membership == example_vector[5])]))

# evolution of vertices for each year 
plot(x,example_vertices,type = "l", col = "red", main = "COMMUNITIES_VERTICES_EVOLUTION",
     xlab = "Timeline"  , ylab = var1)


# subsets to find similaritities for authors 
example_subset1 <- subset(df_members_2016, V1 == example_1)
example_subset2 <- subset(df_members_2017, V1 == example_2)
example_subset3 <- subset(df_members_2018, V1 == example_3)
example_subset4 <- subset(df_members_2019, V1 == example_4)
example_subset5 <- subset(df_members_2020, V1 == example_5)


example_similarities <- Reduce(intersect, list(example_subset1$Author,
                                               example_subset2$Author,
                                               example_subset3$Author,
                                               example_subset4$Author,
                                               example_subset5$Author))
example_similarities


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Plots   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

library(RColorBrewer)

# color attribute
V(network_2016)$color <- factor(membership(louvain_2016))
V(network_2017)$color <- factor(membership(louvain_2017))
V(network_2018)$color <- factor(membership(louvain_2018))
V(network_2019)$color <- factor(membership(louvain_2019))
V(network_2020)$color <- factor(membership(louvain_2020))


# Get the sizes of each community
community_size1 <- sizes(louvain_2016)
community_size2 <- sizes(louvain_2017)
community_size3 <- sizes(louvain_2018)
community_size4 <- sizes(louvain_2019)
community_size5 <- sizes(louvain_2020)

# Some mid-size communities
mid_communities1 <- unlist(louvain_2016[community_size1 > 40 & community_size1 < 90])
mid_communities2 <- unlist(louvain_2017[community_size2 > 40 & community_size2 < 90])
mid_communities3 <- unlist(louvain_2018[community_size3 > 40 & community_size3 < 90])
mid_communities4 <- unlist(louvain_2019[community_size4 > 40 & community_size4 < 90])
mid_communities5 <- unlist(louvain_2020[community_size5 > 40 & community_size5 < 90])

# Induce a subgraph of graph 
subgraph1 <- induced.subgraph(network_2016, mid_communities1)
subgraph2 <- induced.subgraph(network_2017, mid_communities2)
subgraph3 <- induced.subgraph(network_2018, mid_communities3)
subgraph4 <- induced.subgraph(network_2019, mid_communities4)
subgraph5 <- induced.subgraph(network_2020, mid_communities5)

# Does the edge cross betwen commmunities?
is_crossing1 <- crossing(network_2016, communities = louvain_2016)
is_crossing2 <- crossing(network_2017, communities = louvain_2017)
is_crossing3 <- crossing(network_2018, communities = louvain_2018)
is_crossing4 <- crossing(network_2019, communities = louvain_2019)
is_crossing5 <- crossing(network_2020, communities = louvain_2020)

# Set the edge line type,  solid is for crossings, dotted otherwise 
E(network_2016)$lty <- ifelse(is_crossing1, "solid", "dotted")
E(network_2017)$lty <- ifelse(is_crossing2, "solid", "dotted")
E(network_2018)$lty <- ifelse(is_crossing3, "solid", "dotted")
E(network_2019)$lty <- ifelse(is_crossing4, "solid", "dotted")
E(network_2020)$lty <- ifelse(is_crossing5, "solid", "dotted")

# communities subgraphs
community_subgraph1 <- cluster_louvain(subgraph1)
community_subgraph2 <- cluster_louvain(subgraph2)
community_subgraph3 <- cluster_louvain(subgraph3)
community_subgraph4 <- cluster_louvain(subgraph4)
community_subgraph5 <- cluster_louvain(subgraph5)

# Plot  mid-size communities
# 2016
plot(subgraph1, 
     vertex.color=rainbow(16, alpha=1)[community_subgraph1$membership], 
     vertex.label=NA, 
     edge.arrow.size=.2, 
     vertex.size=7,
     margin = 0,  
     coords = layout_with_lgl(subgraph1), 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     lty=E(network_2016)$lty, 
     main="Communities of 2016")

# 2017
plot(subgraph2, 
     vertex.color=rainbow(16, alpha=1)[community_subgraph2$membership], 
     vertex.label=NA, 
     edge.arrow.size=.2, 
     vertex.size=7,
     margin = 0,  
     coords = layout_with_lgl(subgraph2), 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     lty=E(network_2017)$lty, 
     main="Communities of 2017")

# 2018
plot(subgraph3, 
     vertex.color=rainbow(16, alpha=1)[community_subgraph3$membership], 
     vertex.label=NA, 
     edge.arrow.size=.2, 
     vertex.size=7,
     margin = 0,  
     coords = layout_with_lgl(subgraph3), 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     lty=E(network_2018)$lty, 
     main="Communities of 2018")

# 2019
plot(subgraph4, 
     vertex.color=rainbow(16, alpha=1)[community_subgraph4$membership], 
     vertex.label=NA, 
     edge.arrow.size=.2, 
     vertex.size=7,
     margin = 0,  
     coords = layout_with_lgl(subgraph4), 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     lty=E(network_2019)$lty, 
     main="Communities of 2019")

# 2020
plot(subgraph5, 
     vertex.color=rainbow(16, alpha=1)[community_subgraph5$membership], 
     vertex.label=NA, 
     edge.arrow.size=.2, 
     vertex.size=7,
     margin = 0,  
     coords = layout_with_lgl(subgraph5), 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     lty=E(network_2020)$lty, 
     main="Communities of 2020")

