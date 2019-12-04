library(igraph)
library(dplyr)
library(corrr)
library(tidyverse)
library(ggraph)
setwd("~dir")

df_twitter<- read_excel("dataset.xlsx")
View(df_twitter)
summary(df_twitter)
sum(is.na(df_twitter))

df_twitter <- df_twitter[,-1]

df_twitter_corr <- df_twitter %>% correlate() %>% stretch()
names(df_twitter_corr)[3]<-"corr_coeff"

nw_corr_graph <- df_twitter_corr %>% filter(abs(corr_coeff) > 0.35) %>% graph_from_data_frame(directed = FALSE)
V(nw_corr_graph) #44
E(nw_corr_graph) #90

#Network Visualization with Correlation 
ggraph(nw_corr_graph, layout = 'kk') +
geom_edge_link(aes(edge_alpha = abs(corr_coeff), edge_width = abs(corr_coeff), color = corr_coeff)) +
guides(edge_alpha = "none", edge_width = "none") +
scale_edge_colour_gradientn(limits = c(-0.3, 0.85), colors = c("firebrick2", "dodgerblue2")) +
geom_node_point(color = "darkgreen", size = 5) +
geom_node_text(aes(label = name), repel = TRUE) +
theme_graph() +
labs(title = "Network Correlation Between Nasdaq 100 Companies")


install.packages('SemiPar')
install.packages('qgraph')
library(SemiPar)
library(Hmisc)
library(qgraph)

library(Hmisc)
rcorr(as.matrix(df_twitter, type='pearson'))


cormat<-cor(df_twitter)  #correlation matrix generated
View(cormat)

library(readxl)
industry <- read_excel("dataset.xlsx", 
                                      sheet = "Industry")
View(industry)

#Creating list of indexes for each Industry
ind_list <- list()
ind_list[["Entertainment"]]<- c(11,
                                30,
                                31,
                                39,
                                52,
                                55,
                                61,
                                65,
                                75)

ind_list[["Hardware"]]<- c(38,
                           76,
                           90)
ind_list[["Hardware & Software"]]<- c(20,
                                      24,
                                      28,
                                      63)
ind_list[["Healthcare"]]<- c(6,
                             8,
                             14,
                             18,
                             19,
                             33,
                             41,
                             44,
                             45,
                             48,
                             51,
                             60,
                             81,
                             86,
                             87,
                             91)

ind_list[["IT Consulting"]]<- c(15,
                                26,
                                46)

ind_list[["Others"]]<- c(7,
                         10,
                         21,
                         35,
                         49,
                         67,
                         68,
                         72,
                         74,
                         78)

ind_list[["Retail"]]<- c(9,
                         12,
                         23,
                         29,
                         32,
                         71,
                         73,
                         77,
                         82,
                         88)

ind_list[["Semiconductors"]]<- c(16,
                                 40,
                                 50,
                                 53,
                                 54,
                                 56,
                                 57,
                                 58,
                                 59,
                                 64,
                                 70)

ind_list[["Software"]]<- c(1,
                           2,
                           3,
                           5,
                           17,
                           27,
                           47,
                           66,
                           80)

ind_list[["Technology"]]<- c(36,
                             37,
                             42,
                             43,
                             79)

ind_list[["Telecom"]]<- c(22,
                          62,
                          83,
                          84)

ind_list[["Travel"]]<- c(25,
                         34,
                         69,
                         89)

View(ind_list)


qgraph(cormat, shape='circle', posCol='darkgreen', negCol='darkred', layout='spring', vsize=6)




# Correlations:
qgraph(cor(df_twitter), minimum = 0.25,posCol = "darkgreen", negCol = "darkred", cut = 0.4, vsize = 4, groups = ind_list, 
            legend = TRUE, borders = TRUE)
title("Correlation between Companies segregated by Industries", line = 2.5)


# Same graph with spring layout:
qgraph(cor(df_twitter), layout = "spring",posCol = "darkgreen", negCol = "darkred", minimum = 0.25, cut = 0.4, vsize = 4, groups = ind_list, 
            legend = TRUE, borders = TRUE)
title("Correlations between Companies segregated by Industries", line = 2.5)


#Calculating the partial correlation coefficients between nodes 
pcorr <- cor(df_twitter)
n <- dim(df_twitter)[1]
pcorr.pvals <- matrix(0, dim(pcorr)[1], dim(pcorr)[2])
for(i in seq(1, 92)){
  for(j in seq(1, 92)){
    rowi <- pcorr[i, -c(i, j)]
    rowj <- pcorr[j, -c(i, j)]
    tmp <- (pcorr[i, j] - rowi*rowj)/sqrt((1-rowi^2) * (1-rowj^2))
    tmp.zvals <- (0.5) * log((1+tmp) / (1-tmp))
    tmp.s.zvals <- sqrt(n-4) * tmp.zvals
    tmp.pvals <- 2 * pnorm(abs(tmp.s.zvals), 0, 1, lower.tail=F)
    pcorr.pvals[i, j] <- max(tmp.pvals)
  }
}

#Fisher's transformation to determine the confidence intervals for p-values
z <- 0.5 * log((1 + pcorr) / (1 - pcorr))
z.vec <- v[upper.tri(z)]
n <- dim(df_twitter)[1]
corr.pvals <- 2*pnorm(abs(z.vec), 0, sqrt(1/(n-3)), lower.tail=FALSE)

#Benjamini-Hochberg adjustment using threshold of p < 0.05 
pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")

#applying a nominal threshold of 0.05
pcorr.edges <- (pcorr.pvals.adj < 0.05)
length(pcorr.pvals.adj[pcorr.edges])  
# 48

#Alpha = 0.05 
Companies <- names(df_twitter)
pcorr.A <- matrix(0, length(Companies), length(Companies))
rownames(pcorr.A) <- Companies
colnames(pcorr.A) <- Companies
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")

#showing only the nodes of degree greater than zero; i.e. only nodes that are connected to at least one other node.
g.pcorr<- delete.vertices(g.pcorr, which(degree(g.pcorr)<1))
V(g.pcorr)
E(g.pcorr)
V(g.pcorr)$size =6
E(g.pcorr)$color = "purple"
plot(g.pcorr, vertex.label.dist=1,vertex.label.size=0.5, vertex.color="orange",
     vertex.label.color="darkblue",main='Partial Correlation with p<0.05',
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.1)



#applying a threshold of 0.01
pcorr.edges <- (pcorr.pvals.adj < 0.01)
length(pcorr.pvals.adj[pcorr.edges])  
#32

Companies <- names(df_twitter)
pcorr.A <- matrix(0, length(Companies), length(Companies))
rownames(pcorr.A) <- Companies
colnames(pcorr.A) <- Companies
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")

#showing only the nodes of degree greater than zero; i.e. only nodes that are connected to at least one other node.
g.pcorr<- delete.vertices(g.pcorr, which(degree(g.pcorr)<1))

V(g.pcorr)
E(g.pcorr)

V(g.pcorr)$size =6
E(g.pcorr)$color = "purple"
plot(g.pcorr, vertex.label.dist=1,vertex.label.size=0.5, vertex.color="orange",
     vertex.label.color="darkblue",main='Partial Correlation with p<0.01',
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.1)


#Overall Correlation
pcorr <- cor(df_twitter)
z <- 0.5 * log((1 + pcorr) / (1 - pcorr))
z.vec <- z[upper.tri(z)]
n <- dim(df_twitter)[1]
corr.pvals <- 2*pnorm(abs(z.vec), 0, sqrt(1/(n-3)), lower.tail=F)
corr.pvals.adj <- p.adjust(corr.pvals, "BH")

#Comparing these values to a standard 0.05 significance level
corr.edges <- (corr.pvals.adj < 0.05)
length(corr.pvals.adj[corr.pvals.adj < 0.05])
#180

Companies <- names(df_twitter)
corr.A <- matrix(0, length(Companies), length(Companies))
rownames(corr.A) <- Companies
colnames(corr.A) <- Companies
corr.A[lower.tri(corr.A)] <- as.numeric(corr.edges)
g.corr <- graph.adjacency(corr.A, "undirected")

#showing only the nodes of degree greater than zero; i.e. only nodes that are connected to at least one other node.
g.corr<- delete.vertices(g.corr, which(degree(g.corr)<1))

V(g.corr) #89
E(g.corr) #180

V(g.corr)$size =4
E(g.corr)$color = "purple"
plot(g.corr, vertex.label.dist=1,vertex.label.size=0.5, vertex.color="orange",
     vertex.label.color="darkblue",main='Overall Correlation at p<0.05',
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.15)



#Comparing these values to a standard 0.05 significance level
corr.edges <- (corr.pvals.adj < 0.01)
length(corr.pvals.adj[corr.pvals.adj < 0.01])
#114


Companies <- names(df_twitter)
corr.A <- matrix(0, length(Companies), length(Companies))
rownames(corr.A) <- Companies
colnames(corr.A) <- Companies
corr.A[lower.tri(corr.A)] <- as.numeric(corr.edges)
g.corr <- graph.adjacency(corr.A, "undirected")

#showing only the nodes of degree greater than zero; i.e. only nodes that are connected to at least one other node.
g.corr<- delete.vertices(g.corr, which(degree(g.corr)<1))

V(g.corr)
E(g.corr)

V(g.corr)$size =4
E(g.corr)$color = "purple"
plot(g.corr, vertex.label.dist=1,vertex.label.size=0.5, vertex.color="orange",
     vertex.label.color="darkblue",main='Overall Correlation at p<0.01',
     edge.width = 1,edge.arrow.width = 0.3, edge.arrow.size = 0.5,vertex.size2 = 3,vertex.label.cex = .75,asp = 0.5,margin = -0.15)




