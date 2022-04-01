## DATA FROM MS 609 (BIBLIOTHEQUE DE TOULOUSE) COLLECTED BY JEAN-PAUL REHR
## Network analyses
## R script written by Jose Luis Estevez (Masaryk University)
## Date: March 30th 2022
########################################################################################################################

# DATA LOADING
rm(list=ls())
load('Toulouse_data.RData')
deceased <- readxl::read_excel('MS609_extrainfo.xlsx',sheet='deceased')
kinship <- readxl::read_excel('MS609_extrainfo.xlsx',sheet='kinship')

# Required packages
library(igraph)

########################################################################################################################

# KINSHIP TIES

kinship <- kinship[!duplicated(kinship[,c('actor1','actor2')]),] # Remove duplicated
kinship <- kinship[kinship$tie %!in% c('unspecified'),] # remove unspecified ties
family_graph <- graph_from_edgelist(as.matrix(kinship[,c('actor1','actor2')]),directed = FALSE)
E(family_graph)$kind <- kinship$tie # type of kinship tie
is.simple(family_graph) # No multiple ties?

# Add who is dead
deceased <- unique(deceased$person)
deceased <- deceased[deceased %!in% deponents_ids] # if they deposed, they were alive still
V(family_graph)$deceased <- ifelse(V(family_graph)$name %in% deceased,'yes','no')

# Visualisation
set.seed(0708)
kinship_layout <- layout_nicely(family_graph)

jpeg(filename='Kinship ties (reported).jpeg',width=20,height=20,units='in',res=500)
plot(family_graph,
     edge.color=ifelse(E(family_graph)$kind %in% c('parent-child','parent-child (illegitimate)'),'firebrick3', # parents in red
                       ifelse(E(family_graph)$kind %in% c('spouses','former spouses'),'dodgerblue', # marriages in blue
                              ifelse(E(family_graph)$kind %in% c('siblings','siblings (illegitimate)'),'springgreen4', # siblings in green
                                     ifelse(E(family_graph)$kind == 'concubine','violet',grey(.6))))), 
     edge.width=5,
     edge.lty=ifelse(E(family_graph)$kind %in% c('siblings','parent-child','spouses'),1,3),
     vertex.size=2,
     vertex.label=NA,
     vertex.color=ifelse(V(family_graph)$deceased == 'yes',grey(0.5,0.2),'goldenrod3'),
     vertex.frame.color=ifelse(V(family_graph)$deceased == 'yes',grey(0,0.2),'black'),
     layout=kinship_layout)

legend("bottomleft", legend = c('Alive','Deceased'), pch=21,
       col=c("black",grey(0,0.2)), pt.bg=c("goldenrod3",grey(0.5,0.2)),
       pt.cex=3, cex=1.5, bty="o", ncol=1)
legend("bottomright", legend = c('Siblings','Parent-child','Marriage',
                                 'Half siblings','Parent-child (illegitimate)','Previous marriage','Concubine','Other'),
       col=c('springgreen4','firebrick3','dodgerblue',
             'springgreen4','firebrick3','dodgerblue','violet',grey(.6)),
       lty=c(1,1,1,3,3,3,3,3), 
       lwd=5,cex=1.5, bty="o", ncol=1)
dev.off()

########################################################################################################################

# Let's find extra ties that can be inferred from the existing ones

# 1) Siblings from shared parent(s): t(pc(i,j)) %*% pc(i,j) 
parentchild <- graph_from_edgelist(as.matrix(kinship[kinship$tie == 'parent-child',c('actor1','actor2')]),directed = TRUE)
# And add the missing nodes
extra_nodes <- V(family_graph)$name[V(family_graph)$name %!in% V(parentchild)$name]
parentchild <- add.vertices(parentchild,nv=length(extra_nodes),attr=list(name=extra_nodes))

PC <- as.matrix(get.adjacency(parentchild))
S <- t(PC) %*% PC
diag(S) <- 0
S[S!=0] <- 1 # remove multiplex ties (in case they shared both parents == 2)
siblings <- graph_from_adjacency_matrix(S,mode='undirected')

# Siblings from shared siblings: sib(i,j) %*% sib(i,j)
# Let's put together the inferred sibling ties and those reported explicitly
siblings2 <- graph_from_edgelist(as.matrix(kinship[kinship$tie == 'siblings',c('actor1','actor2')]),directed = FALSE)
# And add the missing nodes
extra_nodes <- V(family_graph)$name[V(family_graph)$name %!in% V(siblings2)$name]
siblings2 <- add.vertices(siblings2,nv=length(extra_nodes),attr=list(name=extra_nodes))

S2 <- as.matrix(get.adjacency(siblings2))
S2 <- S2[match(rownames(S),rownames(S2)),match(rownames(S),rownames(S2))] # same order than siblings 1

S2 <- S + S2
S2[S2 != 0] <- 1

# Now let's connect to siblings in second degree (sibling's siblings)
S3 <- S2 %*% S2
S3 <- S3 + S2
diag(S3) <- 0
S3[S3 != 0] <- 1 # remove multiplex ties
siblings3 <- graph_from_adjacency_matrix(S3,mode='undirected')

rm(S);rm(siblings);rm(S2);rm(siblings2)

########################################################################################################################

# Let's join the original ties and these inferred ones
family_graph <- family_graph %u% siblings3
E(family_graph)$kind[is.na(E(family_graph)$kind)] <- 'siblings'

########################################################################################################################

# New visualisation

jpeg(filename='Kinship ties (reported and inferred).jpeg',width=20,height=20,units='in',res=500)
plot(family_graph,
     edge.color=ifelse(E(family_graph)$kind %in% c('parent-child','parent-child (illegitimate)'),'firebrick3', # parents in red
                       ifelse(E(family_graph)$kind %in% c('spouses','former spouses'),'dodgerblue', # marriages in blue
                              ifelse(E(family_graph)$kind %in% c('siblings','siblings (illegitimate)'),'springgreen4', # siblings in green
                                     ifelse(E(family_graph)$kind == 'concubine','violet',grey(.6))))), 
     edge.width=5,
     edge.lty=ifelse(E(family_graph)$kind %in% c('siblings','parent-child','spouses'),1,3),
     vertex.size=2,
     vertex.label=NA,
     vertex.color=ifelse(V(family_graph)$deceased == 'yes',grey(0.5,0.2),'goldenrod3'),
     vertex.frame.color=ifelse(V(family_graph)$deceased == 'yes',grey(0,0.2),'black'),
     layout=kinship_layout)

legend("bottomleft", legend = c('Alive','Deceased'), pch=21,
       col=c("black",grey(0,0.2)), pt.bg=c("goldenrod3",grey(0.5,0.2)),
       pt.cex=3, cex=1.5, bty="o", ncol=1)
legend("bottomright", legend = c('Siblings','Parent-child','Marriage',
                                 'Half siblings','Parent-child (illegitimate)','Previous marriage','Concubine','Other'),
       col=c('springgreen4','firebrick3','dodgerblue',
             'springgreen4','firebrick3','dodgerblue','violet',grey(.6)),
       lty=c(1,1,1,3,3,3,3,3), 
       lwd=5,cex=1.5, bty="o", ncol=1)
dev.off()



