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

# Kinship relationships
kinship <- kinship[!duplicated(kinship[,c('actor1','actor2')]),] # Remove duplicated
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

jpeg(filename='Families.jpeg',width=30,height=30,units='in',res=500)
plot(family_graph,
     edge.color=ifelse(E(family_graph)$kind %in% c('parent-child','parent-child (illegitimate)'),'firebrick3', # parents in red
                       ifelse(E(family_graph)$kind == 'spouses','dodgerblue', # marriages in blue
                              ifelse(E(family_graph)$kind %in% c('siblings','siblings (illegitimate)'),'springgreen4', # siblings in green
                                     ifelse(E(family_graph)$kind == 'concubine','violet',grey(.6))))), 
     edge.width=5,
     edge.lty=ifelse(E(family_graph)$kind %in% c('concubine','parent-child (illegitimate)','siblings (illegitimate)','unspecified'),3,1),
     vertex.size=2,
     vertex.label=NA,
     vertex.color=ifelse(V(family_graph)$deceased == 'yes',grey(0.5,0.2),'goldenrod3'),
     vertex.frame.color=ifelse(V(family_graph)$deceased == 'yes',grey(0,0.2),'black'),
     layout=kinship_layout)

legend("bottomleft", legend = c('Alive','Deceased'), pch=21,
       col=c("black",grey(0,0.2)), pt.bg=c("goldenrod3",grey(0.5,0.2)),
       pt.cex=4, cex=3, bty="o", ncol=1)
legend("bottomright", legend = c('Siblings','Half siblings','Parent-child','Spouses','Parent-child (illegitimate)','Concubine','Other','Unclear'),
       col=c('springgreen4','springgreen4','firebrick3','dodgerblue','firebrick3','violet',grey(.6),grey(.6)),
       lty=c(1,3,1,1,3,3,1,3), 
       lwd=5,cex=2.5, bty="o", ncol=1)
dev.off()

########################################################################################################################

