## DATA FROM MS 609 (BIBLIOTHEQUE DE TOULOUSE) COLLECTED BY JEAN-PAUL REHR
## Exploratory script
## R script written by Jose Luis Estevez (Masaryk University)
## Date: March 8th 2022
########################################################################################################################

# DATA LOADING
rm(list=ls())
# Required packages
library(stringr);library(igraph);library(ggplot2);library(ggrepel);library(ggpubr)
library(sf);library(rnaturalearth);library(rnaturalearthhires);library(ggspatial)
 
# I corrected 2 lines of the .txt: d'Arago for de~Arago (line 112), and mother of Saramunda del Mas (line 1120)
people <- read.table("MS609_named_entities/people.txt",header=FALSE,sep="~")
names(people) <- c('name','identifier','forename','namelink','surname','genname','rolename','gender','place_id','t')

people[people$place_id == 'Saint-Pons-de-Thomieres_HÃ©rault',]$place_id <- 'Saint-Pons-de-Thomieres_Herault' # correction

# I turned both è and é into e, and à to a in the .txt
places <- read.table("MS609_named_entities/places.txt",header=FALSE,sep="~")
names(places) <- c('place_id','place_type','placename','settlement','lat','long','t')

depositions <- read.table("MS609_named_entities/MS609-deposition.txt",header=FALSE,sep="~")
names(depositions) <- c('document_id','doc_stat','ordinal','folio','doc_type','group_name','dep_date_source','dep_date',
                        'deponent_pers_id','witness_source','t')

dep_event <- read.table("MS609_named_entities/MS609-deposition-event.txt",header=FALSE,sep="~")
names(dep_event) <- c('document_id','event_id','event_type','event_subtype','event_date','corresp_id','t')

dep_event[dep_event$event_id == 'MS609-0012-2',]$event_date <- 1243 # correction: 1243 instead of 1943

dep_event_people <- read.table("MS609_named_entities/MS609-deposition-event-people.txt",header=FALSE,sep="~")
names(dep_event_people) <- c('event_id','pers_id','role','t')

#dep_event_places <- read.table("MS609_named_entities/MS609-deposition-event-places.txt",header=FALSE,sep="~")
#names(dep_event_places) <- c('event_id','place_id','place_type','t')

# I had to remove the time stamps to load this file, that is why there are none
#dep_event_people_acts <- read.table("MS609_named_entities/MS609-deposition-event-people-acts.txt",header=FALSE,sep="~")
#names(dep_event_people_acts) <- c('event_id','pers_id','role','act')

########################################################################################################################

# DEPOSITIONS
# General inspection
depositions <- depositions[depositions$doc_type != 'Recitation',] # Exclude recitations which have no additional information
depositions <- depositions[depositions$doc_stat != 'exclude',] # Exclude one duplicated item
summary(factor(depositions$doc_stat)) # We have 684 full depositions, and one in two pieces (685 in total)
partial_dep <- depositions$document_id[depositions$doc_stat == 'partial']
depositions_ids <- depositions$document_id
length(unique(depositions$deponent_pers_id)) # 688 unique deponents
# Village where the deposition was obtained
depositions$village <- factor(ifelse(depositions$group_name %in% c('De_Manso_Sanctarum_Puellarum_1',
                                                                   'De_Manso_Sanctarum_Puellarum_2'),
                                     'Mas-Saintes-Puelles','Saint-Martin-Lalande'))
summary(depositions$village) # 425 deposition from Mas-Saintes-Puelles and 261 from Saint-Martin-Lalande
dep_le_mas <- depositions[depositions$village == 'Mas-Saintes-Puelles',]$document_id
dep_saint_martin <- depositions[depositions$village == 'Saint-Martin-Lalande',]$document_id

# Time of the depositions
depositions$dep_date <- as.Date(depositions$dep_date,format='%Y-%m-%d')
summary(depositions$dep_date) # First deposition from 12 May 1245, last deposition from 14 July 1246

########################################################################################################################

# EVENTS REFERRED IN THE DEPOSITIONS
# Exclusion of recitations
dep_event <- dep_event[(dep_event$document_id %in% depositions_ids),]
# Classification of events by type: denial, event, etc.
dep_event$event_subtype <- factor(dep_event$event_subtype)
summary(dep_event$event_subtype)

# Number of depositions that contain at least one event
no_events <- dep_event[dep_event$event_subtype != 'event',]
events <- dep_event[dep_event$event_subtype == 'event',]

# ... 500 only contains denials or beliefs, etc.
length(setdiff(unique(no_events$document_id),unique(events$document_id))) 
# ... 185 refer to at least one actual event
length(unique(events$document_id)) 
partial_dep %in% events$document_id # 186 (but there are two half depositions)

# 116 from Le Mas, 69 from Saint-Martin
length(unique(events[events$document_id %in% dep_le_mas,]$document_id)) 
partial_dep %in% events[events$document_id %in% dep_le_mas,]$document_id
length(unique(events[events$document_id %in% dep_saint_martin,]$document_id)) 

# Events date from 1185 to 1244
range(events$event_date,na.rm=TRUE)

########################################################################################################################

# DEPONENTS AND PEOPLE REFERRED IN THOSE EVENTS
# In this dataset, people roles are included: dep (deponent), par (participant), inf (infermus), her (hereticus), etc.
# First, I isolated the 668 deponents
deponents_ids <- unique(dep_event_people[dep_event_people$role == 'dep',]$pers_id)
deponents <- people[people$name %in% deponents_ids,]
unique(deponents$place_id) # These come from 5 different villages
summary(factor(deponents$place_id)) # Mostly from Le Mas and Saint-Martin though

# Now let's see people who are either inculpated for participating, being heretics, owners of the house, or sick
reportees <- dep_event_people[dep_event_people$role %in% c('par','her','own','inf'),]
# Let's isolate those who can be identified
reportees$identified <- 1*!(str_detect(reportees$pers_id,'unknown') | # not unknown
                              str_detect(reportees$pers_id,'not_named') | # not-named
                              str_detect(reportees$pers_id,'unnamed') | # unnamed
                              str_detect(reportees$pers_id,'unclear') | # unclear
                              str_detect(reportees$pers_id,'unrecalled') | # unrecalled
                              str_detect(reportees$pers_id,'not_recalled') |
                              str_detect(reportees$pers_id,'in_public')) # in public

targets_ids <- unique(reportees[reportees$identified == 1,]$pers_id) 
length(targets_ids) # 641 people are reported
targets <- people[people$name %in% targets_ids,]
unique(targets$place_id) # These come from 49 different villages
summary(factor(targets$place_id)) 

########################################################################################################################

# VILLAGES
# Targets distribution by village
villages <- as.data.frame(summary(factor(targets$place_id))[-1]) # remove when the place is empty
names(villages) <- 'targets'
villages$targets <- (villages$targets)
villages <- cbind(places[places$place_id %in% targets$place_id,],villages) # Obtaining the coordinates in a map

# Visualisation of number of targets per village
world <- ne_countries(scale = 'large', returnclass = "sf")
villages$placename[villages$placename %in% c('Lasbordes','Fendeille','Castelnaudary','Montferrand',
                                             'Villeneuve-la-Comptal')] <- NA # to allow visualisation

jpeg(filename='Map.jpeg',width=14,height=7,units='in',res=300)
ggplot(data = world) + 
  geom_sf(fill= 'antiquewhite') + 
  geom_point(data=villages,aes(x = long, y = lat, size = targets),alpha=.65,colour='firebrick') +
  geom_text_repel(data=villages,aes(x=long,y=lat,label = placename),size=3) +
  annotate(geom = 'text',x=3.6,y=42.9,label='Mediterranean\nSea',fontface='italic',color='grey22',size=6) + 
  annotation_scale(location = 'bl', width_hint = 0.5) + 
  coord_sf(xlim = c(.35,4.05), ylim = c(42.55,43.95), expand = FALSE) + 
  theme(panel.grid.major = element_line(color = gray(.75), linetype = 'dashed', size = 0.5), 
        panel.background = element_rect(fill = 'aliceblue')) +
  xlab('')+ylab('')+labs(size='Number of\ntargets')+ 
  scale_size_continuous(trans = 'sqrt')
dev.off()

villages$placename <- places[places$place_id %in% villages$place_id,]$placename

rm(world)

########################################################################################################################

# INCULPATIONS PER DEPONENT
reportees <- reportees[reportees$identified == 1,] # keep only those identifiable 
reportees$document_id <- str_sub(reportees$event_id,1,10) # add the document ID
# Joined deponents with their reportees
edge_list <- merge(reportees,depositions[,c('document_id','dep_date','deponent_pers_id','village')],
                   by='document_id',all.x=TRUE)

# At this stage let's correct the two partial depositions as only one
edge_list[edge_list$document_id %in% partial_dep,]
# correct date
edge_list[edge_list$document_id == partial_dep[1],]$dep_date <- depositions[depositions$document_id == partial_dep[2],]$dep_date
# change the ID too
edge_list[edge_list$document_id == partial_dep[1],]$document_id <- depositions[depositions$document_id == partial_dep[2],]$document_id

edge_list <- edge_list[edge_list$document_id %in% depositions_ids,] # Exclude redundant documents
edge_list <- edge_list[edge_list$deponent_pers_id != edge_list$pers_id,] # Remove self inculpations
edge_list <- edge_list[order(edge_list$dep_date),] # order by deposition date
edge_list <- edge_list[!duplicated(edge_list[,c('deponent_pers_id','pers_id')]),] # remove duplicates
# If the same deponent inculpates the same person, we keep only the first inculpation
edge_list <- edge_list[,c('deponent_pers_id','pers_id','dep_date','document_id','role','village')] # keep only key variables

# Convert in an igraph object
inculp_ntw <- graph_from_edgelist(as.matrix.data.frame(edge_list[,c('deponent_pers_id','pers_id')]),
                                  directed = TRUE)
# edge attributes (date of the deposition)
E(inculp_ntw)$dep_date <- as.character(edge_list$dep_date) # add date of the inculpation
# add deponents who did not inculpate anybodfy as isolates
'%!in%' <- function(x,y)!('%in%'(x,y))
add_nodes <- deponents_ids[deponents_ids %!in% V(inculp_ntw)$name] 
inculp_ntw <- add_vertices(inculp_ntw,length(add_nodes),attr=list(name=add_nodes))
# node attribute: If the person deposed or not
V(inculp_ntw)$deponent <- ifelse(V(inculp_ntw)$name %in% deponents_ids,'deponent','target')

# Visualisation of the network
inculp_layout <- layout_with_kk(inculp_ntw)

jpeg(filename='Network of inculpations.jpeg',width=12,height=12,units='in',res=300)
plot(inculp_ntw,
     vertex.label=NA,vertex.size=2,vertex.color=ifelse(V(inculp_ntw)$deponent == 'deponent','firebrick','dodgerblue'),
     edge.arrow.size=.2,edge.color=gray(0.35),edge.lty=1,
     layout=inculp_layout,
     main='Inculpations contained in Manuscript 609 (Bibliotheque de Toulouse)')
legend("bottomright",bty="o",legend=c('Deponent','Target'),fill=c('firebrick','dodgerblue'))
dev.off()

# Extraction of number of targets per deponent (and inculpations received)
inculp_by_deponent <- data.frame(name = V(inculp_ntw)$name,
                                 inculp_sent = degree(inculp_ntw,mode='out'), # inculpations sent
                                 inculp_rec = degree(inculp_ntw,mode='in')) # inculpations received
inculp_by_deponent <- inculp_by_deponent[inculp_by_deponent$name %in% deponents_ids,] # only deponents
deponents <- merge(deponents,inculp_by_deponent,by='name')

rm(inculp_by_deponent);rm(add_nodes)

summary(deponents$inculp_sent) # Number of inculpations per deponent

########################################################################################################################

# INCULPATIONS PER DEPOSITION (DOCUMENT)
inculp_doc <- merge(reportees,depositions[,c('document_id','dep_date','deponent_pers_id','village')],
                    by='document_id',all.x=TRUE)
# Let's correct again the partial deposition
inculp_doc[inculp_doc$document_id %in% partial_dep,]
inculp_doc[inculp_doc$document_id == partial_dep[1],]$dep_date <- depositions[depositions$document_id == partial_dep[2],]$dep_date
inculp_doc[inculp_doc$document_id == partial_dep[1],]$document_id <- depositions[depositions$document_id == partial_dep[2],]$document_id


inculp_doc <- inculp_doc[inculp_doc$document_id %in% depositions_ids,] # Exclude redundant documents
inculp_doc <- inculp_doc[inculp_doc$deponent_pers_id != inculp_doc$pers_id,] # Remove self inculpations
inculp_doc <- inculp_doc[order(inculp_doc$dep_date),] # order by deposition date
inculp_doc <- inculp_doc[!duplicated(inculp_doc[,c('document_id','pers_id')]),] # remove duplicates
inculp_doc <- inculp_doc[,c('deponent_pers_id','pers_id','dep_date','document_id','role','village')] # keep only key variables

# Let's create a bipartite network now
nodesSet1 <- unique(inculp_doc$document_id)
nodesSet2 <- unique(inculp_doc$pers_id)
edgeList <- inculp_doc[,c('document_id','pers_id')]

g <- graph.empty()
g <- add.vertices(g,nv=length(nodesSet1),attr=list(name=nodesSet1,
                                                   type=rep('deposition',length(nodesSet1))))
g <- add.vertices(g,nv=length(nodesSet2),attr=list(name=nodesSet2,
                                                   type=rep('target',length(nodesSet2))))
# we need to turn edgeList into a vector (and using names instead of indexes)
edgeListVec <- as.vector(t(as.matrix(data.frame(S1=edgeList$document_id,
                                                S2=edgeList$pers_id))))
g <- add.edges(g,edgeListVec)
is.bipartite(g)

# add depoisitions with no inculpations
add_deps <- depositions_ids[depositions_ids %!in% V(g)$name]
add_deps <- add_deps[add_deps != partial_dep[1]] # remove the partial deposition

g <- add_vertices(g,length(add_deps),
                  attr=list(name=add_deps,type='deposition'))

rm(nodesSet1);rm(nodesSet2);rm(edgeList);rm(edgeListVec);rm(add_deps)

# Visualisation of the network
inculp_doc_layout <- layout_with_kk(g)

jpeg(filename='Bipartite Network of inculpations.jpeg',width=12,height=12,units='in',res=300)
plot(g,
     vertex.label=NA,
     vertex.size=2,vertex.color=ifelse(V(g)$type == 'deposition','firebrick','dodgerblue'),
     vertex.shape=ifelse(V(g)$type == 'deposition','square','circle'),
     edge.arrow.size=0,edge.color=gray(0.35),edge.lty=1,
     layout=inculp_doc_layout,
     main='Inculpations contained in Manuscript 609 (Bibliotheque de Toulouse)')
legend("bottomright",bty="o",legend=c('Deposition','Target'),fill=c('firebrick','dodgerblue'))
dev.off()

# Extraction of number of targets per deposition
inculp_by_deposition <- data.frame(document_id=V(g)$name[V(g)$type == 'deposition'],
                                   inculp = as.vector(degree(g,mode='out'))[1:685])
depositions <- merge(depositions,inculp_by_deposition,by='document_id')
rm(inculp_by_deposition)

summary(depositions$inculp)

########################################################################################################################

# INCULPATIONS OVER TIME

# Let's obtain how many new inculpations by the day
inculp_time <- edge_list[!duplicated(edge_list[,c('pers_id')]),]
inculp_time <- data.frame(dep_date = as.Date(unique(inculp_time$dep_date)),
                          new_inculp = summary(as.factor(inculp_time$dep_date)))
# We have 41 different dates, but let's not forget about depositions with no targets
missing_days <- data.frame(dep_date = as.Date(unique(depositions$dep_date)[unique(depositions$dep_date) %!in% inculp_time$dep_date]),
                           new_inculp = 0)
inculp_time <- rbind(inculp_time,missing_days)
row.names(inculp_time) <- 1:nrow(inculp_time) # rownames changed

# Now let's create a data set to include all days between May 1245 and July 1246
range(inculp_time$dep_date)
temp_data <- data.frame(time=seq(as.Date('1245-05-01'),as.Date('1246-07-30'),by="days"),
                        new_inculp=0, # number of new targets
                        deposition=0) # whether there was at least one deposition that day
# And move our dates to this data set
for(i in inculp_time$dep_date){
  temp_data[temp_data$time == i,]$deposition <- 1
  temp_data[temp_data$time == i,]$new_inculp <- inculp_time[inculp_time$dep_date == i,]$new_inculp
}

# Cumulative sum of targets
temp_data$cum_inculp <- cumsum(temp_data$new_inculp)

rm(inculp_time)

# Visualisation
grid.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

p1 <- ggplot(data=depositions) +
  geom_point(aes(x=dep_date,y=inculp,color=village),size=2,alpha=.75) +
  xlab('')+ylab('Targets mentioned per deposition') +
  scale_color_manual(name='',values=c('sienna3','springgreen4')) +
  grid.background +
  theme(axis.text.x = element_text(angle=90, vjust =0.5, hjust=1)) +
  scale_x_continuous(breaks = as.Date(c('1245-05-01','1245-06-01','1245-07-01','1245-08-01','1245-09-01','1245-10-01',
                                        '1245-11-01','1245-12-01','1246-01-01','1246-02-01','1246-03-01','1246-04-01',
                                        '1246-05-01','1246-06-01','1246-07-01','1246-08-01'))) +
  theme(legend.position="top")

p2 <- ggplot() +
  geom_line(data=temp_data,aes(x=time,y=new_inculp),color='navyblue') +
  geom_line(data=temp_data,aes(x=time,y=cum_inculp),color='cyan4') +
  geom_point(data=temp_data[temp_data$deposition == 1,],aes(x=time,y=new_inculp),size=2,color='mediumblue',alpha=.5) +
  geom_point(data=temp_data[temp_data$deposition == 1,],aes(x=time,y=cum_inculp),size=2,color='lightblue4',alpha=.5) +
  xlab('')+ylab('New targets discovered per day') +
  grid.background +
  theme(axis.text.x = element_text(angle=90, vjust =0.5, hjust=1)) +
  scale_x_continuous(breaks = as.Date(c('1245-05-01','1245-06-01','1245-07-01','1245-08-01','1245-09-01','1245-10-01',
                                        '1245-11-01','1245-12-01','1246-01-01','1246-02-01','1246-03-01','1246-04-01',
                                        '1246-05-01','1246-06-01','1246-07-01','1246-08-01'))) 

jpeg(filename='Inculpations over time.jpeg',width=12,height=7,units='in',res=1000)
ggarrange(p1,p2,nrow=1,labels=c('',''))
dev.off()

########################################################################################################################

# Save image
save.image('Toulouse_data.RData')
