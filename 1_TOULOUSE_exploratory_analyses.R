## DATA FROM MS 609 (BIBLIOTHEQUE DE TOULOUSE) COLLECTED BY JEAN-PAUL REHR
## Exploratory script
## R script written by Jose Luis Estevez (Masaryk University)
## Date: March 15th 2022
########################################################################################################################

# DATA LOADING

rm(list=ls())
# Required packages
library(stringr);library(igraph);library(ggplot2);library(ggrepel);library(ggpubr);library(isnar)
library(sf);library(rnaturalearth);library(rnaturalearthhires);library(ggspatial)

# I corrected 2 lines of the .txt: d'Arago for de~Arago (line 112), and mother of Saramunda del Mas (line 1120)
people <- read.table("MS609_named_entities/people.txt",header=FALSE,sep="~")
names(people) <- c('name','identifier','forename','namelink','surname','genname','rolename','gender','place_id','t')

people[people$place_id == 'Saint-Pons-de-Thomieres_HÃ©rault',]$place_id <- 'Saint-Pons-de-Thomieres_Herault' # correction

# Some corrections
people[people$name == 'Peire_de_Rosengue_MSP-AU',]$gender <- 'male' # Peire is a male, not a female
people[people$surname == 'Meta nÃ©e del Mas',]$surname <- 'Meta'
people[people$surname == 'de Mont Server nÃ©e del Mas',]$surname <- 'Mont Server'
people[people$surname == 'MontrÃ©al',]$surname <- 'Montreal'
people[people$surname == 'Porquer nÃ©e Garric',]$surname <- 'Porquer'
people[people$surname == 'Quiders nÃ©e Laura',]$surname <- 'Quiders'
people[people$surname %in% c('','B','B.','F','R','W.'),]$surname <- NA

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

# DATA CORRECTIONS

# Let's remove individuals who are not identifiable
people$identified <- 1*!(str_detect(people$name,'unknown') | # not unknown
                           str_detect(people$name,'not_named') | # not-named
                           str_detect(people$name,'unnamed') | # unnamed
                           str_detect(people$name,'unclear') | # unclear
                           str_detect(people$name,'unrecalled') | # unrecalled
                           str_detect(people$name,'not_recalled') |
                           str_detect(people$name,'in_public'))
people <- people[people$identified == 1,]

# Let's do something similar with events
# First, let's only consider subjects reported in the roles of participant, heretics, owner or infirmus
reportees <- dep_event_people[dep_event_people$role %in% c('par','her','own','inf'),]
# Let's isolate those who can be identified
reportees$identified <- 1*!(str_detect(reportees$pers_id,'unknown') | # not unknown
                              str_detect(reportees$pers_id,'not_named') | # not-named
                              str_detect(reportees$pers_id,'unnamed') | # unnamed
                              str_detect(reportees$pers_id,'unclear') | # unclear
                              str_detect(reportees$pers_id,'unrecalled') | # unrecalled
                              str_detect(reportees$pers_id,'not_recalled') |
                              str_detect(reportees$pers_id,'in_public')) # in public
reportees <- reportees[reportees$identified == 1,]

# Now, let's subset only those subjects that are either deponents, or reported as participants, heretics, etc.
people <- people[people$name %in% c(unique(reportees$pers_id),depositions$deponent_pers_id),] # N = 1126 

# Let's find out if there are duplicated subjects
# First, let's create full-name labels
for(i in 1:nrow(people)){
  if(people$genname[i] == ''){
    people$fullname[i] <- paste(people$forename[i],people$surname[i])
  }else{
    # in case there junior and senior, e.g.
    people$fullname[i] <- paste(people$forename[i],people$surname[i],people$genname[i])
  }
}
people[people$fullname == ' Barona',]$fullname <- 'Barona' # correct the space

# If no names available, use the identifiers
people[people$fullname == ' NA',]$fullname <- people[people$fullname == ' NA',]$identifier

# Some family names can be inferred from family ties reported
people[people$name == 'Na_Arbona_SML-AU',]$surname <- 'Arbona'
people[people$name == 'Raimunda_mother_of_Flor_del_Mas_BPC-AU',]$surname <- 'Mas'
people[people$name == 'P_del_Mas_H',]$surname <- 'Mas'
people[people$name == 'wife_of_Guilhem_Arnald_SML-AU',]$surname <- 'Arnald'
people[people$name == 'Na_Camona_MSP-AU',]$surname <- 'Camona'
people[people$name == 'Dominic_de_Catalonia',]$surname <- 'Catalonia'
people[people$name == 'mother_of_Dominic_de_Catalonia',]$surname <- 'Catalonia'
people[people$name == 'Na_Comdors_Heuna_MSP-AU',]$surname <- 'Comdors'
people[people$name == 'Alamanda_mother_Amada_Fendelha_FDL-AU',]$surname <- 'Fendelha'
people[people$name == 'wife_of_Peire_Gari_SPdT-HE',]$surname <- 'Gari'
people[people$name == 'Garnier_Senior_MSP-AU',]$surname <- 'Garnier'
people[people$name == 'daughter_of_Melia_Johan_SML-AU',]$surname <- 'Johan'
people[people$name == 'Na_Laureta_MPX-AR',]$surname <- 'Laureta'
people[people$name == 'Guilhelma_sister_Arnald_Maiestre_MSP-AU',]$surname <- 'Maiestre'
#people[people$name == 'Raimunda_concubine_Maiestre_MSP-AU',]$surname <- 'Maiestre'
#people[people$name == 'Nauda_concubine_of_Raimund_dels_Alamans_SML-AU',]$surname <- 'Alamans'
people[people$name == 'mother_of_Saramunda_del_Mas_CMS-AU',]$surname <- 'Mas'
people[people$name == 'mother_of_Cerdana_de_Lalanda_SML-AU',]$surname <- 'Lalanda'

unique_names <- unique(people$fullname) # Probably 1,082 individuals, NOT 1,126

# Use the full names in the other data files to facilitate connection later
depositions <- merge(depositions,
                     people[,c('name','fullname')],by.x='deponent_pers_id',by.y='name',all.x=TRUE)
reportees <- merge(reportees,
                   people[,c('name','fullname')],by.x='pers_id',by.y='name',all.x=TRUE)
dep_event_people <- merge(dep_event_people, 
                          people[,c('name','fullname')],by.x='pers_id',by.y='name',all.x=TRUE)

# Let's remove the duplicates 
people <- merge(people,places[,c('place_id','placename')],by='place_id',all.x=TRUE) # first, let's get villages

people <- people[!duplicated(people[,c('fullname','placename')]),] # if full name and place_id are the same, we only keep one

# If we have the same name but two different places...
'%!in%' <- function(x,y)!('%in%'(x,y))
no_dup <- people[people$fullname %!in% people[duplicated(people[,'fullname']),]$fullname,]
yes_dup <- people[people$fullname %in% people[duplicated(people[,'fullname']),]$fullname,]
# In most cases, are individuals reported living both in Mas-Saintes-Puelles and Saint-Martin-Lalande
yes_dup[yes_dup$placename %!in% c('Mas-Saintes-Puelles','Saint-Martin-Lalande'),]
# This are just cases where one row is missing id_place 
yes_dup1 <- yes_dup[yes_dup$fullname %in% c('Bernarda','Bertrand Marti','Guilhem Vidal','P Bernard')
                    & yes_dup$placename %in% c('Mas-Saintes-Puelles','Saint-Martin-Lalande'),]
# This are both from the two different villages
yes_dup2 <- yes_dup[yes_dup$fullname %!in% c('Bernarda','Bertrand Marti','Guilhem Vidal','P Bernard'),]
# We pick one at random
set.seed(123)
yes_dup2 <- yes_dup2[sample(row.names(yes_dup2),replace=FALSE),]
yes_dup2 <- yes_dup2[!duplicated(yes_dup2[,c('fullname')]),]

people <- rbind(no_dup,yes_dup1,yes_dup2)

# Remove unnecessary objects
rm(yes_dup);rm(yes_dup1);rm(yes_dup2);rm(no_dup)

########################################################################################################################

# DEPOSITIONS

# General inspection
depositions <- depositions[depositions$doc_type != 'Recitation',] # Exclude recitations which have no additional information
depositions <- depositions[depositions$doc_stat != 'exclude',] # Exclude one duplicated item
summary(factor(depositions$doc_stat)) # We have 684 full depositions, and one in two pieces (685 in total)
partial_dep <- depositions$document_id[depositions$doc_stat == 'partial']
depositions_ids <- depositions$document_id
length(unique(depositions$fullname)) # 651 unique deponents
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

# One of the two partial depositions has the date missing, so I use the value of the other half
depositions[depositions$document_id == 'MS609-0612',]$dep_date <- depositions[depositions$document_id == 'MS609-0424',]$dep_date

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
# First, I isolated the 651 unique deponents
deponents_ids <- unique(depositions$fullname)
deponents <- people[people$fullname %in% deponents_ids,]
unique(deponents$placename) # These come from 5 different villages
summary(factor(deponents$placename)) # Mostly from Le Mas and Saint-Martin though

targets_ids <- unique(reportees[reportees$identified == 1,]$fullname) 
length(targets_ids) # 624 people are reported
targets <- people[people$fullname %in% targets_ids,]
unique(targets$placename) # These come from 49 different villages
summary(factor(targets$placename)) 

########################################################################################################################

# VILLAGES

# Targets distribution by village
villages <- as.data.frame(summary(factor(targets$placename))[c(-49)]) # remove when the place is empty
names(villages) <- 'targets'
villages$targets <- (villages$targets)
villages <- cbind(places[places$placename %in% targets$placename,],villages) # Obtaining the coordinates in a map

# Visualisation of number of targets per village
world <- ne_countries(scale = 'large', returnclass = "sf")
villages$placename[villages$placename %in% c('Lasbordes','Fendeille','Castelnaudary','Montferrand',
                                             'Villeneuve-la-Comptal')] <- NA # to allow visualisation

jpeg(filename='Map.jpeg',width=14,height=7,units='in',res=1000)
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

rm(world);rm(villages)

########################################################################################################################

# INCULPATIONS PER DEPONENT

reportees$document_id <- str_sub(reportees$event_id,1,10) # add the document ID
# Joined deponents with their reportees
edge_list <- merge(reportees[,c('document_id','fullname','role','event_id')],
                   depositions[,c('document_id','dep_date','fullname','village')],
                   by='document_id',all.x=TRUE)

names(edge_list)[2] <- 'pers_id' 
names(edge_list)[6] <- 'deponent_pers_id'

# At this stage let's address the two partial depositions as only one
edge_list[edge_list$document_id == partial_dep[2],]$document_id <- depositions[depositions$document_id == partial_dep[1],]$document_id

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
add_nodes <- deponents_ids[deponents_ids %!in% V(inculp_ntw)$name] 
inculp_ntw <- add_vertices(inculp_ntw,length(add_nodes),attr=list(name=add_nodes))
# Node attributes: If the person deposed or not
V(inculp_ntw)$deponent <- ifelse(V(inculp_ntw)$name %in% deponents_ids,'deponent','target')
# Gender
rownames(people) <- 1:nrow(people) 
people <- people[match(V(inculp_ntw)$name,people$fullname),] # same order of appearance
people$gender[people$gender %in% c('','unknown')] <- NA # people with unknown gender to NA
V(inculp_ntw)$gender <- people$gender
# Surname (for family ties)
V(inculp_ntw)$family <- people$surname
# Village
V(inculp_ntw)$village <- people$placename

# Visualisation of the network
set.seed(0708)
inculp_layout <- layout_with_kk(inculp_ntw)

# By deponent vs. non-deponent (target)
jpeg(filename='Network of inculpations (deponents).jpeg',width=12,height=12,units='in',res=1000)
plot(inculp_ntw,
     vertex.label=NA,vertex.size=2,vertex.color=ifelse(V(inculp_ntw)$deponent == 'deponent','firebrick','dodgerblue'),
     edge.arrow.size=.2,edge.color=gray(0.35),edge.lty=1,
     layout=inculp_layout,
     main='Inculpations contained in Manuscript 609 (Bibliotheque de Toulouse)')
legend("bottomright",bty="o",legend=c('Deponent','Target'),fill=c('firebrick','dodgerblue'))
dev.off()

# By gender
jpeg(filename='Network of inculpations (gender).jpeg',width=12,height=12,units='in',res=1000)
plot(inculp_ntw,
     vertex.label=NA,vertex.size=2,vertex.color=ifelse(V(inculp_ntw)$gender == 'male','royalblue','magenta'),
     edge.arrow.size=.2,edge.color=gray(0.35),edge.lty=1,
     layout=inculp_layout,
     main='Inculpations contained in Manuscript 609 (Bibliotheque de Toulouse)')
legend("bottomright",bty="o",legend=c('Male','Female'),fill=c('royalblue','magenta'))
dev.off()

# By village
jpeg(filename='Network of inculpations (village).jpeg',width=12,height=12,units='in',res=1000)
plot(inculp_ntw,
     vertex.label=NA,vertex.size=2,
     vertex.color=ifelse(V(inculp_ntw)$village == 'Mas-Saintes-Puelles','sienna3',
                         ifelse(V(inculp_ntw)$village == 'Saint-Martin-Lalande','springgreen4',
                                ifelse(V(inculp_ntw)$village == 'Laurac','deeppink','gold'))),
     edge.arrow.size=.2,edge.color=gray(0.35),edge.lty=1,
     layout=inculp_layout,
     main='Inculpations contained in Manuscript 609 (Bibliotheque de Toulouse)')
legend("bottomright",bty="o",legend=c('Mas-Saintes-Puelles','Saint-Martin-Lalande','Laurac','Somewhere else'),
       fill=c('sienna3','springgreen4','deeppink','gold'))
dev.off()

# Extraction of number of targets per deponent (and inculpations received)
inculp_by_deponent <- data.frame(fullname = V(inculp_ntw)$name,
                                 inculp_sent = degree(inculp_ntw,mode='out'), # inculpations sent
                                 inculp_rec = degree(inculp_ntw,mode='in')) # inculpations received
inculp_by_deponent <- inculp_by_deponent[inculp_by_deponent$fullname %in% deponents_ids,] # only deponents
deponents <- merge(deponents,inculp_by_deponent,by='fullname')

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

# add depositions with no inculpations
add_deps <- depositions_ids[depositions_ids %!in% V(g)$name]
add_deps <- add_deps[add_deps != partial_dep[1]] # remove the partial deposition

g <- add_vertices(g,length(add_deps),
                  attr=list(name=add_deps,type='deposition'))

rm(nodesSet1);rm(nodesSet2);rm(edgeList);rm(edgeListVec);rm(add_deps)

# Visualisation of the network
plot(g,
     vertex.label=NA,
     vertex.size=2,vertex.color=ifelse(V(g)$type == 'deposition','firebrick','dodgerblue'),
     vertex.shape=ifelse(V(g)$type == 'deposition','square','circle'),
     edge.arrow.size=0,edge.color=gray(0.35),edge.lty=1,
     layout=layout_with_kk(g),
     main='Inculpations contained in Manuscript 609 (Bibliotheque de Toulouse)')
legend("bottomright",bty="o",legend=c('Deposition','Target'),fill=c('firebrick','dodgerblue'))

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
missing_days <- data.frame(dep_date = as.Date(unique(depositions[!is.na(depositions$dep_date),]$dep_date)
                                              [unique(depositions[!is.na(depositions$dep_date),]$dep_date) %!in% 
                                                  inculp_time$dep_date]),
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

rm(inculp_time);rm(p1);rm(p2)

########################################################################################################################

# EVOLUTION OF THE INCULPATIONS OVER TIME 

# Let's make snapshot of the network based on targets identified by day
key_dates <- as.Date(unique(E(inculp_ntw)$dep_date))

snapshot_ntw <- list()
for(i in seq_along(key_dates)){
  snapshot_ntw[[i]] <- delete_edges(inculp_ntw,which(as.Date(E(inculp_ntw)$dep_date) > key_dates[[i]]))
}
names(snapshot_ntw) <- as.character(key_dates) # add names with the date

# Let's choose the 9 dates with the highest number of new inculpations
dates_to_plot <- temp_data[temp_data$new_inculp > 20,]$time

# Visualisation
jpeg(filename='Evolution.jpeg',width=18,height=18,units='in',res=1000)
par(mfrow=c(3,3)) # A 3 by 3 figure
for(i in match(as.character(dates_to_plot),as.character(names(snapshot_ntw)))){
  plot(snapshot_ntw[[i]],
       vertex.label=NA,vertex.size=2,
       vertex.color=ifelse(degree(snapshot_ntw[[i]],mode='total') == 0,grey(0.5,0.2),
                           ifelse(V(snapshot_ntw[[i]])$village == 'Mas-Saintes-Puelles','sienna3',
                                  ifelse(V(snapshot_ntw[[i]])$village == 'Saint-Martin-Lalande','springgreen4',
                                         ifelse(V(snapshot_ntw[[i]])$village == 'Laurac','deeppink','gold')))),
       vertex.frame.color=ifelse(degree(snapshot_ntw[[i]],mode='total') == 0,grey(0,0.2),'black'),
       edge.width=.5,edge.arrow.size=.15,edge.lty=1,
       edge.color= ifelse(E(snapshot_ntw[[i]])$dep_date != key_dates[i],gray(0.15),'red'),
       layout=inculp_layout,
       main=paste('Inculpations by',names(snapshot_ntw)[[i]]),sep=' ')
}
legend("bottomright",bty="o",legend=c('Mas-Saintes-Puelles','Saint-Martin-Lalande','Laurac','Somewhere else'),
       fill=c('sienna3','springgreen4','deeppink','gold'))
dev.off()

rm(dates_to_plot);rm(key_dates);rm(i)

########################################################################################################################

# CREATION OF A NETWORK: PRESENCE IN THE SAME EVENT

event_person <- reportees

# Let's create a bipartite network now
nodesSet1 <- unique(event_person$event_id)
nodesSet2 <- unique(event_person$fullname)
edgeList <- event_person

g <- graph.empty()
g <- add.vertices(g,nv=length(nodesSet1),attr=list(name=nodesSet1,
                                                   type=rep('event',length(nodesSet1))))
g <- add.vertices(g,nv=length(nodesSet2),attr=list(name=nodesSet2,
                                                   type=rep('person',length(nodesSet2))))
# we need to turn edgeList into a vector (and using names instead of indexes)
edgeListVec <- as.vector(t(as.matrix(data.frame(S1=edgeList$event_id,
                                                S2=edgeList$fullname))))
g <- add.edges(g,edgeListVec)
is.bipartite(g)
# Any individual that is not in the inculpation network?
nodesSet2[!(V(g)$name[V(g)$type == 'person'] %in% unique_names)]

# add people
add_nodes <- unique_names[unique_names %!in% nodesSet2]
g <- add_vertices(g,length(add_nodes),
                  attr=list(name=add_nodes,type='person'))

# Make undirected
g <- as.undirected(g,mode='collapse')

# Visualisation
jpeg(filename='Network event-person (bipartite).jpeg',width=12,height=12,units='in',res=1000)
plot(g,
     vertex.label=NA,
     vertex.size=2,vertex.color=ifelse(V(g)$type == 'event','firebrick','dodgerblue'),
     vertex.shape=ifelse(V(g)$type == 'event','square','circle'),
     edge.arrow.size=0,edge.color=gray(0.35),edge.lty=1,
     layout=layout_with_kk(g),
     main='Bipartite network (event-person) contained in Manuscript 609 (Bibliotheque de Toulouse)')
legend("bottomright",bty="o",legend=c('Event','Person'),fill=c('firebrick','dodgerblue'))
dev.off()

# Let's get the tie-by-event network
event_person_ntw <- as.matrix(get.adjacency(g))
# People to rows, events to columns
event_person_ntw <- event_person_ntw[rownames(event_person_ntw) %in% V(g)$name[V(g)$type == 'person'],]
event_person_ntw <- event_person_ntw[,colnames(event_person_ntw) %in% V(g)$name[V(g)$type == 'event']]
# Matrix times its transposed
ties_event <- event_person_ntw %*% t(event_person_ntw)

# Let's put the nodes in the same order than in the inculpation network
ties_event <- ties_event[match(V(inculp_ntw)$name,rownames(ties_event)),
                         match(V(inculp_ntw)$name,colnames(ties_event))]
# Let's dichotomise
ties_event[ties_event > 0] <- 1

# And convert into an igraph object
ties_event_graph <- graph_from_adjacency_matrix(ties_event,mode='undirected',diag=FALSE)
# Addition of node-level attributes
V(ties_event_graph)$deponent <- V(inculp_ntw)$deponent
V(ties_event_graph)$gender <- V(inculp_ntw)$gender
V(ties_event_graph)$family <- V(inculp_ntw)$family
V(ties_event_graph)$village <- V(inculp_ntw)$village

# Visualisation
jpeg(filename='Network of ties by event (village).jpeg',width=12,height=12,units='in',res=1000)
plot(ties_event_graph,
     vertex.label=NA,vertex.size=2,
     vertex.color=ifelse(V(ties_event_graph)$village == 'Mas-Saintes-Puelles','sienna3',
                         ifelse(V(ties_event_graph)$village == 'Saint-Martin-Lalande','springgreen4',
                                ifelse(V(ties_event_graph)$village == 'Laurac','deeppink','gold'))),
     edge.arrow.size=.2,edge.color=gray(0.35),edge.lty=1,
     layout=layout_with_kk(ties_event_graph),
     main='Ties from shared events contained in Manuscript 609 (Bibliotheque de Toulouse)')
legend("bottomright",bty="o",legend=c('Mas-Saintes-Puelles','Saint-Martin-Lalande','Laurac','Somewhere else'),
       fill=c('sienna3','springgreen4','deeppink','gold'))
dev.off()

rm(event_person);rm(event_person_ntw);rm(nodesSet1);rm(nodesSet2);rm(edgeListVec);rm(g);rm(add_nodes)

########################################################################################################################

# CROSS-SECTIONAL DESCRIPTION OF THE INCULPATION NETWORK

# Summary table
inculp_desc <- data.frame(stats=c('N','inculpations','components (n > 1)','largest component','isolates','density','ave degree',
                                  'sd (out-degree)','sd (in-degree)','recip','recip (deponents only)','trans',
                                  'trans (deponents only)','degree centr','diameter','ave path length',
                                  'EI (gender)','gender missing','EI (family name)','family name missing',
                                  'EI (village)','village missing'),
                          value=NA)

# Nodes and ties
inculp_desc[1,'value'] <- vcount(inculp_ntw) 
inculp_desc[2,'value'] <- ecount(inculp_ntw) 
# Components and isolates
inculp_components <- decompose(inculp_ntw)
inculp_desc[3,'value'] <- sum(sapply(inculp_components,vcount) > 1)
inculp_desc[4,'value'] <- max(sapply(inculp_components,vcount))
inculp_desc[5,'value'] <- sum(sapply(inculp_components,vcount) == 1)
# Density and degree
inculp_desc[6,'value'] <- round(edge_density(inculp_ntw),3)
inculp_desc[7,'value'] <- round(mean(degree(inculp_ntw,mode='out')),3)
inculp_desc[8,'value'] <- round(sd(degree(inculp_ntw,mode='out')),3)
inculp_desc[9,'value'] <- round(sd(degree(inculp_ntw,mode='in')),3)

ggplot()+
  geom_histogram(aes(x=degree(inculp_ntw,mode='out')),
                 bins=(1+max(degree(inculp_ntw,mode='out'))),colour='black',fill='dodgerblue')+
  xlab('Inculpations reported') + ylab('Count (log10 scale)') + 
  xlim(c(-1,(1+max(degree(inculp_ntw,mode='out'))))) +
  scale_y_log10() +
  grid.background

ggplot()+
  geom_histogram(aes(x=degree(inculp_ntw,mode='in')),
                 bins=(1+max(degree(inculp_ntw,mode='in'))),colour='black',fill='tomato')+
  xlab('Inculpations received') + ylab('Count (log10 scale)') + 
  xlim(c(-1,(1+max(degree(inculp_ntw,mode='out'))))) +
  scale_y_log10() +
  grid.background

# Reciprocity and transitivity
inculp_desc[10,'value'] <- round(reciprocity(inculp_ntw),3)
inculp_desc[11,'value'] <- round(reciprocity(delete.vertices(inculp_ntw,which(V(inculp_ntw)$name %!in% deponents_ids))),3)
inculp_desc[12,'value'] <- round(transitivity(inculp_ntw),3)
inculp_desc[13,'value'] <- round(transitivity(delete.vertices(inculp_ntw,which(V(inculp_ntw)$name %!in% deponents_ids))),3)
#  Degree centralisation, diameter, ave. path length
inculp_desc[14,'value'] <- round(centr_degree(inculp_ntw,mode='total')$centralization,3)
inculp_desc[15,'value'] <- diameter(inculp_ntw,directed = TRUE)
inculp_desc[16,'value'] <- round(average.path.length(inculp_ntw),3)
# EI index: remember, -1 means perfect homophily, and 1 perfect heterophily 
inculp_desc[17,'value'] <- round(ei(inculp_ntw,'gender'),3) 
inculp_desc[18,'value'] <- sum(is.na(V(inculp_ntw)$gender)) # gender missing
inculp_desc[19,'value'] <- round(ei(inculp_ntw,'family'),3)
inculp_desc[20,'value'] <- sum(is.na(V(inculp_ntw)$family)) # family name missing
inculp_desc[21,'value'] <- round(ei(inculp_ntw,'village'),3) 
inculp_desc[22,'value'] <- sum(is.na(V(inculp_ntw)$village)) # village missing

inculp_desc # Summary table

# Individuals for which we know gender, surname, and village
vcount(inculp_ntw) - sum(is.na(V(inculp_ntw)$gender) | is.na(V(inculp_ntw)$family) | is.na(V(inculp_ntw)$village))

########################################################################################################################

# Remove unnecessary objects
rm(deponents);rm(edge_list);rm(edgeList);rm(events);rm(inculp_components);rm(grid.background);rm(inculp_doc)
rm(inculp_layout);rm(missing_days);rm(no_events);rm(places);rm(reportees);rm(targets);rm(temp_data);rm(ties_event)
rm(dep_le_mas);rm(dep_saint_martin);rm(depositions_ids);rm(unique_names);rm(dep_event)

# Save image
save.image('Toulouse_data.RData')