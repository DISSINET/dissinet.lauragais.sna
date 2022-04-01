## DATA FROM MS 609 (BIBLIOTHEQUE DE TOULOUSE) COLLECTED BY JEAN-PAUL REHR
## Exploratory script
## R script written by Jose Luis Estevez (Masaryk University)
## Date: March 30th 2022
########################################################################################################################

# DATA LOADING

rm(list=ls())
# Required packages
library(stringr);library(igraph);library(ggplot2);library(ggrepel);library(ggpubr);library(isnar)
library(sf);library(rnaturalearth);library(rnaturalearthhires);library(ggspatial)

# I corrected 2 lines of the .txt: d'Arago for de~Arago (line 112), and mother of Saramunda del Mas (line 1120)
people <- read.table("MS609_named_entities/people.txt",header=FALSE,sep="~")
names(people) <- c('name','identifier','forename','namelink','surname','genname','rolename','gender','place_id','t')

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

########################################################################################################################

# DATA CORRECTIONS

# Some corrections including French accents
people[people$place_id == 'Saint-Pons-de-Thomieres_HÃ©rault',]$place_id <- 'Saint-Pons-de-Thomieres_Herault' # correction
people[people$surname == 'Meta nÃ©e del Mas',]$surname <- 'Meta'
people[people$surname == 'de Mont Server nÃ©e del Mas',]$surname <- 'Mont Server'
people[people$surname == 'MontrÃ©al',]$surname <- 'Montreal'
people[people$surname == 'Porquer nÃ©e Garric',]$surname <- 'Porquer'
people[people$surname == 'Quiders nÃ©e Laura',]$surname <- 'Quiders'
people[people$surname == 'Amaniels',]$surname <- 'Amanielhs' #there is a missing H
people[people$surname %in% c('','B','B.','F','R','W.'),]$surname <- NA # Missing surnames to NA

# Correction of gender
# Peire is male
people[people$name == 'Peire_de_Rosengue_MSP-AU',]$gender <- 'male' 
people[people$name == 'Peire_Mamiro_MSP-AU',]$gender <- 'male'
people[people$name == 'Peire_de_Milhar_MSP-AU',]$gender <- 'male'
# Raimund is male
people[people$name == 'Raimund_de_Ponte_MSP-AU',]$forename <- 'Raimund'
people[people$name == 'Raimund_de_Ponte_MSP-AU',]$gender <- 'male'
# Raimunda is female
people[people$name == 'Raimunda_de_Morer_MSP-AU',]$gender <- 'female' 
people[people$name == 'Raimunda_Johan_de_Verazilh_SML-AU',]$gender <- 'female'
people[people$name == 'Raimunda_Mundissa_MSP-AU',]$gender <- 'female'
people[people$name == 'Raimunda_Vadis_MSP-AU',]$gender <- 'female'

# Bernard de Saint-Julian is Bernard de Saint-Julia (duplicated person)
people[people$name == 'Bernard_de_Saint-Julian_LRC-AU',]$name <- 'Bernard_de_Saint-Julia_StJU-HG'
people[people$name == 'Bernard_de_Saint-Julia_StJU-HG',]$surname <- 'Saint-Julia'
people[people$name == 'Bernard_de_Saint-Julia_StJU-HG',]$place_id <- 'Saint-Julia_Haute-Garonne'
dep_event_people[dep_event_people$pers_id == 'Bernard_de_Saint-Julian_LRC-AU',]$pers_id <- 'Bernard_de_Saint-Julia_StJU-HG'

# W de na Vierna is Guilhem de na Vierna
people[people$name == 'W_de_Na_Vierna_MSP-AU',]$name <- 'Guilhem_de_Na_Vierna_MSP-AU'
people[people$name == 'Guilhem_de_Na_Vierna_MSP-AU',]$gender <- 'male'
dep_event_people[dep_event_people$pers_id == 'W_de_Na_Vierna_MSP-AU',]$pers_id <- 'Guilhem_de_Na_Vierna_MSP-AU' 

# Raimund Vilandi, Raimund Vilandina, and Raimund Viliari are the same person
people[people$name == 'Raimund_Viliari_SML-AU',]$name <- 'Raimund_Vilandina_SML-AU'
people[people$name == 'Raimund_Vilandi_SML-AU',]$name <- 'Raimund_Vilandina_SML-AU'
depositions[depositions$deponent_pers_id == 'Raimund_Vilandi_SML-AU',]$deponent_pers_id  <- 'Raimund_Vilandina_SML-AU'
dep_event_people[dep_event_people$pers_id == 'Raimund_Viliari_SML-AU',]$pers_id  <- 'Raimund_Vilandina_SML-AU'
dep_event_people[dep_event_people$pers_id == 'Raimund_Vilandi_SML-AU',]$pers_id  <- 'Raimund_Vilandina_SML-AU'

# Raimund dels Alaman is Raimund Alaman (duplicated person)
dep_event_people[dep_event_people$pers_id == 'Raimund_dels_Alamans_MSP-AU',]$pers_id <- 'Raimund_Alaman_MSP-AU'
people[people$name == 'Raimund_dels_Alamans_MSP-AU',]$name <- 'Raimund_Alaman_MSP-AU'

# There are two different Raimunda Gastanha (need of a new ID)
depositions[depositions$deponent_pers_id == 'Raimunda_Gastanh_MSP-AU' &
              depositions$document_id == 'MS609-0390',]$deponent_pers_id <- 'Raimunda_Gastanh_2_MSP-AU' # this ID is new
dep_event_people[dep_event_people$event_id == 'MS609-0390-1' & 
                   dep_event_people$pers_id == 'Raimunda_Gastanh_MSP-AU',]$pers_id <- 'Raimunda_Gastanh_2_MSP-AU'
people[nrow(people)+1,] <- people[people$name == 'Raimunda_Gastanh_MSP-AU',]
people[nrow(people),]$name <- 'Raimunda_Gastanh_2_MSP-AU'
  
# Raimund Aleman Junior is also Raimund Alaman
people[people$name == 'Raimund_Aleman_Junior_MSP-AU',]$name <- 'Raimund_Alaman_MSP-AU'
dep_event_people[dep_event_people$pers_id == 'Raimund_Aleman_Junior_MSP-AU',]$pers_id <- 'Raimund_Alaman_MSP-AU'

# This is the same Esteve Faure
people[people$name == 'Esteve_Faure_2_SML-AU',]$name <- 'Esteve_Faure_SML-AU'
dep_event_people[dep_event_people$event_id == 'MS609-3737-1' &
                   dep_event_people$pers_id == 'Esteve_Faure_2_SML-AU',]$pers_id <- 'Esteve_Faure_SML-AU'

# And this is the same Peire Folc
people[people$name == 'Peire_Folc_2_SML-AU',]$name <- 'Peire_Folc_SML-AU'
dep_event_people[dep_event_people$event_id == 'MS609-3746-1' &
                   dep_event_people$pers_id == 'Peire_Folc_2_SML-AU',]$pers_id <- 'Peire_Folc_SML-AU'

# Bernard 'place holder' Mir is Bernard Mir junior
people[people$name == 'Bernard_place_holder_Mir_SML-AU',]$name <- 'Bernard_Mir_Junior_SML-AU'
depositions[depositions$deponent_pers_id == 'Bernard_place_holder_Mir_SML-AU',]$deponent_pers_id <- 'Bernard_Mir_Junior_SML-AU'
dep_event_people[dep_event_people$pers_id == 'Bernard_place_holder_Mir_SML-AU',]$pers_id <- 'Bernard_Mir_Junior_SML-AU'
# The person referred here is Bernard Mir junior, not Senior
dep_event_people[dep_event_people$event_id == 'MS609-0585-2' & 
                   dep_event_people$pers_id == 'Bernard_Mir_SML-AU',]$pers_id <- 'Bernard_Mir_Junior_SML-AU'

# The B Vezia referred by Peirona and Romana is not B Vezia but probably B Veziat
dep_event_people[dep_event_people$event_id == 'MS609-0253-1' & 
                   dep_event_people$pers_id == 'B_Vezia_MSP-AU',]$pers_id <- 'B_Veziat_MSP-AU'
dep_event_people[dep_event_people$event_id == 'MS609-0254-1' & 
                   dep_event_people$pers_id == 'B_Vezia_MSP-AU',]$pers_id <- 'B_Veziat_MSP-AU'

# Guilhem Mas and Guilhem Palazis are mentioned by Bernard Mas senior but not in a heretic event. 
# According to Bernard, they were trying to save their mother and sister from heresy
dep_event_people[dep_event_people$event_id == 'MS609-0199-9' &
                   dep_event_people$pers_id == 'Guilhem_del_Mas_Senior_MSP-AU',]$role <- 'npar'
dep_event_people[dep_event_people$event_id == 'MS609-0199-9' &
                   dep_event_people$pers_id == 'Guilhem_del_Mas_Chap_MasSaintesPuelles',]$role <- 'npar'

# Garnier is referring to his own son Pons Garnier, not to Pons Gauta
dep_event_people[dep_event_people$event_id == 'MS609-0182-4' & 
                   dep_event_people$pers_id == 'Pons_Gauta_MSP-AU',]$pers_id <- 'Pons_Garnier_MSP-AU'

# The person Guilhem Mas junior is referring to is his brother Jordanet (or Jordan junior), not his uncle Jordan
dep_event_people[dep_event_people$event_id == 'MS609-0203-2' & 
                   dep_event_people$pers_id == 'Jordan_del_Mas_MSP-AU',]$pers_id <- 'Jordanet_del_Mas_MSP-AU'
dep_event_people[dep_event_people$event_id == 'MS609-0207-7' & 
                   dep_event_people$pers_id == 'Jordan_del_Mas_MSP-AU',]$pers_id <- 'Jordanet_del_Mas_MSP-AU'

# The mother of Guilhem Canast-Brus is Peirona Canast, not Guilhelma Canast
dep_event_people[dep_event_people$event_id == 'MS609-0207-5' & 
                   dep_event_people$pers_id == 'Guilhelma_Canast_de_Paracol_MSP-AU',]$pers_id <- 'Peirona_Canast_mother_MSP-AU'
dep_event_people[dep_event_people$event_id == 'MS609-0015-4' & 
                   dep_event_people$pers_id == 'Guilhelma_Canast_de_Paracol_MSP-AU',]$pers_id <- 'Peirona_Canast_mother_MSP-AU'

# The Jordan Mas referred is said to be deceased, but this Jordan Mas deposed the day before...
dep_event_people[dep_event_people$event_id == 'MS609-0211-11' & dep_event_people$pers_id == 'Jordan_del_Mas_MSP-AU',]
dep_event_people <- dep_event_people[-4028,] # I just removed this inculpation

# The husband of Peregrina de Mont-Serveur is Ysarn the Mont-Serveur, not Ysarn de Fanjeaux
dep_event_people[dep_event_people$event_id == 'MS609-0015-1' & 
                   dep_event_people$pers_id == 'Ysarn_de_Fanjeaux_FJX-AU',]$pers_id <- 'Ysarn_de_Mont_Server_SRZ-TA'

# The brother of Peire Faure is Arnald Faure of SML, not of MSP
dep_event_people[dep_event_people$event_id == 'MS609-0454-1' & 
                   dep_event_people$pers_id == 'Arnald_Faure_MSP-AU',]$pers_id <- 'Arnald_Faure_SML-AU'

# The husband of Auda Comas is Pons not Peire Comas
dep_event_people[dep_event_people$event_id == 'MS609-0534-1' & 
                   dep_event_people$pers_id == 'Peire_Comas_SML-AU',]$pers_id <- 'Pons_Comas_SML-AU'

# I will not consider the episode of Bernard Quiders urinating over Peire Ramond Prosat an inculpation whatsoever, so 
# the owner of the house must not be inculpated
dep_event_people[dep_event_people$event_id == 'MS609-0206-1' &
                   dep_event_people$role == 'own',]$role <- 'npar'

# W Babau is said explicitly not to adore the heretics
dep_event_people[dep_event_people$event_id == 'MS609-0566-1' &
                   dep_event_people$pers_id == 'W_Babau_CNY-AU',]$role <- 'npar'

# The husband of Ermengarde Gazanha (2) is Pons not Piere
dep_event_people[dep_event_people$event_id == 'MS609-0379-1' &
                   dep_event_people$pers_id == 'Peire_Gazanha_MSP-AU',]$pers_id <- 'Pons_Gazanha_MSP-AU'

# The Raimund Alaman here is the father, not the son
dep_event_people[dep_event_people$event_id == 'MS609-0214-1' &
                   dep_event_people$pers_id == 'Raimund_Alaman_MSP-AU',]$pers_id <- 'Raimund_Alaman_Sr_MSP-AU' # new ID

# The late B Roger, wife of Raimunda, is not B Roger, wife of Galhard
dep_event_people[dep_event_people$event_id == 'MS609-0150-1' &
                   dep_event_people$pers_id == 'B_Roger_2_MSP-AU',]$pers_id <- 'B_Roger_3_MSP-AU' # new ID

# References but not participants
dep_event_people[dep_event_people$event_id == 'MS609-0211-1' & 
                   dep_event_people$pers_id == 'Guilhem_del_Mas_Senior_MSP-AU',]$role <- 'ref'
dep_event_people[dep_event_people$event_id == 'MS609-0211-2' & 
                   dep_event_people$pers_id == 'Guilhem_del_Mas_Senior_MSP-AU',]$role <- 'ref'
dep_event_people[dep_event_people$event_id == 'MS609-0211-8' & 
                   dep_event_people$pers_id == 'Bernard_Barrau_MSP-AU',]$role <- 'ref'
dep_event_people[dep_event_people$event_id == 'MS609-0299-1' & 
                   dep_event_people$pers_id == 'Guilhem_Ramanh_MSP-AU',]$role <- 'ref'

########################################################################################################################

# This is to facilitate connection among datasets
people <- merge(people,places[,c('place_id','placename')],by='place_id',all.x=TRUE) # first, let's get villages
people$fullname <- people$name
depositions$fullname <- depositions$deponent_pers_id
dep_event_people$fullname <- dep_event_people$pers_id

# Now, let's remove from the sample individuals who are not identifiable
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

targets_ids <- unique(edge_list$pers_id) 
length(targets_ids) 
targets <- people[people$fullname %in% targets_ids,]
unique(targets$placename) # These come from 49 different villages
summary(factor(targets$placename)) 

########################################################################################################################

# VILLAGES

# Targets distribution by village
villages <- as.data.frame(summary(factor(targets$placename))[c(-50)]) # remove when the place is empty
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

# Convert in an igraph object
inculp_ntw <- graph_from_edgelist(as.matrix.data.frame(edge_list[,c('deponent_pers_id','pers_id')]),
                                  directed = TRUE)

# edge attributes (date of the deposition)
E(inculp_ntw)$dep_date <- as.character(edge_list$dep_date) # add date of the inculpation
# add deponents who did not inculpate anybody as isolates
'%!in%' <- function(x,y)!('%in%'(x,y))
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

# Coloured by village
jpeg(filename='Network of incriminations.jpeg',width=12,height=12,units='in',res=1000)
plot(inculp_ntw,
     vertex.label=NA,vertex.size=2,
     vertex.color=ifelse(degree(inculp_ntw,mode='total') == 0,grey(0.5,0.2),
                         ifelse(V(inculp_ntw)$village == 'Mas-Saintes-Puelles','sienna3',
                                ifelse(V(inculp_ntw)$village == 'Saint-Martin-Lalande','springgreen4',
                                       ifelse(V(inculp_ntw)$village == 'Laurac','deeppink','goldenrod3')))),
     vertex.frame.color=ifelse(degree(inculp_ntw,mode='total') == 0,grey(0,0.2),'black'),
     edge.arrow.size=.2,edge.color=gray(0.35),edge.lty=1,
     layout=inculp_layout,
     main='Incriminations contained in Manuscript 609 (Bibliotheque de Toulouse)')
legend("bottomright",bty="o",legend=c('Mas-Saintes-Puelles','Saint-Martin-Lalande','Laurac','Somewhere else','Unknown'),
       pch=21,pt.bg=c('sienna3','springgreen4','deeppink','goldenrod3','white'),
       pt.cex=1.25, cex=1.25, ncol=1)
dev.off()

# Extraction of number of targets per deponent (and inculpations received)
inculp_by_deponent <- data.frame(fullname = V(inculp_ntw)$name,
                                 inculp_sent = degree(inculp_ntw,mode='out'), # inculpations sent
                                 inculp_rec = degree(inculp_ntw,mode='in')) # inculpations received
inculp_by_deponent <- inculp_by_deponent[inculp_by_deponent$fullname %in% deponents_ids,] # only deponents
deponents <- merge(deponents,inculp_by_deponent,by='fullname')

# Merge with people's data
people <- merge(people,inculp_by_deponent,by='fullname',all.x=TRUE) 
people$inculp_rec[is.na(people$inculp_rec)] <- 0 

summary(people$inculp_sent);summary(people$inculp_rec)
rm(inculp_by_deponent);rm(add_nodes)

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

# Extraction of number of targets per deposition
inculp_by_deposition <- data.frame(document_id=V(g)$name[V(g)$type == 'deposition'],
                                   inculp = as.vector(degree(g,mode='out'))[1:685])
depositions <- merge(depositions,inculp_by_deposition,by='document_id',all.x=TRUE)
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

jpeg(filename='Incriminations over time.jpeg',width=12,height=7,units='in',res=1000)
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
                                         ifelse(V(snapshot_ntw[[i]])$village == 'Laurac','deeppink','goldenrod3')))),
       vertex.frame.color=ifelse(degree(snapshot_ntw[[i]],mode='total') == 0,grey(0,0.2),'black'),
       edge.width=.5,edge.arrow.size=.15,edge.lty=1,
       edge.color= ifelse(E(snapshot_ntw[[i]])$dep_date != key_dates[i],gray(0.15),'red'),
       layout=inculp_layout,
       main=paste('Incriminations by',names(snapshot_ntw)[[i]]),sep=' ')
}
legend("bottomright",bty="o",legend=c('Mas-Saintes-Puelles','Saint-Martin-Lalande','Laurac','Somewhere else','Unknown'),
       pch=21,pt.bg=c('sienna3','springgreen4','deeppink','goldenrod3','white'))
dev.off()

rm(dates_to_plot);rm(key_dates);rm(i)

########################################################################################################################

# CROSS-SECTIONAL DESCRIPTION OF THE INCULPATION NETWORK

# Summary table
inculp_desc <- data.frame(stats=c('N','incriminations','components (n > 1)','largest component','isolates','density','ave degree',
                                  'sd (out-degree)','sd (in-degree)','recip','recip (deponents only)','trans',
                                  'trans (deponents only)','degree centr','diameter','ave path length',
                                  'EI (gender)','gender missing','EI (village)','village missing'),
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
  xlab('Incriminations reported') + ylab('Count (log10 scale)') + 
  xlim(c(-1,(1+max(degree(inculp_ntw,mode='out'))))) +
  scale_y_log10() +
  grid.background

ggplot()+
  geom_histogram(aes(x=degree(inculp_ntw,mode='in')),
                 bins=(1+max(degree(inculp_ntw,mode='in'))),colour='black',fill='tomato')+
  xlab('Incriminations received') + ylab('Count (log10 scale)') + 
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
inculp_desc[19,'value'] <- round(ei(inculp_ntw,'village'),3) 
inculp_desc[20,'value'] <- sum(is.na(V(inculp_ntw)$village)) # village missing

inculp_desc # Summary table

# Individuals for which we know gender and village
vcount(inculp_ntw) - sum(is.na(V(inculp_ntw)$gender) | is.na(V(inculp_ntw)$village))

########################################################################################################################

# Remove unnecessary objects
rm(deponents);rm(edge_list);rm(events);rm(inculp_components);rm(grid.background);rm(inculp_doc)
rm(inculp_layout);rm(missing_days);rm(no_events);rm(places);rm(reportees);rm(targets);rm(temp_data)
rm(dep_le_mas);rm(dep_saint_martin);rm(depositions_ids);rm(dep_event)

# Save image
save.image('Toulouse_data.RData')