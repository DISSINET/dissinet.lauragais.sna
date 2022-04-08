## DATA FROM MS 609 (BIBLIOTHEQUE DE TOULOUSE) COLLECTED BY JEAN-PAUL REHR
## Longitudinal descriptive analyses
## R script written by Jose Luis Estevez (Masaryk University)
## Date: March 18th 2022
########################################################################################################################

# DATA LOADING
rm(list=ls())
load('Toulouse_data.RData')
deceased <- readxl::read_excel('MS609_extrainfo.xlsx',sheet='deceased')
# Required packages
library(ggplot2);library(sna);library(igraph);library(stringr);library(ggpubr)
library(survival);library(survminer)

########################################################################################################################

# TEMPORAL DESCRIPTION: INCULPATIONS VS DEPOSITIONS

# Let's use only those who were targets of somebody else's deposition and get their deposition dates
info1 <- depositions[depositions$fullname %in% targets_ids,c('fullname','dep_date')]
info1$type <- 'Deposition'

# Let's get the dates when they where reported by someone else
info2 <- dep_event_people[dep_event_people$role %in% c('par','her','own','inf'),]
info2 <- info2[info2$fullname %in% targets_ids,]
info2$document_id <- str_sub(info2$event_id,1,10)
info2 <- merge(info2,depositions[,c('document_id','fullname','dep_date')],by='document_id',all.x=TRUE)

# Remove self-inculpations
info2 <- info2[info2$fullname.x != info2$fullname.y,] # fullname.x (inculpated), fullname.y (deponent)
info2 <- info2[order(info2$dep_date),] # order by time
info2 <- info2[!duplicated(info2[,c('fullname.x','fullname.y','dep_date')]),]

info2 <- info2[,c('fullname.x','dep_date')]
names(info2) <- c('fullname','dep_date')
info2$type <- 'Inculpation'

# Let's merge the information together
sample <- rbind(info1,info2)
sample <- sample[!is.na(sample$fullname),] # remove one NA

# Now, let's keep only first date (first inculpation and first deposition) for survival kind of analysis
info1 <- info1[order(info1$dep_date),]
info1 <- info1[!duplicated(info1$fullname),]
info2 <- info2[order(info2$dep_date),]
info2 <- info2[!duplicated(info2$fullname),]

# New dataset, containing only the date of first inculpation and first deposition (if any)
sample2 <- merge(info1[,1:2],info2[,1:2],by='fullname',all.y=TRUE)
names(sample2) <- c('fullname','deposition','denunciation')
# Convert to dates
sample2$deposition <- as.Date(sample2$deposition)
sample2$denunciation <- as.Date(sample2$denunciation)

# Let's add attributes of the individuals (gender, and place of origin)
sample2 <- merge(sample2,people[c('fullname','gender','placename')],by='fullname',all.x=TRUE)
sample2$deposed <- 1*!is.na(sample2$deposition) # Whether the person deposed or not
sample2$deposed <- factor(sample2$deposed,levels=c(0,1),labels=c('Not deposed','Deposed'))

# Let get rid of people from outside the villages where Inquisitors were operating
sample2 <- sample2[ifelse(sample2$deposed == 1,TRUE,
                          ifelse(sample2$placename %in% c('Mas-Saintes-Puelles','Saint-Martin-Lalande'),TRUE,FALSE)),]
sample <- sample[sample$fullname %in% sample2$fullname,]

# Let's find the difference in time
sample2$diff <- NA
for(i in 1:nrow(sample2)){
  if(sample2$deposed[i] == 'Deposed'){
    # If the person was deposed: (first) deposition date minus (first) denunciation date 
    sample2$diff[i] <- sample2$deposition[i] - sample2$denunciation[i]
  }else{
    # Otherwise, max day the inquisitors were deposing minus (first) denunciation date
    sample2$diff[i] <- as.Date(max(depositions$dep_date)) - sample2$denunciation[i]
  }
}

# For visualisation purposes
sample2$y <- 1:nrow(sample2)
sample2$y <- factor(sample2$y,levels=order(sample2$diff,decreasing = TRUE))

# Visualisation
no.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

p1 <- ggplot(data=sample)+
  geom_point(aes(x=dep_date,y=fullname,color=type,shape=type)) +
  theme(axis.text.y=element_blank()) +
  xlab('Time') + ylab('Individuals') + labs(colour='',shape='') +
  scale_colour_manual(values = c('navyblue','red')) +
  scale_shape_manual(values = c(17,4)) +
  no.background +
  theme(axis.text.y=element_blank()) +
  theme(legend.position="top", legend.justification="center")

p2 <- ggplot(data=sample2) +
  geom_vline(xintercept = 0,colour='darkgrey',lty=2) +
  geom_point(aes(x=diff,y=y,colour=deposed,shape=deposed),alpha=.75)  +
  xlab('Difference in days between (first) inculpation and (first) deposition') + ylab('') + 
  labs(colour='',shape='') +
  scale_colour_manual(values = c('springgreen4','firebrick3')) +
  scale_shape_manual(values = c(15,17)) +
  no.background +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  theme(legend.position="top", legend.justification="center")

ggarrange(p1,p2,ncol=2)

rm(info1);rm(info2);rm(p1);rm(p2)

########################################################################################################################

# SURVIVING INCRIMINATION?

# Let's see now how long took people to be deposed after being inculpated
# First, let's removed those deceased by the time the Inquisition took place
deceased <- unique(deceased$person)
deceased <- deceased[deceased %!in% deponents_ids] # if they deposed, they were alive still
sample <- sample[sample$fullname %!in% deceased,]
sample2 <- sample2[sample2$fullname %!in% deceased,]

# And let's exclude those few who deposed before being incriminated
sample3 <- sample2[sample2$diff >= 0,]

# 1) In general
fit <- survfit(Surv(sample3$diff,ifelse(sample3$deposed == 'Deposed',1,0)) ~ 1,data=sample3,
               conf.type='log-log')
summary(fit)
# 2) By gender
fit2 <- survfit(Surv(sample3$diff,ifelse(sample3$deposed == 'Deposed',1,0)) ~ sample3$gender,data=sample3,
                conf.type='log-log')
summary(fit2)
(result_surv1 <- survdiff(Surv(sample3$diff) ~ sample3$gender,data=sample3))
# 3) By village
fit3 <- survfit(Surv(sample3$diff,ifelse(sample3$deposed == 'Deposed',1,0)) ~ sample3$placename,data=sample3,
                conf.type='log-log')
summary(fit3)
(result_surv2 <- survdiff(Surv(sample3$diff) ~ sample3$placename,data=sample3))

# Visualisations
p1 <- ggsurvplot(fit,data=sample3,
           legend.title = "",
           conf.int.fill = 'navyblue',
           palette = 'navyblue')
p1$plot <- p1$plot + 
  ylab('Survival probability of being deposed') + xlab('')

p2 <- ggsurvplot(fit2,data=sample3,
           legend.title = "",
           legend.labs = c("Female", "Male"),
           conf.int = TRUE,
           palette = c("firebrick3", "dodgerblue")) 
# Add chi square text
p2$plot <- p2$plot +
  annotate("text", x=200,y=0.25,
           label=paste('Chisq =',round(result_surv1$chisq,1),
                       '\n p =',round(pchisq(result_surv1$chisq,1,lower.tail=FALSE),3),sep=' ')) +
  ylab('') + xlab('Days after (first) incrimination')

p3 <- ggsurvplot(fit3,data=sample3,
           legend.title = "",
           legend.labs = c("Mas-Saintes-Puelles", "Saint-Martin-Lalande"),
           conf.int = TRUE,
           palette = c("sienna3", "springgreen4"))
# Add chi square text
p3$plot <- p3$plot +
  annotate("text", x=200,y=0.25,
           label=paste('Chisq =',round(result_surv2$chisq,1),
                       '\n p =',round(pchisq(result_surv2$chisq,1,lower.tail=FALSE),3),sep=' ')) +
  ylab('')+ xlab('')

jpeg(filename='Survival after deposition.jpeg',width=12,height=8,units='in',res=1000)
ggarrange(p1$plot,p2$plot,p3$plot,nrow=1)
dev.off()

rm(fit);rm(fit2);rm(fit3);rm(p1);rm(p2);rm(p3);rm(result_surv1);rm(result_surv2)

########################################################################################################################

# STABILITY OF THE NETWORK

# Let's get the Hamming distance and Jaccard indices between days
# First, let's turn the igraph object into matrices
snapshot_sna <- snapshot_ntw

for(i in seq_along(snapshot_sna)){
  snapshot_sna[[i]] <- as.matrix(get.adjacency(snapshot_sna[[i]]))
  diag(snapshot_sna[[i]]) <- NA # remove diagonal
  snapshot_sna[[i]][rownames(snapshot_sna[[i]]) %in% not_deposed,] <- NA # assign missing data
}

# Now, let's create a matrix where sending the outcome
ham <- jac <- matrix(NA,nrow=length(snapshot_sna),ncol=length(snapshot_sna),
                     dimnames = list(names(snapshot_sna),names(snapshot_sna)))

# And let's define a function to obtain the Jaccard index
Jaccard <- function(matrix1,matrix2){
  shared_ties <- matrix1*matrix2
  diff_ties <- 1*((matrix1+matrix2)==1)
  denominator <- sum(shared_ties,na.rm=TRUE)+sum(diff_ties,na.rm=TRUE)
  outcome <- ifelse(denominator==0,0,sum(shared_ties,na.rm=TRUE)/denominator)
  return(outcome)
}

# Now, let's obtain the results
for(i in seq_along(snapshot_sna)){
  for(j in seq_along(snapshot_sna)){
    if(i > j){
      ham[i,j] <- sum(snapshot_sna[[i]] != snapshot_sna[[j]],na.rm = TRUE)
      jac[i,j] <- Jaccard(snapshot_sna[[i]],snapshot_sna[[j]])
    }
  }
}

########################################################################################################################

save.image('Toulouse_data2.RData')