
data<-read.table("~/boris_output.txt",h=T) # Import data
tail(data)

# Install packages (to be done once for all)
library(tidyr)
library(ggplot2) 

head(data) # Look at the first lines of data

# Create a column "behav_class" were the courship event not are grouped together under the level "courtship" 
data$behav_class<-data$behavior # create a new column "behav_class"
levels(data$behav_class)<-c(levels(data$behav_class),"courtship") # create the additional level "courtship"
data$behav_class[data$behav_class == "approache" | data$behav_class =="circle-display" | 
                   data$behav_class =="circle-female" | data$behav_class =="circle-nest"| 
                   data$behav_class == "courting" | data$behav_class == "guarding"] <- "courtship" # Replace selected events by "courtship"
data$behav_class<-droplevels(data$behav_class) # remove the levels that were replaced
levels(data$behav_class) # look at the levels

# Create a frequency table of the data
freq.behav<-as.data.frame(ftable(observation_id ~subject + behav_class,data = data)) # Table of number of events per subject, in each video
head(freq.behav)
data2<-data[,c(1:3)] # Extract columns of the dataset about the video name, the female morph and the male morphs

d1<-merge(freq.behav,data2[!duplicated(data2),],by = "observation_id",all.y = F) # Adding the information from these columns to the 
# frequency tables
head(d1)
View(d1)


####### Question 1: Courtship display in males  #######

# 1.Courtship 

d.male.court<-d1[d1$behav_class == "courtship" & d1$male != "PLPL",]
d.male.court<- d.male.court[d.male.court$subject != "Female_PL",] 

d.male.court<- d.male.court[d.male.court$subject != "Female_PL_PL",]# New dataset without the videos PLPL male, the females
d.male.court

## Boxplot courtship PL males
plot(as.factor(d.male.court$fem[d.male.court$subject == "Male_PL"]),d.male.court$Freq[d.male.court$subject == "Male_PL"], main = "Male PL") 

## Boxplot courtship SB males
plot(as.factor(d.male.court$fem[d.male.court$subject == "Male_SB"]),d.male.court$Freq[d.male.court$subject == "Male_SB"], main = "Male SB") 


### Male SB (outliers)

d.male.court.minus.sb.out<-d.male.court[d.male.court$observation_id != "video48", ]
d.male.court.minus.sb.out<-d.male.court.minus.sb.out[d.male.court.minus.sb.out$observation_id != "video29", ]
d.male.court.minus.sb.out<-d.male.court.minus.sb.out[d.male.court.minus.sb.out$observation_id != "video33", ] # supprimé les vidéos qui semblaient être des outliers
View(d.male.court.minus.sb.out)
plot(as.factor(d.male.court.minus.sb.out$fem[d.male.court.minus.sb.out$subject == "Male_SB"]),d.male.court.minus.sb.out$Freq[d.male.court.minus.sb.out$subject == "Male_SB"])


View(d.male.court.minus.null)
plot(d.male.court.minus.null$fem[d.male.court.minus.null$subject == "Male_SB"],d.male.court.minus.null$Freq[d.male.court.minus.null$subject == "Male_SB"])

d.male.court.minus.null.sb.out<-d.male.court.minus.null[d.male.court.minus.null$observation_id != "video48", ]
plot(d.male.court.minus.null.sb.out$fem[d.male.court.minus.null.sb.out$subject == "Male_SB"],d.male.court.minus.null.sb.out$Freq[d.male.court.minus.null.sb.out$subject == "Male_SB"])

### Male PL (outliers)

d.male.court.minus.pl.out<-d.male.court[d.male.court$observation_id != "video27", ]
d.male.court.minus.pl.out<-d.male.court.minus.pl.out[d.male.court.minus.pl.out$observation_id != "video12", ] # supprimé les vidéos qui semblaient être des outliers
View(d.male.court.minus.pl.out)
plot(d.male.court.minus.pl.out$fem[d.male.court.minus.pl.out$subject == "Male_PL"],d.male.court.minus.pl.out$Freq[d.male.court.minus.pl.out$subject == "Male_PL"])

plot(d.male.court.minus.null$fem[d.male.court.minus.null$subject == "Male_PL"],d.male.court.minus.null$Freq[d.male.court.minus.null$subject == "Male_PL"])

d.male.court.minus.null.pl.out<-d.male.court.minus.null[d.male.court.minus.null$observation_id != "video27", ]
plot(d.male.court.minus.null.pl.out$fem[d.male.court.minus.null.pl.out$subject == "Male_PL"],d.male.court.minus.null.pl.out$Freq[d.male.court.minus.null.pl.out$subject == "Male_PL"])


#### Ggplot to see outliers 

pd<-position_dodge(1)
court<-ggplot(d.male.court, aes(x=female, y=log(Freq+1))) +
  facet_grid(cols = vars(subject)) +
  geom_boxplot() +
  geom_point(position = position_dodge(width = 1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white"),
        axis.text = element_text(size = 15)) + 
  labs(title="Number of courtship events") + ylab("Number of courtship event") + xlab("Morph of the female")
court


##### Add yes/no variable of courtship 
d.male.court$cour.binary<-rep(0,length(d.male.court$observation_id))
d.male.court$cour.binary[d.male.court$Freq != 0]<-1

d.male.court.minus.null<- d.male.court[d.male.court$Freq != 0,] 


###### Male response to female
# PL male
freq.court.pl<-table(d.male.court$fem[d.male.court$subject == "Male_PL"],d.male.court$cour.binary[d.male.court$subject == "Male_PL"])
freq.court.pl
# SB male
freq.court.sb<-table(d.male.court$fem[d.male.court$subject == "Male_SB"],d.male.court$cour.binary[d.male.court$subject == "Male_SB"])
freq.court.sb


## Boxplot courtship intensity PL males
plot(d.male.court$fem[d.male.court$subject == "Male_PL" & 
       d.male.court$Freq != 0] ,d.male.court$Freq[d.male.court$subject == "Male_PL"& 
                                                    d.male.court$Freq != 0], main = "Male PL")
court1<-d.male.court[d.male.court$Freq != 0,] 
ggplot(d.male.court, aes(x=female, y=log(Freq+0.0005))) +
  facet_grid(cols = vars(subject)) +
  geom_boxplot(fill = "lightgrey") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',binwidth = 0.2, color = "darkblue") + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white")) + 
  labs(title="Number of courtship events") + ylab("Number of courtship event") + xlab("Morph of the female")
court1

## For figure
grid.arrange(ggplot(d.male.court[d.male.court$subject == "Male_PL",], aes(x=female, y=log(Freq+0.0005))) +
  geom_boxplot(fill = "lightgrey") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',binwidth = 0.2, color = "darkblue") + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white")) + 
  labs(title="log Number of courtship events") + ylab("log Number of courtship event") + xlab("Female") + 
  ggtitle("(a) Male PL"),
  ggplot(d.male.court[d.male.court$subject == "Male_SB",], aes(x=female, y=log(Freq+0.0005))) +
    geom_boxplot(fill = "lightgrey") +
    geom_dotplot(binaxis = 'y', stackdir = 'center',binwidth = 0.2, color = "darkblue") + theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "white", colour = "white")) + 
    labs(title="log Number of courtship events") + ylab("") + xlab("Female") + 
    ggtitle("(b) Male SB"), ncol = 2)


## Boxplot courtship intensity SB males
plot(d.male.court$fem[d.male.court$subject == "Male_SB"& 
      d.male.court$Freq != 0] ,d.male.court$Freq[d.male.court$subject == "Male_SB"& 
                                                    d.male.court$Freq != 0], main = "Male SB")



# 2. Agressions 

# Create a column "behav_class" were the courship event not are grouped together under the level "aggression" 

levels(data$behav_class)<-c(levels(data$behav_class),"aggression") # create the additional level "aggression"
data$behav_class[data$behav_class == "attack"] <- "aggression" # Replace selected events by "aggression"
data$behav_class<-droplevels(data$behav_class)
levels(data$behav_class) # look at the levels

# Create a frequency table of the data
freq.behav<-as.data.frame(ftable(observation_id ~subject + behav_class,data = data)) # Table of number of events per suject, in each video
head(freq.behav)
data2<-data[,c(1:3)] # Extract columns of the dataset about the video name, the female morph and the male morphs

d1<-merge(freq.behav,data2[!duplicated(data2),],by = "observation_id",all.y = F) # Adding the information from these columns to the 
# frequency tables
head(d1)
View(d1)




d.male.ag<-d1[d1$behav_class == "aggression",]
d.male.ag<-d.male.ag[d.male.ag$subject != "Female_PL",] 
d.male.ag


# Plot aggressions male SB
plot(d.male.ag$fem[d.male.ag$subject == "Male_SB"],d.male.ag$Freq[d.male.ag$subject == "Male_SB"])

# Plot aggressions male PL
plot(d.male.ag$fem[d.male.ag$subject == "Male_PL"],d.male.ag$Freq[d.male.ag$subject == "Male_PL"])

d.male.ag<-d.male.ag[d.male.ag$male != "PLPL",]
pd<-position_dodge(1)
ag<-ggplot(d.male.ag, aes(x=female, y=Freq)) +
  facet_grid(cols = vars(subject),scales = "free_y") +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center',binwidth = 3, color = "darkblue",dotsize = 2) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white"),
        axis.text = element_text(size = 15)) + 
  labs(title="Number of aggressive events") + ylab("Number of aggressive event") + xlab("Morph of the female")
ag
View(d.male.ag)


# with outlier male SB removed (video 47)

d.male.ag.sb.out<-d.male.ag[d.male.ag$observation_id != "video47", ]
d.male.ag.sb.out


plot(d.male.ag.sb.out$fem[d.male.ag.sb.out$subject == "Male_SB"],d.male.ag.sb.out$Freq[d.male.ag.sb.out$subject == "Male_SB"])


##### Add yes/no variable of aggressions
d.male.ag$cour.binary<-rep(0,length(d.male.ag$observation_id))
d.male.ag$cour.binary[d.male.ag$Freq != 0]<-1

d.male.ag.minus.null<- d.male.ag[d.male.ag$Freq != 0,] s 
d.male.ag.minus.null


d.male.ag.sb.out$cour.binary<-rep(0,length(d.male.ag.sb.out$observation_id))
d.male.ag.sb.out$cour.binary[d.male.ag.sb.out$Freq != 0]<-1

d.male.ag.minus.null.sb.out<- d.male.ag.sb.out[d.male.ag.sb.out$Freq != 0,] 
d.male.ag.minus.null.sb.out


###### Male response to female
# PL male
freq.ag.pl<-table(d.male.ag$fem[d.male.ag$subject == "Male_PL"],d.male.ag$cour.binary[d.male.ag$subject == "Male_PL"])
freq.ag.pl

# SB male
freq.ag.sb<-table(d.male.ag$fem[d.male.ag$subject == "Male_SB"],d.male.ag$cour.binary[d.male.ag$subject == "Male_SB"])
freq.ag.sb


## Boxplot aggressions intensity PL males
plot(d.male.ag$fem[d.male.ag$subject == "Male_PL"& 
                        d.male.ag$Freq != 0] ,d.male.ag$Freq[d.male.ag$subject == "Male_PL"& 
                                                                     d.male.ag$Freq != 0])

## Boxplot aggressions intensity SB males
plot(d.male.ag$fem[d.male.ag$subject == "Male_SB"& 
                        d.male.ag$Freq != 0] ,d.male.ag$Freq[d.male.ag$subject == "Male_SB"& 
                                                                     d.male.ag$Freq != 0])



pd<-position_dodge(1)
ag<-ggplot(d.male.ag.minus.null, aes(x=female, y=Freq)) +
  facet_grid(cols = vars(subject),scales = "free_y") +
  geom_dotplot(binaxis = 'y', stackdir = 'center',binwidth = 3, color = "darkblue") +
  geom_boxplot() +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white")) + 
  labs(title="Number of aggressive events") + ylab("Number of aggressive event") + xlab("Morph of the female")
ag


pd<-position_dodge(1)
ag.out<-ggplot(d.male.ag.minus.null.sb.out, aes(x=female, y=Freq)) +
  facet_grid(cols = vars(subject)) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center',binwidth = 3, color = "darkblue") + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white")) + 
  labs(title="Number of aggressive events") + ylab("Number of aggressive event") + xlab("Morph of the female")
ag.out



# FEMALE


# 2. Agressions 


# merge behavioural categories under "agression"
levels(data$behav_class)<-c(levels(data$behav_class),"aggression") 
data$behav_class[data$behav_class == "attack"] <- "aggression" 
data$behav_class<-droplevels(data$behav_class)

# Frequency table
freq.behav<-as.data.frame(ftable(observation_id ~subject + behav_class,data = data)) 
head(freq.behav)
data2<-data[,c(1:3)] 
d1<-merge(freq.behav,data2[!duplicated(data2),],by = "observation_id",all.y = F) bles
head(d1)


d.female.ag1<-d1[d1$behav_class == "attack-same-morph",]
d.female.ag1<-d.female.ag1[d.female.ag1$subject != "Male_PL",]
d.female.ag1<-d.female.ag1[d.female.ag1$subject != "Male_SB",]
d.female.ag1

d.female.ag2<-d1[d1$behav_class == "attack-other-morph",]
d.female.ag2<-d.female.ag2[d.female.ag2$subject != "Male_PL",]
d.female.ag2<-d.female.ag2[d.female.ag2$subject != "Male_SB",]
d.female.ag2


# Plot aggressions Female, attack other morph
plot(as.factor(d.female.ag2$fem[d.female.ag2$subject == "Female_PL"]),log(d.female.ag2$Freq[d.female.ag2$subject == "Female_PL"]+1), main = "attack other morph")

# Plot aggressions Female, attack same morph
plot(as.factor(d.female.ag1$fem[d.female.ag1$subject == "Female_PL"]),log(d.female.ag1$Freq[d.female.ag1$subject == "Female_PL"]+1), main = "attack same morph")


pd<-position_dodge(1)
ag1<-ggplot(d.female.ag1, aes(x=female, y=log(Freq+1))) +
  facet_grid(cols = vars(subject)) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center',binwidth = 0.1, color = "darkblue") + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white")) + 
  labs(title="attack-same-morph") + ylab("Number of aggressive event") + xlab("Morph of the female")
ag1 # attack same morph


pd<-position_dodge(1)
ag2<-ggplot(d.female.ag1, aes(x=female, y=log(Freq+1))) +
  facet_grid(cols = vars(subject)) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center',binwidth = 0.1, color = "darkblue") + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "white")) + 
  labs(title="attack-same-morph") + ylab("Number of aggressive event") + xlab("Morph of the female")
ag2 # attack other morph 







############### Female choice: courtship 
court.event <- pivot_wider(d.male.court[,c(1:6)], names_from = subject,values_from = Freq)
court.event$fem.choice <- rep(0, nrow(court.event))
court.event$fem.choice[court.event$Male_PL > court.event$Male_SB] <- "PL"
court.event$fem.choice[court.event$Male_PL < court.event$Male_SB] <- "SB"


#####################################################################
####################### RI ESTIMATES ################################
#####################################################################

####################### SB females #######################

# RI formula
RI <- function(C, H) {1 - 2*(H/(H+C))}

# Conspecific/Heterospecific courting ratio
freq.sb <- (length(court.event$fem.choice[court.event$female == "SB" & court.event$fem.choice == "SB"])/
           length(court.event$fem.choice[court.event$female == "SB"])) # Freq SB female mating with PL
freq.sbpl <- (length(court.event$fem.choice[court.event$female == "SB" & court.event$fem.choice == "PL"])/
           length(court.event$fem.choice[court.event$female == "SB"]))  # Freq Sb female mating with SB

freq.consp.sb <- freq.sb/(freq.sbpl+freq.sb) # Freq conspecific mating (if everyone mate)
freq.hsp.sb <- freq.sbpl/(freq.sbpl+freq.sb)  #Freq heterospecific mating (if everyone mate)
RI(C = freq.consp.sb, H = freq.hsp.sb) # Observed RI


###### Cumulative strength

# Prop Conspecific at barrier 2
C.sb.b2 <- (freq.consp.sb*C.sb.tot.survey)/((freq.consp.sb*C.sb.tot.survey)+(freq.hsp.sb*H.sb.tot.survey))
# Prop Heterospecific at barrier 2
H.sb.b2 <-(freq.hsp.sb*H.sb.tot.survey)/((freq.consp.sb*C.sb.tot.survey)+(freq.hsp.sb*H.sb.tot.survey))

# Total RI sb
sb.total.ri <- RI(C = C.sb.b2, H = H.sb.b2)

###### Absolute and Relative strength
sb.temporal.ri <- RI(C = C.sb.tot.survey, H = H.sb.tot.survey) # Absolute strength of temporal mating barriers
sb.mate.ri <- sb.total.ri-sb.temporal.ri # Absolute strength of assortative mating RI

sb.temporal.ri/sb.total.ri # Relative strength of temporal mating barriers
sb.mate.ri/sb.total.ri # Relative strength of assortative mating


####################### PL females #######################

# Conspecific/Heterospecific courting ratio
freq.pl <-length(court.event$fem.choice[court.event$female == "PL" & court.event$fem.choice == "PL"])/
  length(court.event$fem.choice[court.event$female == "PL"]) # Freq PL female mating with PL
freq.plsb <- length(court.event$fem.choice[court.event$female == "PL" & court.event$fem.choice == "SB"])/
  length(court.event$fem.choice[court.event$female == "PL"]) # Freq PL female mating with SB

freq.consp <- freq.pl/(freq.plsb+freq.pl) # Freq conspecific mating (if everyone mate)
freq.hsp <- freq.plsb/(freq.plsb+freq.pl)  #Freq heterospecific mating (if everyone mate)
ri.pl.choice <- RI(C = freq.consp, H = freq.hsp) # Observed RI




######## RI female PL at barrier 2 (mjó)
C.pl.b2.mjo <- (freq.consp*C.pl.mjo.survey.f)/((freq.consp*C.pl.mjo.survey.f)+(freq.hsp*H.pl.mjo.survey.f))
H.pl.b2.mjo <-(freq.hsp*H.pl.mjo.survey.f)/((freq.consp*C.pl.mjo.survey.f)+(freq.hsp*H.pl.mjo.survey.f))
pl.total.ri.mjo <- RI(C = C.pl.b2.mjo, H = H.pl.b2.mjo)

ACchoice <- pl.total.ri.mjo - mjo.ri.temp.pl # AC asortative mating
mjo.ri.temp.pl/pl.total.ri.mjo # Relative strength of temporal spawning barriers
ACchoice/pl.total.ri.mjo # Relative strength of asortative mating


######## RI female PL at barrier 2 (óla)
C.pl.b2.ol <- (freq.consp*C.pl.ol.survey.f)/((freq.consp*C.pl.ol.survey.f)+(freq.hsp*H.pl.ol.survey.f))
H.pl.b2.ol <-(freq.hsp*H.pl.ol.survey.f)/((freq.consp*C.pl.ol.survey.f)+(freq.hsp*H.pl.ol.survey.f))
pl.total.ri.ol <- RI(C = C.pl.b2.ol, H = H.pl.b2.ol)

ACc.pl.ol <- pl.total.ri.ol - ol.ri.temp.pl # AC assortative mating
ol.ri.temp.pl/pl.total.ri.ol # Relative strength of temporal mating barriers
ACc.pl.ol/pl.total.ri.ol # Relative strength of assortative mating

############# RI SB óla
C.sb.b2.ol <- (freq.consp.sb*C.sb.ol.survey.f)/((freq.consp.sb*C.sb.ol.survey.f)+(freq.hsp.sb*H.sb.ol.survey.f))
# Prop Heterospecific at barrier 2
H.sb.b2.ol <-(freq.hsp.sb*H.sb.ol.survey.f)/((freq.consp.sb*C.sb.ol.survey.f)+(freq.hsp.sb*H.sb.ol.survey.f))
sb.total.ri.ol <- RI(C = C.sb.b2.ol, H = H.sb.b2.ol)

ACc.sb.ol <- sb.total.ri.ol - ol.ri.temp.sb # AC assortative mating
ol.ri.temp.sb/sb.total.ri.ol # Relative strength of temporal mating barriers
ACc.sb.ol/sb.total.ri.ol # Relative strength of assortative mating

############# RI SB mjó
C.sb.b2.mjo <- (freq.consp.sb*C.sb.mjo.survey.f)/((freq.consp.sb*C.sb.mjo.survey.f)+(freq.hsp.sb*H.sb.mjo.survey.f))
# Prop Heterospecific at barrier 2
H.sb.b2.mjo <-(freq.hsp.sb*H.sb.mjo.survey.f)/((freq.consp.sb*C.sb.mjo.survey.f)+(freq.hsp.sb*H.sb.mjo.survey.f))
sb.total.ri.mjo <- RI(C = C.sb.b2.mjo, H = H.sb.b2.mjo)

ACc.sb.mjo <- sb.total.ri.mjo - mjo.ri.temp.sb # AC asortative mating
mjo.ri.temp.sb/sb.total.ri.mjo # Relative strength of temporal mating barriers
ACc.sb.mjo/sb.total.ri.mjo # Relative strength of asortative mating




################### RI PL with early postzygotic barrier

C.pl.b2.mjo.pz <- (freq.consp*C.pl.mjo.survey.f*94.91404)/((freq.consp*C.pl.mjo.survey.f*94.91404)+(freq.hsp*H.pl.mjo.survey.f*76.86006))
H.pl.b2.mjo.pz <-(freq.hsp*H.pl.mjo.survey.f*76.86006)/((freq.consp*C.pl.mjo.survey.f*94.91404)+(freq.hsp*H.pl.mjo.survey.f*76.86006))
pl.total.ri.mjo.pz <- RI(C = C.pl.b2.mjo.pz, H = H.pl.b2.mjo.pz)

ACpz <- pl.total.ri.mjo.pz- pl.total.ri.mjo # AC assortative mating
mjo.ri.temp.pl/pl.total.ri.mjo.pz # Relative strength of temporal spawning barriers
ACchoice/pl.total.ri.mjo.pz # Relative strength of assortative mating
ACpz/pl.total.ri.mjo.pz # Relative strength of postzygotic isolation


###### PL ol with postzygotic 

C.pl.b2.ol.pz <- (freq.consp*C.pl.ol.survey.f*94.91404)/((freq.consp*C.pl.ol.survey.f*94.91404)+(freq.hsp*H.pl.ol.survey.f*76.86006))
H.pl.b2.ol.pz <-(freq.hsp*H.pl.ol.survey.f*76.86006)/((freq.consp*C.pl.ol.survey.f*94.91404)+(freq.hsp*H.pl.ol.survey.f*76.86006))
pl.total.ri.ol.pz <- RI(C = C.pl.b2.ol.pz, H = H.pl.b2.ol.pz)

ACpz.ol <- pl.total.ri.ol.pz- pl.total.ri.ol # AC assortative mating
ol.ri.temp.pl/pl.total.ri.ol.pz # Relative strength of temporal spawning barriers
ACc.pl.ol/pl.total.ri.ol.pz # Relative strength of assortative mating
ACpz.ol/pl.total.ri.ol.pz # Relative strength of postzygotic isolation

###### SB Mjóanes with postzygotic 

C.sb.b2.mjo.pz <- (freq.consp.sb*C.sb.mjo.survey.f*87.35358)/((freq.consp.sb*C.sb.mjo.survey.f*87.35358)+(freq.hsp.sb*H.sb.mjo.survey.f*53.7042))
H.sb.b2.mjo.pz <-(freq.hsp.sb*H.sb.mjo.survey.f*53.7042)/((freq.consp.sb*C.sb.mjo.survey.f*87.35358)+(freq.hsp.sb*H.sb.mjo.survey.f*53.7042))
sb.total.ri.mjo.pz <- RI(C = C.sb.b2.mjo.pz, H = H.sb.b2.mjo.pz)

ACpz.sb.mjo <- sb.total.ri.mjo.pz- sb.total.ri.mjo # AC postzygotic
mjo.ri.temp.sb/sb.total.ri.mjo.pz # Relative strength of temporal sp.sbawning barriers
ACc.sb.mjo/sb.total.ri.mjo.pz # Relative strength of assortative mating
ACpz/sb.total.ri.mjo.pz # Relative strength of postzygotic isolation

###### SB Ólafsdráttur

C.sb.b2.ol.pz <- (freq.consp.sb*C.sb.ol.survey.f*87.35358)/((freq.consp.sb*C.sb.ol.survey.f*87.35358)+(freq.hsp.sb*H.sb.ol.survey.f*53.7042))
H.sb.b2.ol.pz <-(freq.hsp.sb*H.sb.ol.survey.f*53.7042)/((freq.consp.sb*C.sb.ol.survey.f*87.35358)+(freq.hsp.sb*H.sb.ol.survey.f*53.7042))
sb.total.ri.ol.pz <- RI(C = C.sb.b2.ol.pz, H = H.sb.b2.ol.pz)

ACpz.sb.ol <- sb.total.ri.ol.pz- sb.total.ri.ol # AC postzygotic
ol.ri.temp.sb/sb.total.ri.ol.pz # Relative strength of temporal sp.sbawning barriers
ACc.sb.ol/sb.total.ri.ol.pz # Relative strength of assortative mating
ACpz.sb.ol/sb.total.ri.ol.pz # Relative strength of postzygotic isolation


##### Fisher test
fem.c <- rbind(
  table(
  court.event$fem.choice[court.event$female == "PL" & court.event$fem.choice != "0"]),
  table(court.event$fem.choice[court.event$female == "SB" & court.event$fem.choice != "0"]))
