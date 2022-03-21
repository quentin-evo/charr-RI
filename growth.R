library(tidyr)
library(ggplot2)

d1<-read.table("~/free-embryos-sizes.txt",h=T)
d1<-separate(d1,col = fam,into = c("date","fam"),sep = 2)
d1$age<-rep(2,nrow(d1))
liver1$age<-rep(10,nrow(liver1))

size.jj<-read.table("~/bucket-june.txt",h=T)
density.jj<-read.table("~/density-bucket-june.txt", h=T)
names(size.jj)<-c("fam","length_mm","weight","age")
size.jj<-merge(size.jj, density.jj, by.x = "fam",by.y = "bucket",all.x = T)
size.jj$age<-as.numeric(size.jj$age)
size.jj$age[size.jj$age == 1]<- 7
size.jj$age[size.jj$age == 2]<- 5

colnames(size.jj) <- c("fam","length_mm","weight","age","cross","density")
size.jj$length_mm <- size.jj$length_mm*10


size.age<-rbind(liver1[,c("fam","length_mm","age")],d1[,c("fam","length_mm","age")])
size.age<-merge(size.age,liver1[,c("fam","cross")],by = "fam",all.x =T)
size.age[size.age$fam == "SB36","cross"] <-"SB"
size.age<-merge(size.age, size.jj[,c("fam", "length_mm", "age", "cross")], all = T)



head(size.age)

age.sum <- ddply(size.age, c("fam","cross","age"), summarise,
            N    = length(!is.na(length_mm)),
            mean = mean(length_mm,na.rm = T),
            sd   = sd(length_mm,na.rm = T),
            se   = sd / sqrt(N)
)


pd <- position_dodge(0.1)
ggplot(na.omit(age.sum), aes(x=as.factor(age), y=mean, colour= cross,group = fam)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black",width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd,size = 2) + theme_classic() + theme(panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank()) + 
  labs(title="Log Mean body length of each family at 4 developmental time points") + ylab("Log Length (mm)") +
  xlab("Degree day")





########## Density-corrected sizes

density_liver<-lm(log(liver1[complete.cases(liver1$length_mm),]$length_mm)~liver1[complete.cases(liver1$length_mm),]$density)
dens_res<-data.frame(liver1[complete.cases(liver1$length_mm),],
                     length.res = resid(density_liver) + summary(density_liver)[["coefficients"]][1,1],
                     age = rep(10,nrow(liver1[complete.cases(liver1$length_mm),])))

dens.m5<-lm(log(size.jj$length_mm[complete.cases(size.jj$density) & size.jj$age == 5 & size.jj$fam != "PLSB4" & complete.cases(size.jj$length_mm)])~
                  size.jj$density[complete.cases(size.jj$density) & size.jj$age == 5 & size.jj$fam != "PLSB4" & complete.cases(size.jj$length_mm)])

res.dens.m5<-data.frame(size.jj[complete.cases(size.jj$density)  & size.jj$fam != "PLSB4" 
                                & complete.cases(size.jj$length_mm) & size.jj$age == 5,],
                     length.res = resid(dens.m5) + summary(dens.m5)[["coefficients"]][1,1]
                     )

dens.m7<-lm(log(size.jj$length_mm[complete.cases(size.jj$density) & size.jj$age == 7 & size.jj$fam != "PLSB4" & complete.cases(size.jj$length_mm)])~
              size.jj$density[complete.cases(size.jj$density) & size.jj$age == 7 & size.jj$fam != "PLSB4" & complete.cases(size.jj$length_mm)])

res.dens.m7<-data.frame(size.jj[complete.cases(size.jj$density)  & size.jj$fam != "PLSB4" 
                                & complete.cases(size.jj$length_mm) & size.jj$age == 7,],
                        length.res = resid(dens.m7) + summary(dens.m7)[["coefficients"]][1,1]
)
head(res.dens.m7)
head(d1)

d1$length.res <- log(d1$length_mm)

size.age2<-rbind(dens_res[,c("fam","length.res","age")],d1[,c("fam","length.res","age")])
size.age2<-merge(size.age2,liver1[,c("fam","cross")],by = "fam",all.x =T)
size.age2[size.age2$fam == "SB36" | size.age2$fam == "SB19","cross"] <-"SB"
size.age2<- droplevels(size.age2)
size.jj2<-rbind(res.dens.m5,res.dens.m7)
size.age2<-rbind(size.age2, size.jj2[,c("fam","length.res","age","cross")])


age.sum2 <- ddply(size.age2, c("fam","cross","age"), summarise,
                 N    = length(!is.na(length.res)),
                 mean = mean(length.res,na.rm = T),
                 sd   = sd(length.res,na.rm = T),
                 se   = sd / sqrt(N)
)

age.sum2$cross[age.sum2$fam == "SB19"] <- "SB"
pd <- position_dodge(0.1)
cbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

size.all <- ggplot(na.omit(age.sum2), aes(x=as.factor(age), y=mean, colour= cross,group = fam, fill = cross)) + 
  geom_line(size = 1, position=pd) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black",width=.1, position=pd) +
  scale_fill_manual(
    values = c("blue3", "cyan3", "darkgreen", "darkolivegreen3"),
    name="Cross type",
    breaks = (c("PL","PLSB","SB","SBPL")),
    labels = c("PLxPL","PLxSB","SBxSB","SBxPL")) +
  scale_colour_manual(
    values = c( "blue3", "cyan3", "darkgreen", "darkolivegreen3")) +
  guides(colour = F) +
  geom_point(shape = 21, position=pd,size = 2) +
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85,0.4),
        legend.title = element_text(face = "bold",size = 12),
        legend.text =  element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 15, v =2.5),
        axis.title.x = element_text(size = 15, v = -0.5)) + 
  labs(title="") + ylab("Res. log Length (mm)") +
  ggtitle("(a)") +
  xlab("Months post-hatching")

grid.arrange(size.all,pcondition,ncol=2)
