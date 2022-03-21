
str(data)
library(ggplot2)
library(tidyr)
library(gridExtra)

data<-read.table("~/survey.txt",h=T)


############# Mjóanes ###################

mj<-data[data$site == "Mjoanes" & data$running=="1" & data$morph != "PI" 
            & data$morph != "LB" & data$sex !="J" & data$fishing_date != "03.10.17" & data$fishing_date != "12.10.17",]
mj<-droplevels(mj)
mj.f<-as.data.frame(table(mj[, c("sex","day","morph")]))
mj.f$day<-as.POSIXct(x=mj.f$day,format="%j")


pm.pl<-ggplot(data=mj.f[mj.f$morph == "PL",],aes(x= day,y = Freq)) + 
  geom_line(aes(x= day,y = Freq,col=sex,group=sex),size = 1, linetype ="dashed") + 
  xlab("") + ylab("Number of invidividuals") + theme_classic() + 
  geom_point(aes(colour = sex), size = 3) +
  theme(legend.position = "none", 
        axis.title.x = element_text(#face = "bold",
                                    size = 11),
        axis.title.y = element_text(#face = "bold",
          size = 11, vjust = 3),
        title = element_text(#face = "bold",
          size = 10),
        axis.text = element_text(#face = "bold",
          size = 10)) +
  labs(title = "(a) Mjóanes PL") + 
  scale_y_continuous(breaks = seq(0,115, by = 50), limits = c(0,120))

pm.sb<-ggplot(data=mj.f[mj.f$morph == "SB",],aes(x= day,y = Freq)) + 
  geom_line(aes(x= day,y = Freq,col=sex,group=sex),size = 1, linetype ="dashed") + 
  xlab("Date") + ylab("Number of invidividuals") + theme_classic() + 
  geom_point(aes(colour = sex), size = 3) +
  theme(legend.position = "none", 
        axis.title.x = element_text(#face = "bold",
                                    size = 11),
        axis.title.y = element_text(#face = "bold",
                                    size = 11, vjust = 3),
        title = element_text(#face = "bold",
                             size = 10),
        axis.text = element_text(#face = "bold",
        size = 10)) +
  labs(title = "(c) Mjóanes SB") +
  scale_y_continuous(breaks = seq(0,115, by = 50), limits = c(0,120))
pm.sb


###### Olafsdrattur 
ol<-data[data$site == "Olafdrattur" & data$running=="1" & data$morph != "PI" 
         & data$morph != "LB" & data$sex !="J",]
#ol$day[ol$day == "252"]<-"251"
ol$day[ol$day == "295"] <- "265"
ol<-droplevels(ol)
ol.f1<-as.data.frame(table(ol[, c("sex","day","morph")]))
xt <- as.Date(as.numeric(as.character(ol.f1$day)), origin = "2016-12-31")
ol.f1$day<-as.POSIXct(x=xt,format="%j")
po.pl<-ggplot(data=ol.f1[ol.f1$morph == "PL",],aes(x= day,y = Freq)) + 
  geom_line(aes(x= day,y = Freq,col=sex,group=sex),size = 1, linetype = "dashed") +
  xlab("") + theme_classic() + labs(title = "(b) Ólafsdráttur PL") + 
  geom_point(aes(colour = sex), size = 3) +
  theme(axis.title.y = element_blank(), 
        legend.position = "none", 
        axis.title.x = element_text(#face = "bold",
          size = 11),
        title = element_text(#face = "bold",
          size = 10),
        axis.text = element_text(#face = "bold",
          size = 10)
        ) + 
  scale_y_continuous(breaks = seq(0,115, by = 50), limits = c(0,120)) +
  scale_colour_discrete(name  ="Sex",breaks=c("F", "M"),labels=c("Females", "Males"))
po.sb<-ggplot(data=ol.f1[ol.f1$morph == "SB",],aes(x= day,y = Freq)) + 
  geom_line(aes(x= day,y = Freq,col=sex,group=sex),size = 1, linetype = "dashed") +
  xlab("Date") + theme_classic() + labs(title = "(d) Ólafsdráttur SB") + 
  geom_point(aes(colour = sex), size = 3) +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(#face = "bold",
          size = 11),
        title = element_text(#face = "bold",
          size = 10),
        axis.text = element_text(#face = "bold",
          size = 10),
        legend.position = c(0.75,0.5)
  ) + 
  scale_y_continuous(breaks = seq(0,115, by = 50), limits = c(0,120)) +
  scale_colour_discrete(name  ="Sex",breaks=c("F", "M"),labels=c("Females", "Males"))

### Panel
grid.arrange(pm.pl,po.pl, pm.sb,po.sb, ncol = 2, nrow = 2 )

### Save legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#### 
legend <- get_legend(po1)
po1<-po1 + theme(legend.position = "none")
####
grid.arrange(pm1,po1, legend, ncol = 3, nrow = 1, widths = c(2.3,2.3,0.8) )





######################### Estimating RI

# Mjoanes

mj<-data[data$site == "Mjoanes" & data$running=="1" & data$morph != "PI" 
         & data$morph != "LB" & data$sex !="J" & data$fishing_date != "03.10.17" & data$fishing_date != "12.10.17",]
mj<-droplevels(mj)
mj.f<-as.data.frame(table(mj[, c("sex","day","morph")]))
mj.f$day <- as.numeric(as.character(mj.f$day))

plot(mj.f$day[ mj.f$sex == "M" & mj.f$morph == "PL"],
     mj.f$Freq[mj.f$sex == "M" & mj.f$morph == "PL"])

y <- mj.f$Freq[mj.f$sex == "M" & mj.f$morph == "PL"]
x <- mj.f$day[ mj.f$sex == "M" & mj.f$morph == "PL"]

PLmjo.lm <- lm(y~poly(x,degree = 4))

newPLmjo <- data.frame(x = c(240:300))
predPLmjo <- predict.lm(PLmjo.lm,newPLmjo)

plot(newPLmjo$x, predPLmjo)
points(x,y, pch = 16, cex = 2)
points(x,y , bg = 1)



########## P heterospecific mating 

# Function
prob.offspring <- function(pop){
  x <- matrix(nrow = nrow(pop),ncol = 8)
  colnames(x) <- c("prob.PLPL","prob.PLSB","prob.SBSB","prob.SBPL",
                   "n.PLPL","n.PLSB","n.SBSB","n.SBPL")
  x[,"prob.PLPL"] <- (pop$F_PL/(pop$F_PL+pop$F_SB))*(pop$M_PL/(pop$M_PL+pop$M_SB))
  x[,"prob.PLSB"] <- (pop$F_PL/(pop$F_PL+pop$F_SB))*(pop$M_SB/(pop$M_PL+pop$M_SB))
  x[,"prob.SBSB"]<- (pop$F_SB/(pop$F_PL+pop$F_SB))*(pop$M_SB/(pop$M_PL+pop$M_SB))
  x[,"prob.SBPL"] <- (pop$F_SB/(pop$F_PL+pop$F_SB))*(pop$M_PL/(pop$M_PL+pop$M_SB))
  
  x[,"n.PLPL"] <-x[,"prob.PLPL"]*pop$F_PL
  x[,"n.PLSB"] <- x[,"prob.PLSB"]*pop$F_PL
  x[,"n.SBSB"]<-  x[,"prob.SBSB"]*pop$F_SB
  x[,"n.SBPL"] <- x[,"prob.SBPL"]*pop$F_SB
  return(x)
}

### Mjóanes

mj.f$morph_sex <- paste(mj.f$sex, mj.f$morph, sep = "_")
mj.f <- pivot_wider(mj.f[,c("day","morph_sex","Freq")],names_from = morph_sex, values_from = Freq)


mj.pr <- prob.offspring(mj.f)
colSums(mj.pr)

# Ólafsdráttur

ol<-data[data$site == "Olafdrattur" & data$running=="1" & data$morph != "PI" 
         & data$morph != "LB" & data$sex !="J" ,]
ol$day[ol$day == "252"]<-"251"
ol<-droplevels(ol)
ol.f<-as.data.frame(table(ol[, c("sex","day","morph")]))
ol.f$day <- as.numeric(as.character(ol.f$day))
ol.f$morph_sex <- paste(ol.f$sex, ol.f$morph, sep = "_")
ol.f <- pivot_wider(ol.f[,c("day","morph_sex","Freq")],names_from = morph_sex, values_from = Freq)

ol.pr <- prob.offspring(ol.f)
colSums(ol.pr)

### Total 
sum.prop <- colSums(rbind(colSums(mj.pr),colSums(ol.pr)))
sum.mjo <- colSums(mj.pr)
sum.ol <- colSums(ol.pr)

# Proportions of PL Conspecific & Heterospecific matings at Mjóanes
C.pl.mjo.survey <- sum.mjo["n.PLPL"]/(sum.mjo["n.PLPL"]+sum.mjo["n.PLSB"])
H.pl.mjo.survey <-(sum.mjo["n.PLSB"])/(sum.mjo["n.PLPL"]+sum.mjo["n.PLSB"])

# Proportions of SB Conspecific & Heterospecific matings at at Mjóanes
C.sb.mjo.survey <- sum.mjo["n.SBSB"]/(sum.mjo["n.SBSB"]+sum.mjo["n.SBPL"])
H.sb.mjo.survey <- (sum.mjo["n.SBPL"])/(sum.mjo["n.SBSB"]+sum.mjo["n.SBPL"])


# Proportions of PL Conspecific & Heterospecific matings at Ólafsdráttur
C.pl.ol.survey <- sum.ol["n.PLPL"]/(sum.ol["n.PLPL"]+sum.ol["n.PLSB"])
H.pl.ol.survey <-(sum.ol["n.PLSB"])/(sum.ol["n.PLPL"]+sum.ol["n.PLSB"])
# Proportions of SB Conspecific & Heterospecific matings at Ólafsdráttur
C.sb.ol.survey <- sum.ol["n.SBSB"]/(sum.ol["n.SBSB"]+sum.ol["n.SBPL"])
H.sb.ol.survey <- (sum.ol["n.SBPL"])/(sum.ol["n.SBSB"]+sum.ol["n.SBPL"])


########## Including female fecundity ######
mj2 <- na.omit(mj)
ol2 <- na.omit(ol)

# PL fecundity
fecPL.mj <- (2.08*(mean(mj2$lenght[mj2$morph == "PL"])*10)) - (18.6*6.3) - 66.11
fecPL.ol <- (2.08*(mean(ol2$lenght[ol2$morph == "PL"])*10)) - (18.6*6.3) - 66.11
#SB fecundity
fecSB.mj <- (1.38*(mean(mj2$lenght[mj2$morph == "SB"])*10)) - (4.75*5.1) - 57.91
fecSB.ol <- (1.38*(mean(ol2$lenght[ol2$morph == "SB"])*10)) - (4.75*5.1) - 57.91

# Proportions of PL Conspecific & Heterospecific matings at Ólafsdráttur
C.pl.ol.survey <- sum.ol["n.PLPL"]/(sum.ol["n.PLPL"]+sum.ol["n.PLSB"])
H.pl.ol.survey <-(sum.ol["n.PLSB"])/(sum.ol["n.PLPL"]+sum.ol["n.PLSB"])
# Proportions of SB Conspecific & Heterospecific matings at Ólafsdráttur
C.sb.ol.survey <- sum.ol["n.SBSB"]/(sum.ol["n.SBSB"]+sum.ol["n.SBPL"])
H.sb.ol.survey <- (sum.ol["n.SBPL"])/(sum.ol["n.SBSB"]+sum.ol["n.SBPL"])

### Adding fecundity

# PL matings at Mjóanes
C.pl.mjo.survey.f <- sum.mjo["n.PLPL"]/(sum.mjo["n.PLPL"]+sum.mjo["n.PLSB"])*fecPL.mj
H.pl.mjo.survey.f <-(sum.mjo["n.PLSB"])/(sum.mjo["n.PLPL"]+sum.mjo["n.PLSB"])*fecPL.mj

# Proportions of SB Conspecific & Heterospecific matings at at Mjóanes
C.sb.mjo.survey.f <- sum.mjo["n.SBSB"]/(sum.mjo["n.SBSB"]+sum.mjo["n.SBPL"])*fecSB.mj
H.sb.mjo.survey.f <- (sum.mjo["n.SBPL"])/(sum.mjo["n.SBSB"]+sum.mjo["n.SBPL"])*fecSB.mj


C.pl.ol.survey.f <- (sum.ol["n.PLPL"]/(sum.ol["n.PLPL"]+sum.ol["n.PLSB"]))*fecPL.ol 
H.pl.ol.survey.f <-((sum.ol["n.PLSB"])/(sum.ol["n.PLPL"]+sum.ol["n.PLSB"]))*fecPL.ol
# Proportions of SB Conspecific & Heterospecific matings at Ólafsdráttur
C.sb.ol.survey.f <- (sum.ol["n.SBSB"]/(sum.ol["n.SBSB"]+sum.ol["n.SBPL"]))*fecSB.ol 
H.sb.ol.survey.f <- ((sum.ol["n.SBPL"])/(sum.ol["n.SBSB"]+sum.ol["n.SBPL"]))*fecSB.ol

# Proportions both sites combined

# C.pl.tot.survey <- sum.prop["n.PLPL"]/(sum.prop["n.PLPL"]+sum.prop["n.PLSB"])
# H.pl.tot.survey <-(sum.prop["n.PLSB"])/(sum.prop["n.PLPL"]+sum.prop["n.PLSB"])
# C.sb.tot.survey <- sum.prop["n.SBSB"]/(sum.prop["n.SBSB"]+sum.prop["n.SBPL"])
# H.sb.tot.survey <- (sum.prop["n.SBPL"])/(sum.prop["n.SBSB"]+sum.prop["n.SBPL"])

############# RI formula ###############
RI <- function(C, H) {1 - 2*(H/(H+C))}

### Mjóanes RI estimates
mjo.ri.temp.pl <- RI(C = C.pl.mjo.survey.f, H = H.pl.mjo.survey.f) # PL

mjo.ri.temp.sb <- RI(C = C.sb.mjo.survey.f, H = H.sb.mjo.survey.f) # SB


# Ólafsdráttur RI estimates

ol.ri.temp.pl <-RI(C = C.pl.ol.survey.f, H = H.pl.ol.survey.f) # PL

ol.ri.temp.sb <-RI(C = C.sb.ol.survey.f, H = H.sb.ol.survey.f) # SB

# Total RI estimates
RI(C = C.pl.tot.survey, H = H.pl.tot.survey) # PL

RI(C = C.sb.tot.survey, H = H.sb.tot.survey) # SB

### Empirical estimates from Barchmann et al. (2021)

1-2*(9.5/(9.5+90.5))

