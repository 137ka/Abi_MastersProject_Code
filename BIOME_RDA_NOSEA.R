#clear workspace
rm(list= ls(all= TRUE))

# install.packages("vegan")

library(ggplot2)
library(dplyr)
# Path to PE datasets
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Near_Neighbours")
library('vegan')

#read in biome datafiles
PoSp<-read.table(file="POLARSPR.csv",sep=",",h=T)
PoSu<-read.table(file="POLARSUM.csv",sep=",",h=T)
PoA<-read.table(file="POLARAUT.csv",sep=",",h=T)
WeSp<-read.table(file="WESTSPR.csv",sep=",",h=T)
WeSu<-read.table(file="WESTSUM.csv",sep=",",h=T)
WeA<-read.table(file="WESTAUT.csv",sep=",",h=T)
TrSp<-read.table(file="TRADESSPR.csv",sep=",",h=T)
TrSu<-read.table(file="TRADESSUM.csv",sep=",",h=T)
TrA<-read.table(file="TRADESAUT.csv",sep=",",h=T)
WEST<- rbind(WeSp,WeSu,WeA)
POLAR<- rbind(PoSp,PoSu,PoA)
TRADE<- rbind(TrSp,TrSu,TrA)
# SPRING <- rbind(PoSp,WeSp,TrSp)
# SUMMER <- rbind(PoSu,WeSu,TrSu)
# AUTUMN <- rbind(PoA,WeA,TrA)
# GLOBAL <- rbind(SPRING,SUMMER,AUTUMN)

# Code for multivariate statistical analyses

CB<-TRADE
C1<- CB_short<-cbind(1,CB$TEMP,CB$NO3,CB$DEPTH,CB$SALINITY,CB$Fe,CB$Mn,CB$Ni,CB$Cd,CB$Co,CB$Zn,CB$FUCO,CB$HEX,CB$BUT,CB$DVCHLA,CB$ZEA,CB$PERID,CB$DIAD)

CB<-WEST
C2<- CB_short<-cbind(2,CB$TEMP,CB$NO3,CB$DEPTH,CB$SALINITY,CB$Fe,CB$Mn,CB$Ni,CB$Cd,CB$Co,CB$Zn,CB$FUCO,CB$HEX,CB$BUT,CB$DVCHLA,CB$ZEA,CB$PERID,CB$DIAD)

CB<-POLAR
C3<- CB_short<-cbind(3,CB$TEMP,CB$NO3,CB$DEPTH,CB$SALINITY,CB$Fe,CB$Mn,CB$Ni,CB$Cd,CB$Co,CB$Zn,CB$FUCO,CB$HEX,CB$BUT,CB$DVCHLA,CB$ZEA,CB$PERID,CB$DIAD)


all_table <- rbind(C1,C2,C3)
all_data<-as.data.frame(all_table)

all_data<-all_data[rowSums(is.na(all_data)) < 1, ]
colnames(all_data)<-c("CLUSTER","TEMP","NO3","DEPTH","SALINITY","Fe","Mn","Ni","Cd","Co","Zn","FUCO","HEX","BUT","DVCHLA","ZEA","PERID","DIAD")

all_env<-all_data[,1:11]
all_pig<-all_data[,12:18]

pigment_rda<-rda(all_pig, all_env[,2:11])

set.seed(1)
(fit <- envfit(pigment_rda, all_env[,2:11], perm = 999))


screeplot(pigment_rda)
summary(pigment_rda)
(R2 <- RsquareAdj(pigment_rda)$r.squared)
(R2adj <- RsquareAdj(pigment_rda)$adj.r.squared)

levels(all_env$CLUSTER)<-c("Cluster1","Cluster2","Cluster3")
clust<-all_env$CLUSTER
bg <- c("red","#48BA4C","#377EB8")
plot(pigment_rda, type="n", xlim = c(-1,1), ylim = c(-1,1),
     main = "Biome Redundancy Analysis")
points(pigment_rda, pch=21, cex=1.1, col="gray32", bg=bg[all_env$CLUSTER])
plot(fit, add = TRUE, col = "black", cex=0.8)
legend(x= 'bottomright',cex = 0.75, legend= c("TRADES","WESTERLIES","POLAR"), fill= c("red","#48BA4C","#377EB8"))

# Total variance explained by RDA
RsquareAdj(pigment_rda)
# TEST OF SIGNIFICANCE
#Global RDA Significance
anova.cca(pigment_rda) # p=0.163 --> Insignificant

# Com Structure -----------------------------------------------------------

CB<-TRADE
CC<-WEST
CD<-POLAR

CBtot <- sum(CB$FUCO, na.rm = T)+sum(CB$HEX, na.rm = T)+sum(CB$BUT, na.rm = T)+sum(CB$DVCHLA, na.rm = T)+sum(CB$ZEA, na.rm = T)+sum(CB$PERID, na.rm = T)+sum(CB$DIAD, na.rm = T)
CCtot <- sum(CC$FUCO, na.rm = T)+sum(CC$HEX, na.rm = T)+sum(CC$BUT, na.rm = T)+sum(CC$DVCHLA, na.rm = T)+sum(CC$ZEA, na.rm = T)+sum(CC$PERID, na.rm = T)+sum(CC$DIAD, na.rm = T)
CDtot <- sum(CD$FUCO, na.rm = T)+sum(CD$HEX, na.rm = T)+sum(CD$BUT, na.rm = T)+sum(CD$DVCHLA, na.rm = T)+sum(CD$ZEA, na.rm = T)+sum(CD$PERID, na.rm = T)+sum(CD$DIAD, na.rm = T)

CBchart <- cbind((sum(CB$FUCO, na.rm = T)/CBtot),(sum(CB$ZEA, na.rm = T)/CBtot),(sum(CB$HEX, na.rm = T)/CBtot),(sum(CB$BUT, na.rm = T)/CBtot),(sum(CB$PERID, na.rm = T)/CBtot),(sum(CB$DIAD, na.rm = T)/CBtot), (sum(CB$DVCHLA, na.rm = T)/CBtot))
colnames(CBchart) <- c("FUCO","ZEA","HEX","BUT","PERID","DIAD","DVCHLA")
CCchart <- cbind((sum(CC$FUCO, na.rm = T)/CCtot),(sum(CC$ZEA, na.rm = T)/CCtot),(sum(CC$HEX, na.rm = T)/CCtot),(sum(CC$BUT, na.rm = T)/CCtot),(sum(CC$PERID, na.rm = T)/CCtot),(sum(CC$DIAD, na.rm = T)/CCtot), (sum(CC$DVCHLA, na.rm = T)/CCtot))
colnames(CCchart) <- c("FUCO","ZEA","HEX","BUT","PERID","DIAD","DVCHLA")
CDchart <- cbind((sum(CD$FUCO, na.rm = T)/CDtot),(sum(CD$ZEA, na.rm = T)/CDtot),(sum(CD$HEX, na.rm = T)/CDtot),(sum(CD$BUT, na.rm = T)/CDtot),(sum(CD$PERID, na.rm = T)/CDtot),(sum(CD$DIAD, na.rm = T)/CDtot), (sum(CD$DVCHLA, na.rm = T)/CDtot))
colnames(CDchart) <- c("FUCO","ZEA","HEX","BUT","PERID","DIAD","DVCHLA")

pie(CBchart, labels = c("Diatoms","Cyanobacteria","Haptophytes","Pelagophytes","Dinoflagellates","Diatoms/Haptos","Prochlorococcus"), col = rainbow(length(CBchart)), cex = 0.75)
pie(CCchart, labels = c("Diatoms","Cyanobacteria","Haptophytes","Pelagophytes","Dinoflagellates","Diatoms/Haptos","Prochlorococcus"), col = rainbow(length(CBchart)), cex = 0.75)
pie(CDchart, labels = c("Diatoms","Cyanobacteria","Haptophytes","Pelagophytes","Dinoflagellates","Diatoms/Haptos","Prochlorococcus"), col = rainbow(length(CBchart)), cex = 0.75)








