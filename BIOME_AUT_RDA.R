#clear workspace
rm(list= ls(all= TRUE))

# install.packages("vegan")

library(ggplot2)
library(dplyr)
# Path to PE datasets
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Geosphere")
library('vegan')

GLOBAL<-read.table(file="PIG_TM_all3.csv",sep=",",h=T)
ARCTIC<-GLOBAL[which(GLOBAL$DOMAIN==2)*1,]
SpARCTIC<-ARCTIC[which(ARCTIC$SEASON==3)*1,]

WEST<-GLOBAL[which(GLOBAL$DOMAIN==3)*1,]
SpWEST<-WEST[which(WEST$SEASON==3)*1,]

TRADE<-GLOBAL[which(GLOBAL$DOMAIN==4)*1,]
SpTRADE<-TRADE[which(TRADE$SEASON==3)*1,]

ANTARCTIC<-GLOBAL[which(GLOBAL$DOMAIN==5)*1,]
SpANT<-ANTARCTIC[which(ANTARCTIC$SEASON==3)*1,]

# Code for multivariate statistical analyses

CB<- SpARCTIC
C1<- CB_short<-cbind(1,CB$TEMP,CB$NO3,CB$Fe,CB$Mn,CB$Zn,CB$Ni,CB$Cu,CB$Co,CB$Cd,CB$FUCO,CB$X19_HEX,CB$X19_BUT,CB$DVCHLA,CB$ZEA,CB$PERID,CB$DIADINO)

CB<- SpWEST
C2<- CB_short<-cbind(2,CB$TEMP,CB$NO3,CB$Fe,CB$Mn,CB$Zn,CB$Ni,CB$Cu,CB$Co,CB$Cd,CB$FUCO,CB$X19_HEX,CB$X19_BUT,CB$DVCHLA,CB$ZEA,CB$PERID,CB$DIADINO)

CB<-SpTRADE
C3<- CB_short<-cbind(3,CB$TEMP,CB$NO3,CB$Fe,CB$Mn,CB$Zn,CB$Ni,CB$Cu,CB$Co,CB$Cd,CB$FUCO,CB$X19_HEX,CB$X19_BUT,CB$DVCHLA,CB$ZEA,CB$PERID,CB$DIADINO)

CB<-SpANT
C4<- CB_short<-cbind(4,CB$TEMP,CB$NO3,CB$Fe,CB$Mn,CB$Zn,CB$Ni,CB$Cu,CB$Co,CB$Cd,CB$FUCO,CB$X19_HEX,CB$X19_BUT,CB$DVCHLA,CB$ZEA,CB$PERID,CB$DIADINO)


all_table<-rbind(C1,C2,C3,C4)
all_data<-as.data.frame(all_table)

all_data<-all_data[rowSums(is.na(all_data)) < 1, ]
colnames(all_data)<-c("CLUSTER","TEMP","NO3","Fe","Mn","Zn","Ni","Cu","Co","Cd","FUCO","HEX","BUT","DVCHLA","ZEA","PERID", "DIAD")

all_env<-all_data[,1:10]
all_pig<-all_data[,11:17]

pigment_rda<-rda(all_pig, all_env[,2:10])

set.seed(1)
(fit <- envfit(pigment_rda, all_env[,2:10], perm = 999))


screeplot(pigment_rda)
summary(pigment_rda)
(R2 <- RsquareAdj(pigment_rda)$r.squared)
(R2adj <- RsquareAdj(pigment_rda)$adj.r.squared)

levels(all_env$CLUSTER)<-c("Cluster1","Cluster2","Cluster3","Cluster4")
clust<-all_env$CLUSTER
bg <- c("#FFAB00","#377EB8","#48BA4C","#D72CF9")
plot(pigment_rda, type="n", xlim = c(-2,2), ylim = c(-2,2),
     main = "Biome RDA Analysis Autumn")
plot(fit, add = TRUE, col = "black", cex = 0.8)
points(pigment_rda, pch=21, cex=1, col="gray32", bg=bg[all_env$CLUSTER])
legend(x= 'topright',cex = 0.75, legend= c("ARCTIC","WESTERLIES","TRADES","ANTARCTIC"), fill= c("#FFAB00","#377EB8","#48BA4C","#D72CF9"))

# Total variance explained by RDA
RsquareAdj(pigment_rda)
# TEST OF SIGNIFICANCE
#Global RDA Significance
anova.cca(pigment_rda) # p=0.001 --> significant


# Community Structure -----------------------------------------------------

CB<-read.table(file="POLARAUT.csv",sep=",",h=T)
CC<-read.table(file="WESTAUT.csv",sep=",",h=T)
CD<-read.table(file="TRADESAUT.csv",sep=",",h=T)

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













