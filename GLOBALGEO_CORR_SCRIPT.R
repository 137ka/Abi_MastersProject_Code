#DIMES Cruise Plots

library(tidyverse)
library(reshape2)

library(plotly)
library(ggcorrplot)

#correlation heat map attempt (AAIW factors)

#clear workspace
rm(list=ls(all=TRUE))

#Set path to datasets
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Geosphere")

#Read dataset
GLOBAL<-read.table(file="PIG_TM_all3.csv",sep=",",h=T)

#Create data frame of selected variables and ensure they are numeric
BFdata <- select(GLOBAL, YEAR, MONTH, DAY, lon, lat, TEMP, DEPTH, PERID, FUCO, X19_BUT, X19_HEX, ZEA, DVCHLA, CHLA,DIADINO,NO3,SILICATE,PHOS,Cd,Fe,Ni,Cu,Zn,Mn,Co)

#criteriacheck(BFdata)
Pig_df<-as.data.frame(BFdata)

PERID_N <-Pig_df$PERID
BUTA_N <- Pig_df$X19_BUT
FUCO_N <- Pig_df$FUCO
HEXA_N <- Pig_df$X19_HEX
ZEAX_N <- Pig_df$ZEA
DIAD_N <- Pig_df$DIAD

df2 <- cbind(PERID_N, BUTA_N, FUCO_N, HEXA_N, ZEAX_N,DIAD_N,Pig_df$DVCHLA,Pig_df$TEMP,Pig_df$NO3,Pig_df$Fe,Pig_df$Mn,Pig_df$Ni,Pig_df$Cu,Pig_df$Zn,Pig_df$Cd,Pig_df$Co)
colnames(df2)<-c("PERID","BUT19","FUCO","HEX19","ZEAX","DIAD","DVCHLA","TEMP","NO3","Fe","Mn","Ni","Cu","Zn","Cd","Co")

cormat2 <- round(cor(df2, use = "complete.obs"),2)
head(cormat2)

melted_cormat2 <- melt(cormat2)


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat2){
  cormat[upper.tri(cormat2)] <- NA
  return(cormat2)}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat2){
  cormat[lower.tri(cormat2)]<- NA
  return(cormat2)}



ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()+
  scale_color_continuous(name="Correlation")+
  ggtitle("JC069 Cruise Correlation heatmap")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()+


  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)

setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Corr_Plot")

p.mat <- cor_pmat(cormat2)
P_DIMES <-as.data.frame(p.mat)
write.table(P_DIMES,"P_DIMES.csv",col.names=T,row.names=T,sep=',')
#ggcorrplot(cormat2, outline.color = "white", insig = "blank",lab=T)
ggcorrplot(cormat2, outline.color = "white", p.mat = p.mat)
ggcorrplot(cormat2, outline.color = "white", lab = T)

