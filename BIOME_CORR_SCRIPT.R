#Near-Neighbours Cruise Plots

library(tidyverse)
library(reshape2)

library(plotly)
library(ggcorrplot)

#correlation heat map attempt (AAIW factors)

#clear workspace
rm(list=ls(all=TRUE))

#Set path to datasets
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Near_Neighbours")

#Read dataset
PoSp<-read.table(file="POLARSPR.csv",sep=",",h=T)
PoSu<-read.table(file="POLARSUM.csv",sep=",",h=T)
PoA<-read.table(file="POLARAUT.csv",sep=",",h=T)
WeSp<-read.table(file="WESTSPR.csv",sep=",",h=T)
WeSu<-read.table(file="WESTSUM.csv",sep=",",h=T)
WeA<-read.table(file="WESTAUT.csv",sep=",",h=T)
TrSp<-read.table(file="TRADESSPR.csv",sep=",",h=T)
TrSu<-read.table(file="TRADESSUM.csv",sep=",",h=T)
TrA<-read.table(file="TRADESAUT.csv",sep=",",h=T)
SPRING <- rbind(PoSp,WeSp,TrSp)
SUMMER <- rbind(PoSu,WeSu,TrSu)
AUTUMN <- rbind(PoA,WeA,TrA)
GLOBAL <- rbind(SPRING,SUMMER,AUTUMN)


#Create data frame of selected variables and ensure they are numeric
BFdata <- select(SUMMER, YEAR, BIO_MONTH, BIO_DAY, PIG_LAT, PIG_LON, TM_LAT, TM_LON, TEMP, DEPTH, PERID, FUCO, BUT, HEX, ZEA, DVCHLA, CHLA, NO3,Cd,Fe,Ni,Cu,Zn,Mn,Co)

#criteriacheck(BFdata)
Pig_df<-as.data.frame(BFdata)

#colnames(PE.df)<-c("REGION","LAT","LON","DEPTH","YEAR","MONTH","DAY","TEMP","NITRATE","SILICATE","PHOSPHATE","TCHL","ALPHA","PMB","PROV")
Pig_df<-subset(Pig_df,!(is.na(Pig_df["CHLA"])))

PERID_N <-Pig_df$PERID
BUTA_N <- Pig_df$BUT
FUCO_N <- Pig_df$FUCO
HEXA_N <- Pig_df$HEX
ZEAX_N <- Pig_df$ZEA
DVCHLA_N <- Pig_df$DVCHLA

df2 <- cbind(PERID_N, BUTA_N, FUCO_N, HEXA_N, ZEAX_N, DVCHLA_N,Pig_df$TEMP,Pig_df$NO3,Pig_df$Fe,Pig_df$Mn)
colnames(df2)<-c("PERID","BUT19","FUCO","HEX19","ZEAX","DVCHLA","TEMP","NO3","Fe","Mn")

df2<-subset(df2,!(is.na(Pig_df["FUCO"]) | is.na(Pig_df["NO3"])))

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

# lower_tri <- get_lower_tri(cormat)
#
#
# melted_cormat <- melt(lower_tri, na.rm = TRUE)

# 
# ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) +
#   geom_tile()+
#   scale_color_continuous(name="Correlation")+
#   ggtitle("DIMES Cruise Correlation heatmap")+
#   geom_tile(color = "white")+
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white",
#                        midpoint = 0, limit = c(-1,1), space = "Lab",
#                        name="Pearson\nCorrelation") +
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1,
#                                    size = 12, hjust = 1))+
#   coord_fixed()+
#   
#   
#   geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)

setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Corr_Plot")

p.mat <- cor_pmat(cormat2)
P_NNSPRING <-as.data.frame(p.mat)
write.table(P_NNSPRING,"P_NNSPRING.csv",col.names=T,row.names=T,sep=',')
ggcorrplot(cormat2, outline.color = "white", insig = "blank",lab=T)
ggcorrplot(cormat2, outline.color = "white", p.mat = p.mat)
ggcorrplot(cormat2, outline.color = "white", lab = T)

