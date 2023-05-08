#KRAMER-SIEGEL PLOT

library(tidyverse)
library(reshape2)

library(plotly)
library(ggcorrplot)

#correlation heat map attempt (AAIW factors)

#clear workspace
rm(list=ls(all=TRUE))

#Set path to datasets
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/")

#Read dataset
BF <- read.csv(file="Kramer-Siegel3.csv",header =T)

#Create data frame of selected variables and ensure they are numeric
BFdata <- select(BF, YEAR, MONTH, DAY, LAT, LON, TEMP, MIN_DEPTH, PERID, FUCO, ALLO, BUTFUCO, HEXFUCO, ZEA, DVCHLB, CHLB, DVCHLA, CHLA, NO3)

#criteriacheck(BFdata)
Pig_df<-as.data.frame(BFdata)

#colnames(PE.df)<-c("REGION","LAT","LON","DEPTH","YEAR","MONTH","DAY","TEMP","NITRATE","SILICATE","PHOSPHATE","TCHL","ALPHA","PMB","PROV")
Pig_df<-subset(Pig_df,!(is.na(Pig_df["CHLA"])))
Pig_df<-subset(Pig_df,!(is.na(is.na(Pig_df["FUCO"]) | is.na(Pig_df["PERID"] |is.na(Pig_df["ZEA"] | is.na(Pig_df["CHLA"] | is.na(Pig_df["HEXFUCO"])))))))


PERID_N <-Pig_df$PERID/Pig_df$CHLA
BUTA_N <- Pig_df$BUTFUCO/Pig_df$CHLA
FUCO_N <- Pig_df$FUCO/Pig_df$CHLA
HEXA_N <- Pig_df$HEXFUCO/Pig_df$CHLA
ZEAX_N <- Pig_df$ZEA/Pig_df$CHLA
CHLB_N <- Pig_df$CHLB/Pig_df$CHLA
DVCHLA_N <- Pig_df$DVCHLA/Pig_df$CHLA


df <- cbind(PERID_N, BUTA_N, FUCO_N, HEXA_N, ZEAX_N, CHLB_N, DVCHLA_N)

cormat <- round(cor(df, use = "complete.obs"),2)
head(cormat)

melted_cormat <- melt(cormat)


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)}

# lower_tri <- get_lower_tri(cormat)
#
#
# melted_cormat <- melt(lower_tri, na.rm = TRUE)


ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()+
  scale_color_continuous(name="Correlation")+
  ggtitle("Kramer-Siegel Correlation heatmap")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()+
  
  
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)
#***************************************************************************************************************************************************************

#Read dataset
BF <- read.csv(file="Kramer-Siegel3.csv",header =T)

#Create data frame of selected variables and ensure they are numeric
BFdata <- select(BF, YEAR, MONTH, DAY, LAT, LON, TEMP, MIN_DEPTH, PERID, FUCO, ALLO, BUTFUCO, HEXFUCO, ZEA, DVCHLB, DIADINO, DVCHLA, CHLA, NO3)

#criteriacheck(BFdata)
Pig_df<-as.data.frame(BFdata)

#colnames(PE.df)<-c("REGION","LAT","LON","DEPTH","YEAR","MONTH","DAY","TEMP","NITRATE","SILICATE","PHOSPHATE","TCHL","ALPHA","PMB","PROV")
Pig_df<-subset(Pig_df,!(is.na(Pig_df["CHLA"])))

PERID_N <-Pig_df$PERID
BUTA_N <- Pig_df$BUTFUCO
FUCO_N <- Pig_df$FUCO
HEXA_N <- Pig_df$HEXFUCO
ZEAX_N <- Pig_df$ZEA
DIAD_N <- Pig_df$DIADINO
DVCHLA_N <- Pig_df$DVCHLA

df2 <- cbind(PERID_N, BUTA_N, FUCO_N, HEXA_N, ZEAX_N, DIAD_N,DVCHLA_N,Pig_df$CHLA,Pig_df$TEMP,Pig_df$NO3)
colnames(df2)<-c("PERID","BUT19","FUCO","HEX19","ZEAX","DIAD","DVCHLA","CHLA","TEMP","NO3")

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


 ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) +
   geom_tile()+
   scale_color_continuous(name="Correlation")+
   ggtitle("Kramer-Siegel Correlation Heatmap")+
   geom_tile(color = "white")+
   scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                        midpoint = 0, limit = c(-1,1), space = "Lab",
                        name="Pearson\nCorrelation") +
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                    size = 12, hjust = 1))+
   coord_fixed()+
   geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)

p.mat <- cor_pmat(cormat2)
ggcorrplot(cormat2, outline.color = "white", p.mat = p.mat)
#ggcorrplot(cormat2, outline.color = "white", lab = T, insig = "blank")


setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Corr_Plot")

p.mat <- cor_pmat(cormat2)
P_DIMES <-as.data.frame(p.mat)
write.table(P_DIMES,"P_KS.csv",col.names=T,row.names=T,sep=',')


