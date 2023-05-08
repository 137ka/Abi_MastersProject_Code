#GEOTRACES PLOT

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
BF <- read.csv(file="JC068_bottle_summary_20.csv",header =T)

#Create data frame of selected variables and ensure they are numeric
BFdata <- select(BF, YEAR, MONTH, DAY, LAT, LON, utemp, udepth, Peridinin, Fucoxanthin, Diadinoxanthin, but, hex, Zeaxanthin, DVCHLA, CHLA, Nitrate_Nit, Silicate, Phosphate, DFe,Zn)

#criteriacheck(BFdata)
Pig_df<-as.data.frame(BFdata)

#colnames(PE.df)<-c("REGION","LAT","LON","DEPTH","YEAR","MONTH","DAY","TEMP","NITRATE","SILICATE","PHOSPHATE","TCHL","ALPHA","PMB","PROV")
Pig_df<-subset(Pig_df,!(is.na(Pig_df["CHLA"])))

PERID_N <-Pig_df$Peridinin
BUTA_N <- Pig_df$but
FUCO_N <- Pig_df$Fucoxanthin
HEXA_N <- Pig_df$hex
ZEAX_N <- Pig_df$Zeaxanthin
DIAD_N <- Pig_df$Diadinoxanthin
DVCHLA_N <- Pig_df$DVCHLA

df2 <- cbind(PERID_N, BUTA_N, FUCO_N, HEXA_N, ZEAX_N, DIAD_N,DVCHLA_N,Pig_df$utemp,Pig_df$Nitrate_Nit, Pig_df$Silicate, Pig_df$Phosphate, Pig_df$DFe)
colnames(df2)<-c("PERID","BUT19","FUCO","HEX19","ZEAX","DIAD","DVCHLA","TEMP", "NO3_NO2", "Silicate", "Phosphate", "DFe")

df2<-subset(df2,!(is.na(Pig_df["Fucoxanthin"]) | is.na(Pig_df["Nitrate_Nit"])))

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
  ggtitle("JC068 Geotraces Correlation Heatmap")+
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
ggcorrplot(cormat2, outline.color = "white", lab = T)


setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Corr_Plot")

p.mat <- cor_pmat(cormat2)
P_DIMES <-as.data.frame(p.mat)
write.table(P_DIMES,"P_JC068.csv",col.names=T,row.names=T,sep=',')